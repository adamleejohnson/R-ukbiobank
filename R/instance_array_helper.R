#' @noRd
expand_helper_ <- function(data,
                           field_name,
                           ctrl_ind,
                           min_instance = 0,
                           max_instance = Inf,
                           min_array = 0,
                           max_array = Inf) {
  field_name <- enquo(field_name)
  input_is_symbol <- quo_is_symbol(field_name)
  field_name <- as.character(quo_get_expr(field_name))

  # define the regex extractor
  # the ctrl_ind indices correspond to the matching groups to be extracted
  field_re <- "(.*[^\\d])?(\\d+)(\\.)(\\d+)(\\.)(\\d+)([^\\d].*)?"

  # ensure field name has correct format
  stopifnot("Column name does not contain Field.Instance.Array" = stringr::str_detect(field_name, field_re))

  # Build regex expression to test against column names
  stopifnot("ctrl_ind must be an integer or sequence of consecutive integers" = setequal(ctrl_ind, min(ctrl_ind):max(ctrl_ind)))
  field_matcher <- stringr::str_match(field_name, field_re)[1, -1]
  pt1 <- quotemeta(paste(field_matcher[1:(min(ctrl_ind) - 1)], collapse = ""))
  pt2 <- quotemeta(paste(field_matcher[ctrl_ind], collapse = "")) %>% stringr::str_replace_all("\\d+", "(\\\\d+)")
  pt3 <- quotemeta(paste(field_matcher[-(1:max(ctrl_ind))], collapse = ""))
  field_matcher <- paste0(pt1, pt2, pt3)

  # find matched columns
  matched_fields <- colnames(data)[stringr::str_detect(colnames(data), field_matcher)]

  # apply MIN and MAX instance filters if expanding by instance
  if (4 %in% ctrl_ind) {
    instance_nums <- stringr::str_match(matched_fields, field_re)[, 4 + 1] %>%
      as.numeric()
    keep_ind <- seq_along(instance_nums) %>%
      intersect(which(instance_nums >= min_instance)) %>%
      intersect(which(instance_nums <= max_instance))
    matched_fields <- matched_fields[keep_ind]
  }

  # apply MIN and MAX instance filters if expanding by array
  if (6 %in% ctrl_ind) {
    array_nums <- stringr::str_match(matched_fields, field_re)[, 6 + 1] %>%
      as.numeric()
    keep_ind <- intersect(
      which(array_nums >= min_array),
      which(array_nums <= max_array)
    )
    matched_fields <- matched_fields[keep_ind]
  }

  # return result
  if (input_is_symbol) {
    sapply(matched_fields, as.symbol, USE.NAMES = F)
  } else {
    matched_fields
  }
}

#' @noRd
expand_instances <- function(data, field_name, ...) {
  expand_helper_(data, {{ field_name }}, 4, ...)
}

#' @noRd
expand_array <- function(data, field_name, ...) {
  expand_helper_(data, {{ field_name }}, 6, ...)
}

#' @noRd
expand_instances_and_array <- function(data, field_name, ...) {
  expand_helper_(data, {{ field_name }}, 4:6, ...)
}

#' @noRd
select_instance_and_expand_array <- function(data, field_name, instance, ...) {
  expand_helper_(data, {{ field_name }}, 4:6,
    min_instance = instance,
    max_instance = instance,
    ...
  )
}

#' @noRd
select_instance_and_array <- function(data, field_name, instance, array) {
  expand_helper_(data, {{ field_name }}, 4:6,
    min_instance = instance,
    max_instance = instance,
    min_array = array,
    max_array = array
  )
}

#' Helper to escape characters when building regex strings
#' @noRd
quotemeta <- function(string) {
  stringr::str_replace_all(string, "(\\W)", "\\\\\\1")
}


#' Helper to apply a function up to a certain instance
#'
#' @inheritParams ukbiobank
#' @param lookup_by_instance_fn Function that takes a target instance as its only argument, and returns a vector of data.
up_to_instance_combiner <- function(data,
                                    lookup_by_instance_fn,
                                    combine_instances = c("any", "max", "min", "first", "last", "mean"),
                                    up_to_instance = 3) {

  # print message about calling functions
  fn1 <- sys.calls()[[sys.nframe() - 2]][[1]]
  fn2 <- sys.calls()[[sys.nframe() - 1]][[1]]
  message("\U25A0 ", fn1, " \U2192 ", fn2)

  # get max instance numbers
  expr_class <- class(quo_get_expr(enquo(up_to_instance)))
  if (!quo_is_symbol(enquo(up_to_instance)) && is.numeric(up_to_instance)) {
    # if the up_to_instance is just a number, use it
    max_inst_name <- up_to_instance
    max_inst_overall <- up_to_instance
  } else if (quo_is_symbol(enquo(up_to_instance))) {
    # if the up_to_instance is a symbol, use it as a conditional column name
    max_inst_overall <-
      data %>%
      pull({{ up_to_instance }}) %>%
      as.numeric() %>%
      max()
    max_inst_name <- paste0("col `", quo_text(enquo(up_to_instance)), "` (overall_max = ", max_inst_overall, ")")
  } else {
    stop("Invalid format for `up_to_instance`")
  }

  # populate results for each instance
  message("   \U251C Using max instance: ", max_inst_name)
  message("   \U251C Populating results for instance ", appendLF = F)
  results <- as.list(rep(NA, 4))
  for (inst in 0:max_inst_overall) {
    message(inst, appendLF = F)
    if (inst < max_inst_overall) message("...", appendLF = F)
    results[[inst + 1]] <- lookup_by_instance_fn(inst)
  }
  message(" \U2713")

  # conditionally combine the results using the provided combining function
  message("   \U2514 Compiling results...", appendLF = F)
  combine_instances <- match.arg(combine_instances)
  combine_fxn <- get_combiner_fn(combine_instances)
  res <- data %>%
    mutate(
      case_when(
        {{ up_to_instance }} == 0 ~ !!results[[1]],
        {{ up_to_instance }} == 1 ~ !!Reduce(combine_fxn, results[1:2]),
        {{ up_to_instance }} == 2 ~ !!Reduce(combine_fxn, results[1:3]),
        {{ up_to_instance }} == 3 ~ !!Reduce(combine_fxn, results[1:4]),
      )
    ) %>%
    pull()
  message("done \U2713")
  return(res)
}

#' Get Combiner Function
#'
#' Helper to define a reduction function for combining instances and arrays. Returns a binary function based on the specified options. Typically used by [up_to_instance_combiner()] and [reduce_by_row()].
#'
#' @inheritParams ukbiobank
get_combiner_fn <- function(combine_instances = c("any", "max", "min", "first", "last", "mean")) {
  combine_instances <- match.arg(combine_instances)
  switch(combine_instances,
    "any" = function(x, y) {
      stopifnot(class(x) == "logical" && class(y) == "logical")
      x | y
    },
    "max" = function(x, y) {
      pmax(x, y, na.rm = T)
    },
    "min" = function(x, y) {
      pmin(x, y, na.rm = T)
    },
    "first" = function(x, y) {
      if (length(x) == 1) x <- rep_along(y, x)
      ifelse(is.na(x), y, x)
    },
    "last" = function(x, y) {
      if (length(y) == 1) y <- rep_along(x, y)
      ifelse(is.na(y), x, y)
    },
    "mean" = function(x, y) {
      m <- rowMeans(cbind(x, y), na.rm = T)
      m[is.nan(m)] <- NA
      m
    }
  )
}
