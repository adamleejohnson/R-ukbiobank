#' @noRd
column_expansion_helper <- function(.data,
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
  matched_cols <- colnames(.data)[stringr::str_detect(colnames(.data), field_matcher)]
  stopifnot(
    "There are no columns in the dataframe that match the template column name" = length(matched_cols) > 0
  )
  instance_nums <- stringr::str_match(matched_cols, field_re)[, 4 + 1] %>% as.numeric()
  array_nums <- stringr::str_match(matched_cols, field_re)[, 6 + 1] %>% as.numeric()

  # sort by array & instance
  arr_order <- order(array_nums)
  inst_order <- order(instance_nums[arr_order])
  matched_cols <- matched_cols[arr_order]
  matched_cols <- matched_cols[inst_order]

  # apply MIN and MAX instance filters if expanding by instance
  if (4 %in% ctrl_ind) {
    instance_nums <- stringr::str_match(matched_cols, field_re)[, 4 + 1] %>% as.numeric()
    keep_ind <- seq_along(instance_nums) %>%
      intersect(which(instance_nums >= min_instance)) %>%
      intersect(which(instance_nums <= max_instance))
    matched_cols <- matched_cols[keep_ind]
  }

  # apply MIN and MAX instance filters if expanding by array
  if (6 %in% ctrl_ind) {
    array_nums <- stringr::str_match(matched_cols, field_re)[, 6 + 1] %>% as.numeric()
    keep_ind <- intersect(
      which(array_nums >= min_array),
      which(array_nums <= max_array)
    )
    matched_cols <- matched_cols[keep_ind]
  }

  # return result
  if (input_is_symbol) {
    sapply(matched_cols, as.symbol, USE.NAMES = F)
  } else {
    matched_cols
  }
}

#' @noRd
expand_instances <- function(.data, field_name, ...) {
  column_expansion_helper(.data, {{ field_name }}, 4, ...)
}

#' @noRd
expand_array <- function(.data, field_name, ...) {
  column_expansion_helper(.data, {{ field_name }}, 6, ...)
}

#' @noRd
expand_instances_and_array <- function(.data, field_name, ...) {
  column_expansion_helper(.data, {{ field_name }}, 4:6, ...)
}

#' @noRd
select_instance_and_expand_array <- function(.data, field_name, instance, ...) {
  column_expansion_helper(.data, {{ field_name }}, 4:6,
    min_instance = instance,
    max_instance = instance,
    ...
  )
}

#' @noRd
select_instance_and_array <- function(.data, field_name, instance, array) {
  column_expansion_helper(.data, {{ field_name }}, 4:6,
    min_instance = instance,
    max_instance = instance,
    min_array = array,
    max_array = array
  )
}


#' @title Instance combiner helper function
#' @description Helper to apply a lookup function to a range of instances, and combine the result.
#' @inheritParams ukbiobank
#' @param lookup_by_instance_fn Function that takes a target instance as its only argument, and returns a vector of data.
#' @keywords internal
instance_combiner <- function(.data,
                              lookup_by_instance_fn,
                              combine_instances = c("any", "max", "min", "first", "last", "mean"),
                              up_to_instance = default_up_to_inst(),
                              after_instance = default_after_inst()) {

  # print message about calling functions
  fn1 <- tryCatch(sys.calls()[[sys.nframe() - 2]][[1]], error = function(e) "null")
  fn2 <- tryCatch(sys.calls()[[sys.nframe() - 1]][[1]], error = function(e) "null")
  message("\U25A0 ", fn1, " \U2192 ", fn2)

  # get the instance numbers and the min/max overall
  # IMPORTANT: after_instance is NOT inclusive of the starting instance, but min_instances IS inclusive, hence the +1
  min_instances <- .data %>%
    mutate(pmax(as.numeric({{ after_instance }}) + 1, MINIMUM_INSTANCE_NUM)) %>%
    pull()
  max_instances <- .data %>%
    mutate(pmin(as.numeric({{ up_to_instance }}), MAXIMUM_INSTANCE_NUM)) %>%
    pull()
  min_inst_overall <- min(min_instances)
  max_inst_overall <- max(max_instances)

  # populate results for each instance
  # ... create a.data frame to hold the results for each instances that we calculate
  stopifnot("** Minimum start instance is greater than the maximum end instance" = min_inst_overall <= max_inst_overall)
  { # message
    min_inst_text <- wrap_str(quo_text(enquo(after_instance)), "`", condition = !quo_is_numeric(enquo(after_instance)))
    max_inst_text <- wrap_str(quo_text(enquo(up_to_instance)), "`", condition = !quo_is_numeric(enquo(up_to_instance)))
    message(
      "   \U251C Using instance range = (",
      min_inst_text, ":", max_inst_text, "] \U2192 Overall range, inclusive = [",
      min_inst_overall, ":", max_inst_overall, "]"
    )
    message("   \U251C Populating results for instance ", appendLF = F)
  }
  populate_inst <- min_inst_overall:max_inst_overall
  results_by_inst <- list()
  for (i in populate_inst) {
    i_idx <- which(i == populate_inst)
    message(i, if (i_idx < length(populate_inst)) "..." else "", appendLF = F)
    lookup_result <- lookup_by_instance_fn(i)
    results_by_inst[[i_idx]] <- lookup_result
  }
  message(" \U2713")

  # conditionally combine the results using the provided combining function
  message("   \U2514 Compiling results...", appendLF = F)
  reduce_fn <- get_reduce_fn(combine_instances)

  # ... create a call to case_when using all possible combinations of start and end
  casewhen_call <- "case_when("
  for (i in unique(min_instances)) {
    for (j in unique(max_instances)) {
      if (i > j) next
      casewhen_call <- paste0(casewhen_call, "min_instances == ", i, " & max_instances == ", j)
      min_idx <- which(i == populate_inst)
      max_idx <- which(j == populate_inst)
      if (min_idx == max_idx) {
        casewhen_call <- paste0(casewhen_call, " ~ results_by_inst[[", min_idx, "]], ")
      } else {
        casewhen_call <- paste0(casewhen_call, " ~ Reduce(reduce_fn, results_by_inst[", min_idx, ":", max_idx, "]), ")
      }
    }
  }
  casewhen_call <- paste0(casewhen_call, ")")
  casewhen_call <- str2lang(casewhen_call)
  output <- eval(casewhen_call)

  # done
  message("done \U2713")
  return(output)
}

#' @title Get Combiner Function
#' @description Helper to define a reduction function for combining instances and arrays. Returns a binary function based on the specified options. Typically used by [instance_combiner()] and [reduce_by_row()].
#' @inheritParams ukbiobank
#' @keywords internal
get_reduce_fn <- function(combine_instances = c("any", "max", "min", "first", "last", "mean")) {
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
