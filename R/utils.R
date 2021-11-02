"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

#' Vectorized OR that prefers non-NA values
#' @noRd
"%|%" <- function(a, b) {
  a <- a %||% NA
  b <- b %||% NA
  ifelse(is.na(a),
    b,
    ifelse(is.na(b),
      a,
      a | b
    )
  )
}

#' Wrap a string in a character, if condition is true
#' @noRd
wrap_str <- function(input, char, condition = TRUE) {
  if (condition) {
    paste0(char, input, char)
  } else {
    input
  }
}

#' Is the expression in a quosure numeric?
#' @noRd
quo_is_numeric <- function(x) !is.na(suppressWarnings(as.numeric(quo_text(x))))

#' Helper to escape characters when building regex strings
#' @noRd
quotemeta <- function(string) {
  stringr::str_replace_all(string, "(\\W)", "\\\\\\1")
}

#' Remove columns with only NAs
#' @noRd
remove_na_columns <- function(data) data[colSums(!is.na(data)) > 0]

#' @title Apply functions across rows
#' @description
#' * `any_by_row`:  For each row, are any of the columns true (see [any()])
#' * `fn_by_row`:  Apply function across each entire row (function takes whole row as argument)
#' * `reduce_by_row`:  Apply [Reduce()] across each row of matrix or dataframe
#' @param data input data
#' @param fn function to apply
#' @keywords internal
any_by_row <- function(data) apply(as.matrix(data), 1, any)

#' @rdname any_by_row
fn_by_row <- function(data, fn) apply(as.matrix(data), 1, fn)

#' @rdname any_by_row
reduce_by_row <- function(data, fn) Reduce(fn, as.data.frame(data))

#' Forward all arguments of the calling function to the specified function.
#' Works as expected with formals.
#' @noRd
forward_args_to_function <- function(fn) {
  fn <- substitute(fn)
  origin_fn_formals <- formals(fun = sys.function(sys.parent()), envir = parent.frame())
  origin_fn_call <- match.call(
    definition = sys.function(sys.parent()),
    call = sys.call(sys.parent()),
    expand.dots = TRUE,
    envir = parent.frame(2L)
  )
  origin_fn_call[[1]] <- as.symbol(fn)
  for (n in names(origin_fn_formals)) {
    if (!(n %in% names(origin_fn_call))) origin_fn_call[[n]] <- origin_fn_formals[[n]]
  }
  eval(origin_fn_call)
}

#' @noRd
default_after_inst <- function() {
  return(MINIMUM_INSTANCE_NUM - 1)
}

#' @noRd
default_up_to_inst <- function() {
  return(MAXIMUM_INSTANCE_NUM)
}
