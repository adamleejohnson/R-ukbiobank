#' @noRd
expand_instances <- function(field_name, indices = 0:3) {
  field_name <- enquo(field_name)
  field_name <- as.character(quo_get_expr(field_name))
  re <- "(.*\\d+\\.)\\d(\\.\\d\\..*)"
  stopifnot("Column name does not contain Field.Instance.Array" = str_detect(field_name, re))
  result <- str_replace(field_name, re, paste0("\\1", indices, "\\2"))
  sapply(result, as.symbol, USE.NAMES = F)
}

#' @noRd
expand_array <- function(data, field_name) {
  field_name <- enquo(field_name)
  field_name <- as.character(quo_get_expr(field_name))
  re <- "(.*[^\\d])?(\\d+)(\\.)(\\d)(\\.)(\\d)([^\\d].*)?"
  stopifnot("Column name does not contain Field.Instance.Array" = str_detect(field_name, re))

  # Build regex expression to test against column names
  field_matcher <- str_match(field_name, re)[1,-1]
  field_matcher <- paste0(c(quotemeta(field_matcher[1:5]), "(\\d+)", quotemeta(field_matcher[7])), collapse = "")
  matched_fields <- colnames(data)[str_detect(colnames(data), field_matcher)]

  # extract array numbers from all the matched fields and check that all the array indices are present
  indices <- as.integer(str_match(matched_fields, field_matcher)[,-1])
  stopifnot("Array indices are missing" = setequal(indices, min(indices):max(indices)))

  # return result
  sapply(matched_fields, as.symbol, USE.NAMES = F)
}

#' Helper to escape characters when building regex strings
#' @noRd
quotemeta <- function(string) {
  str_replace_all(string, "(\\W)", "\\\\\\1")
}
