#' @noRd
.expand_helper <- function(data, field_name, ctrl_ind) {
  field_name <- enquo(field_name)
  input_is_symbol <- quo_is_symbol(field_name)
  field_name <- as.character(quo_get_expr(field_name))

  # define the regex extractor
  # the ctrl_ind indices correspond to the matching groups to be extracted
  field_re <- "(.*[^\\d])?(\\d+)(\\.)(\\d)(\\.)(\\d)([^\\d].*)?"

  # ensure field name has correct format
  stopifnot("Column name does not contain Field.Instance.Array" = stringr::str_detect(field_name, field_re))

  # Build regex expression toS test against column names
  stopifnot("ctrl_ind must be an integer or sequence of consecutive integers" = setequal(ctrl_ind, min(ctrl_ind):max(ctrl_ind)))
  field_matcher <- stringr::str_match(field_name, field_re)[1,-1]
  pt1 <- quotemeta(paste(field_matcher[1:(min(ctrl_ind) - 1)], collapse = ""))
  pt2 <- quotemeta(paste(field_matcher[ctrl_ind], collapse = "")) %>% stringr::str_replace_all("\\d+", "(\\\\d+)")
  pt3 <- quotemeta(paste(field_matcher[-(1:max(ctrl_ind))], collapse = ""))
  field_matcher <- paste0(pt1, pt2, pt3)

  # find matched columns
  matched_fields <- colnames(data)[stringr::str_detect(colnames(data), field_matcher)]

  # return result
  if (input_is_symbol)
    sapply(matched_fields, as.symbol, USE.NAMES = F)
  else
    matched_fields
}

#' @noRd
expand_instances <- function(data, field_name) {
  .expand_helper(data, {{field_name}}, 4)
}

#' @noRd
expand_array <- function(data, field_name) {
  .expand_helper(data, {{field_name}}, 6)
}

#' @noRd
expand_instance_and_array <- function(data, field_name) {
  .expand_helper(data, {{field_name}}, 4:6)
}

#' Helper to escape characters when building regex strings
#' @noRd
quotemeta <- function(string) {
  stringr::str_replace_all(string, "(\\W)", "\\\\\\1")
}
