#' @noRd
expand_instances <- function(field_name, indices = 0:3) {
  field_name <- enquo(field_name)
  field_name <- as.character(quo_get_expr(field_name))
  re <- "(f\\.\\d+\\.)\\d(\\.\\.*)"
  if (str_detect(field_name, re)) {
    result <- str_replace(field_name, re, paste0("\\1", indices, "\\2"))
  } else {
    result <- field_name
  }
  sapply(result, as.symbol, USE.NAMES = F)
}
