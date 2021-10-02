#' Title
#'
#' @param data
#' @param .ethnic_bkg
#'
#' @return
#' @export
#'
#' @examples
Ethnicity_self_reported <- function(data, .ethnic_bkg = f.21000.0.0.Ethnic_background)
{
  ethnic_bkg_cols <- expand_instances({{.ethnic_bkg}}, 0:2)

  get_ethnic_category <- function(x) {
    data %>%
      rename(meaning = {{x}}) %>%
      left_join(coding_ethnicity, by = "meaning") %>%
      mutate(category = if_else(parent_id == 0, node_id, floor(node_id/1000))) %>%
      pull()
  }

  consistency_chk <- function(...) {
    w <- c(...)
    w <- w[!is.na(w)]
    length(unique(w)) == 1
  }

  unique_value <- function(...) {
    w <- c(...)
    w <- w[!is.na(w)]
    unique(w)
  }

  data %>%
    transmute(
      ethnicity_0 = get_ethnic_category( ethnic_bkg_cols[[1]] ),
      ethnicity_1 = get_ethnic_category( ethnic_bkg_cols[[2]] ),
      ethnicity_2 = get_ethnic_category( ethnic_bkg_cols[[3]] )
    ) %>%
    rowwise() %>% mutate(
      consistency_chk = consistency_chk(ethnicity_0, ethnicity_1, ethnicity_2),
      ethnicity_cmn = ifelse(consistency_chk, unique_value(ethnicity_0, ethnicity_1, ethnicity_2), NA)
    ) %>%
    mutate(
      ethnicity_text = case_when(
        is.na(ethnicity_cmn) ~ "Other",
        ethnicity_cmn < 1 ~ "Other",
        ethnicity_cmn == 1 ~ "White",
        ethnicity_cmn == 2 ~ "Mixed",
        ethnicity_cmn == 3 ~ "South-Asian",
        ethnicity_cmn == 4 ~ "Black",
        TRUE ~ "Other"
      )
    ) %>%
    pull()
}
