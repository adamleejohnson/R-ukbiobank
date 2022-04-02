#' @title Self-reported ethnicity, simplified
#' @description Simplifies ethnicity codings (as specified in \url{https://biobank.ctsu.ox.ac.uk/crystal/coding.cgi?id=1001}) to the parent category (e.g., converts White-British to White, and Black-African to Black). If reported ethnicity is not consistent across instances (there are a few), returns "Other" for that participant.
#' @inheritParams ukbiobank
#' @inheritSection ukbiobank Column names
#' @export
ethnicity_self_reported <- function(data, ethnicity_col = f.21000.0.0.Ethnic_background) {
  ethnicity_col_cols <- expand_instances(data, {{ ethnicity_col }})

  get_ethnic_category <- function(x) {
    data %>%
      rename(meaning = {{ x }}) %>%
      left_join(coding_ethnicity, by = "meaning") %>%
      mutate(
        if_else(parent_id == 0, node_id, floor(node_id / 1000))
      ) %>%
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

  ethnicity_0 <- ethnicity_1 <- ethnicity_2 <- NULL

  data %>%
    transmute(
      ethnicity_0 = get_ethnic_category(ethnicity_col_cols[[1]]),
      ethnicity_1 = get_ethnic_category(ethnicity_col_cols[[2]]),
      ethnicity_2 = get_ethnic_category(ethnicity_col_cols[[3]])
    ) %>%
    rowwise() %>%
    mutate(
      consistency_chk = consistency_chk(ethnicity_0, ethnicity_1, ethnicity_2),
      ethnicity_cmn = ifelse(consistency_chk, unique_value(ethnicity_0, ethnicity_1, ethnicity_2), NA)
    ) %>%
    mutate(
      case_when(
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