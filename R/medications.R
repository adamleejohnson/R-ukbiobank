#' Medication Lookups
#'
#' @inheritParams ukbiobank
#' @param codes Medication codes to look up
#' @param medication_field e.g. `f.20003.0.0.Treatment_medication_code`
#'
#' @export
medication_lookup <- function(data,
                              codes,
                              up_to_instance = 3,
                              medication_field = f.20003.0.0.Treatment_medication_code) {

  # remove any columns with only NAs
  data %<>% remove_na_columns()
  codes %<>% unique()

  med_by_instance <- function(i) {
    med_colnames <- select_instance_and_expand_array(data, {{ medication_field }}, instance = i)
    data %>%
      select(!!!med_colnames) %>%
      mutate(across(everything(), ~ .x %in% codes)) %>%
      any_by_row()
  }


  up_to_instance_combiner(
    data,
    lookup_by_instance_fn = med_by_instance,
    up_to_instance = {{ up_to_instance }}
  )
}

#' @noRd
med_text_to_codings <- function(text) {
  meaning <-
    text %>%
    stringr::str_trim(.) %>%
    stringr::str_split(., "\n", simplify = T) %>%
    stringr::str_trim(.) %>%
    unique()

  meaning <- meaning[which(meaning != "")]

  codings <-
    coding_medications %>%
    right_join(tibble(meaning), by = "meaning") %>%
    pull("coding")

  stopifnot(!any(is.na(codings)))

  return(codings)
}

#' @name medication_lookup_by_disease
#' @title Medication lookups for selected diseases
#'
#' These functions are aliases for [medication_lookup()], using hard-coded `codes` that have been pre-selected for each disease/disease category.
#'
#' @inheritParams ukbiobank
#' @param ... Arguments passed to [medication_lookup()] (excluding the `codes` argument, which is pre-set by the `medication_` lookup functions).
#'
#' Specifically:
#' * `data`
#' * `up_to_instance = 3`
#' * `medication_field = f.20003.0.0.Treatment_medication_code`
NULL


#' @rdname medication_lookup_by_disease
#' @export
medication_statin <- function(data, ...) medication_lookup(data, codes = meds_statin, ...)

#' @rdname medication_lookup_by_disease
#' @export
medication_antiHLD <- function(data, ...) {
  medication_lookup(data, codes = c(
    meds_statin,
    meds_antiHLD_other
  ), ...)
}

#' @rdname medication_lookup_by_disease
#' @export
medication_PDE5i <- function(data, ...) medication_lookup(data, codes = meds_PDE5i, ...)

#' @rdname medication_lookup_by_disease
#' @export
medication_BB <- function(data, ...) medication_lookup(data, codes = meds_betablocker, ...)

#' @rdname medication_lookup_by_disease
#' @export
medication_ACEi <- function(data, ...) medication_lookup(data, codes = meds_ACEi, ...)

#' @rdname medication_lookup_by_disease
#' @export
medication_ARB <- function(data, ...) medication_lookup(data, codes = meds_ARB, ...)

#' @rdname medication_lookup_by_disease
#' @export
medication_CCB <- function(data, ...) medication_lookup(data, codes = meds_CCB, ...)

#' @rdname medication_lookup_by_disease
#' @export
medication_loopdiuretic <- function(data, ...) medication_lookup(data, codes = meds_loopdiuretic, ...)

#' @rdname medication_lookup_by_disease
#' @export
medication_thiazide <- function(data, ...) medication_lookup(data, codes = meds_thiazide, ...)

#' @rdname medication_lookup_by_disease
#' @export
medication_antiHTN <- function(data, ...) {
  medication_lookup(data, codes = c(
    meds_antiHTN_other,
    meds_ACEi,
    meds_ARB,
    meds_CCB,
    meds_betablocker,
    meds_thiazide
  ), ...)
}

#' @rdname medication_lookup_by_disease
#' @export
medication_insulin <- function(data, ...) medication_lookup(data, codes = meds_insulin, ...)

#' @rdname medication_lookup_by_disease
#' @export
medication_oralhypoglycemic <- function(data, ...) medication_lookup(data, codes = meds_OHA, ...)

#' @rdname medication_lookup_by_disease
#' @export
medication_pulm_endothelin <- function(data, ...) medication_lookup(data, codes = meds_pulm_endothelin, ...)
