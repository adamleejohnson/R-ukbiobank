#' Diagnosis (self-reported) Lookup
#'
#' @inheritParams ukbiobank
#' @param codes Diagnosis codes to look up
#'
#' @export
diagnosis_self_report_lookup <- function(data,
                                         codes,
                                         up_to_instance = 3,
                                         diagnosis_field = f.20002.0.0.Non_cancer_illness_code_self_reported) {

  # remove any columns with only NAs
  data %<>% remove_na_columns()

  dx_by_instance <- function(i) {
    dx_colnames <- select_instance_and_expand_array(data, {{ diagnosis_field }}, instance = i)
    data %>%
      select(!!!dx_colnames) %>%
      mutate(across(everything(), ~ .x %in% codes)) %>%
      any_by_row()
  }


  up_to_instance_combiner(
    data,
    lookup_by_instance_fn = dx_by_instance,
    up_to_instance = {{ up_to_instance }}
  )
}

#' @noRd
self_dx_text_to_codings <- function(text) {
  meaning <-
    text %>%
    stringr::str_trim(.) %>%
    stringr::str_split(., "\n", simplify = T) %>%
    stringr::str_trim(.)

  coding_medical_conditions %>%
    right_join(tibble(meaning), by = "meaning") %>%
    pull("coding")
}

#' @rdname diagnosis_self_report_lookup
#' @export
diagnosed_chf <- function(data, up_to_instance = 3) {
  codings <- self_dx_text_to_codings("
    heart failure/pulmonary odema
  ")
  self_dx <- diagnosis_self_report_lookup(data, codings, up_to_instance = {{ up_to_instance }})
  icd10 <- icd10_heart_failure(data, up_to_instance = {{ up_to_instance }})

  return(self_dx %|% icd10)
}

#' @rdname diagnosis_self_report_lookup
#' @export
diagnosed_htn <- function(data, up_to_instance = 3) {
  codings <- self_dx_text_to_codings("
    hypertension
    essential hypertension
  ")
  self_dx <- diagnosis_self_report_lookup(data, codings, up_to_instance = {{ up_to_instance }})
  icd10 <- icd10_htn(data, up_to_instance = {{ up_to_instance }})
  meds <- medication_antiHTN(data, up_to_instance = {{ up_to_instance }})
  sbp <- physio_systolicBP(data, up_to_instance = {{ up_to_instance }}, combine_instances = "mean")
  dbp <- physio_diastolicBP(data, up_to_instance = {{ up_to_instance }}, combine_instances = "mean")

  return(self_dx %|% icd10 %|% meds %|% (sbp > 130) %|% (dbp > 80))
}

#' @rdname diagnosis_self_report_lookup
#' @export
diagnosed_hld <- function(data, up_to_instance = 3) {
  codings <- self_dx_text_to_codings("
    high cholesterol
  ")
  self_dx <- diagnosis_self_report_lookup(data, codings, up_to_instance = {{ up_to_instance }})
  icd10 <- icd10_hld(data, up_to_instance = {{ up_to_instance }})
  meds <- medication_antiHLD(data, up_to_instance = {{ up_to_instance }})
  ldl <- biomarker_LDL_mgdL(data, up_to_instance = {{ up_to_instance }}, combine_instances = "max")

  return(self_dx %|% icd10  %|% meds %|% (ldl >= 190))
}

#' @rdname diagnosis_self_report_lookup
#' @export
diagnosed_copd <- function(data, up_to_instance = 3) {
  codings <- self_dx_text_to_codings("
    chronic obstructive airways disease/copd
  ")
  self_dx <- diagnosis_self_report_lookup(data, codings, up_to_instance = {{ up_to_instance }})
  icd10 <- icd10_copd(data, up_to_instance = {{ up_to_instance }})

  return(self_dx %|% icd10)
}

#' @rdname diagnosis_self_report_lookup
#' @export
diagnosed_pHTN <- function(data, up_to_instance = 3) {
  icd10 <- icd10_pulm_htn(data, up_to_instance = {{ up_to_instance }})
  meds <- medication_pulm_endothelin(data, up_to_instance = {{ up_to_instance }})
  return(icd10 %|% meds)
}

#' @rdname diagnosis_self_report_lookup
#' @export
diagnosed_pulm_embolism <- function(data, up_to_instance = 3) {
  codings <- self_dx_text_to_codings("
    pulmonary embolism +/- dvt
  ")
  self_dx <- diagnosis_self_report_lookup(data, codings, up_to_instance = {{ up_to_instance }})
  icd10 <- icd10_pulm_embolism(data, up_to_instance = {{ up_to_instance }})

  return(self_dx %|% icd10)
}

#' @rdname diagnosis_self_report_lookup
#' @export
diagnosed_diabetes <- function(data, up_to_instance = 3) {
  codings <- self_dx_text_to_codings("
    diabetes
    type 1 diabetes
    type 2 diabetes
  ")
  self_dx <- diagnosis_self_report_lookup(data, codings, up_to_instance = {{ up_to_instance }})
  icd10 <- icd10_diabetes(data, up_to_instance = {{ up_to_instance }})
  insulin <- medication_insulin(data, up_to_instance = {{ up_to_instance }})
  oha_meds <- medication_oralhypoglycemic(data, up_to_instance = {{ up_to_instance }})
  a1c <- biomarker_A1c_percent(data, up_to_instance = {{ up_to_instance }})

  return(self_dx %|% icd10 %|% oha_meds %|% insulin %|% (a1c >= 6.5))
}
