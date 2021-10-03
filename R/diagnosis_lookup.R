#' Diagnosis (self-reported) Lookup
#' @param data data table
#' @param .icd10_field example diagnosis field
#' @param ... Diagnosis codes to look up
#' @export
diagnosis_lookup <- function(data, ..., .diagnosis_field = f.20002.0.0.Non_cancer_illness_code_self_reported) {

  dx_columns <- expand_instance_and_array(data, {{ .diagnosis_field }})
  dx_codes <- c(...)

  data %>%
    select(!!!dx_columns) %>%
    mutate(across(everything(), ~.x %in% dx_codes)) %>%
    rowwise() %>% mutate(any(c_across(everything()))) %>%
    pull()
}

#' @noRd
dx_text_to_codings <- function(text) {
  meaning <-
    text %>%
    str_trim() %>%
    str_split("\n", simplify = T) %>%
    str_trim()
  coding_medical_conditions %>%
    right_join(tibble(meaning), by = "meaning") %>%
    pull(coding)
}

#' @rdname diagnosis_lookup
#' @export
diagnosed_chf <- function(data,
                          .diagnosis_field = f.20002.0.0.Non_cancer_illness_code_self_reported,
                          .icd10_field = f.41270.0.0.Diagnoses_ICD10) {

  codings <- dx_text_to_codings("
    heart failure/pulmonary odema
  ")

  dx <- diagnosis_lookup(data, codings, .diagnosis_field = {{.diagnosis_field}})
  icd10 <- ICD10_heart_failure(data, .icd10_field = {{.icd10_field}})
  return(dx | icd10)
}

#' @rdname diagnosis_lookup
#' @export
diagnosed_htn <- function(data,
                          .diagnosis_field = f.20002.0.0.Non_cancer_illness_code_self_reported,
                          .icd10_field = f.41270.0.0.Diagnoses_ICD10) {

  codings <- dx_text_to_codings("
    hypertension
    essential hypertension
  ")
  dx <- diagnosis_lookup(data, codings, .diagnosis_field = {{.diagnosis_field}})

  icd10 <- ICD10_htn(data, .icd10_field = {{.icd10_field}})

  return(dx | icd10)
}

#' @rdname diagnosis_lookup
#' @export
diagnosed_hld <- function(data,
                          .diagnosis_field = f.20002.0.0.Non_cancer_illness_code_self_reported,
                          .icd10_field = f.41270.0.0.Diagnoses_ICD10) {

  codings <- dx_text_to_codings("
    high cholesterol
  ")
  dx <- diagnosis_lookup(data, codings, .diagnosis_field = {{.diagnosis_field}})

  icd10 <- ICD10_hld(data, .icd10_field = {{.icd10_field}})

  return(dx | icd10)
}

#' @rdname diagnosis_lookup
#' @export
diagnosed_copd <- function(data,
                          .diagnosis_field = f.20002.0.0.Non_cancer_illness_code_self_reported,
                          .icd10_field = f.41270.0.0.Diagnoses_ICD10) {

  codings <- dx_text_to_codings("
    chronic obstructive airways disease/copd
  ")
  dx <- diagnosis_lookup(data, codings, .diagnosis_field = {{.diagnosis_field}})

  icd10 <- ICD10_copd(data, .icd10_field = {{.icd10_field}})

  return(dx | icd10)
}

#' @rdname diagnosis_lookup
#' @export
diagnosed_pHTN <- function(data,
                          .diagnosis_field = f.20002.0.0.Non_cancer_illness_code_self_reported,
                          .icd10_field = f.41270.0.0.Diagnoses_ICD10) {

  # codings <- dx_text_to_codings("
  #   chronic obstructive airways disease/copd
  # ")
  # dx <- diagnosis_lookup(data, codings, .diagnosis_field = {{.diagnosis_field}})

  icd10 <- ICD10_copd(data, .icd10_field = {{.icd10_field}})

  return(icd10)
}

#' @rdname diagnosis_lookup
#' @export
diagnosed_pulm_embolism <- function(data,
                          .diagnosis_field = f.20002.0.0.Non_cancer_illness_code_self_reported,
                          .icd10_field = f.41270.0.0.Diagnoses_ICD10) {

  codings <- dx_text_to_codings("
    pulmonary embolism +/- dvt
  ")
  dx <- diagnosis_lookup(data, codings, .diagnosis_field = {{.diagnosis_field}})

  icd10 <- ICD10_pulm_embolism(data, .icd10_field = {{.icd10_field}})

  return(dx | icd10)
}

#' @rdname diagnosis_lookup
#' @export
diagnosed_diabetes <- function(data,
                          .diagnosis_field = f.20002.0.0.Non_cancer_illness_code_self_reported,
                          .icd10_field = f.41270.0.0.Diagnoses_ICD10) {

  codings <- dx_text_to_codings("
    diabetes
    type 1 diabetes
    type 2 diabetes
  ")
  dx <- diagnosis_lookup(data, codings, .diagnosis_field = {{.diagnosis_field}})

  icd10 <- ICD10_diabetes(data, .icd10_field = {{.icd10_field}})

  return(dx | icd10)
}
