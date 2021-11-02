#' @title Self-reported diagnosis lookup
#' @description Look up whether a diagnosis has been self-reported, based on coding 6. See \url{https://biobank.ctsu.ox.ac.uk/crystal/coding.cgi?id=6}. For disease-specific `diagnosis_*()` functions, the function `dx_self_text_to_codes()` is used internally to convert the text form of the diagnosis to the code used in the UKB data table.
#' @inheritParams ukbiobank
#' @param codes Diagnosis codes to look up.
#' @export
dx_self_report_lookup <- function(data,
                                  codes,
                                  after_instance = default_after_inst(),
                                  up_to_instance = default_up_to_inst(),
                                  diagnosis_col = f.20002.0.0.Non_cancer_illness_code_self_reported) {

  # remove any columns with only NAs
  data %<>% remove_na_columns()

  dx_by_instance <- function(i) {
    dx_colnames <- select_instance_and_expand_array(data, {{ diagnosis_col }}, instance = i)
    data %>%
      select(!!!dx_colnames) %>%
      mutate(across(everything(), ~ .x %in% codes)) %>%
      any_by_row()
  }


  instance_combiner(
    data,
    lookup_by_instance_fn = dx_by_instance,
    after_instance = {{ after_instance }},
    up_to_instance = {{ up_to_instance }}
  )
}

#' @noRd
dx_self_text_to_codes <- function(text) {
  meaning <-
    text %>%
    stringr::str_trim(.) %>%
    stringr::str_split(., "\n", simplify = T) %>%
    stringr::str_trim(.)

  coding_medical_conditions %>%
    right_join(tibble(meaning), by = "meaning") %>%
    pull("coding")
}


# ================================================================================================

#' @name diagnosed_placeholder
#' @title Diagnosis algorithms for selected diseases
#' @description These convenience functions perform a combination of self-reported diagnosis lookups, ICD10 lookups, medication lookups, and lab/measurement value lookups in order to return a boolean vector indicating whether a diagnosis has been made within the provided instance ranges. The actual algorithm for each disease diagnosis (e.g. hypertension) is somewhat arbitrary, and (for example) is based on a selected list of medications, blood pressure measurements, ICD10 codes, etc. Users should construct their own algorithms to fit specific use-case scenarios.
#' @inheritParams ukbiobank
#' @inheritSection ukbiobank Instancing
#' @inheritSection ukbiobank Column names
#' @rdname diagnosed_placeholder
#' @export
diagnosed_chf <- function(data,
                          after_instance = default_after_inst(),
                          up_to_instance = default_up_to_inst(),
                          diagnosis_col = f.20002.0.0.Non_cancer_illness_code_self_reported) {
  codings <- dx_self_text_to_codes("
    heart failure/pulmonary odema
  ")
  self_dx <- dx_self_report_lookup(data, codings, after_instance = {{ after_instance }}, up_to_instance = {{ up_to_instance }}, diagnosis_col = {{ diagnosis_col }})
  icd10 <- icd10_heart_failure(data, after_instance = {{ after_instance }}, up_to_instance = {{ up_to_instance }})

  return(self_dx %|% icd10)
}

#' @rdname diagnosed_placeholder
#' @param min_sbp Minimum SBP for HTN diagnosis
#' @param min_dbp Minimum DBP for HTN diagnosis
#' @export
diagnosed_htn <- function(data,
                          after_instance = default_after_inst(),
                          up_to_instance = default_up_to_inst(),
                          diagnosis_col = f.20002.0.0.Non_cancer_illness_code_self_reported,
                          min_sbp = 140,
                          min_dbp = 90) {
  codings <- dx_self_text_to_codes("
    hypertension
    essential hypertension
  ")
  self_dx <- dx_self_report_lookup(data, codings, after_instance = {{ after_instance }}, up_to_instance = {{ up_to_instance }}, diagnosis_col = {{ diagnosis_col }})
  icd10 <- icd10_htn(data, after_instance = {{ after_instance }}, up_to_instance = {{ up_to_instance }})
  meds <- medication_antiHTN(data, after_instance = {{ after_instance }}, up_to_instance = {{ up_to_instance }})
  sbp <- physio_systolicBP(data, after_instance = {{ after_instance }}, up_to_instance = {{ up_to_instance }}, combine_instances = "mean")
  dbp <- physio_diastolicBP(data, after_instance = {{ after_instance }}, up_to_instance = {{ up_to_instance }}, combine_instances = "mean")

  return(self_dx %|% icd10 %|% meds %|% (sbp >= min_sbp) %|% (dbp >= min_dbp))
}

#' @rdname diagnosed_placeholder
#' @export
diagnosed_hld <- function(data,
                          after_instance = default_after_inst(),
                          up_to_instance = default_up_to_inst(),
                          diagnosis_col = f.20002.0.0.Non_cancer_illness_code_self_reported) {
  codings <- dx_self_text_to_codes("
    high cholesterol
  ")
  self_dx <- dx_self_report_lookup(data, codings, after_instance = {{ after_instance }}, up_to_instance = {{ up_to_instance }}, diagnosis_col = {{ diagnosis_col }})
  icd10 <- icd10_hld(data, after_instance = {{ after_instance }}, up_to_instance = {{ up_to_instance }})
  meds <- medication_antiHLD(data, after_instance = {{ after_instance }}, up_to_instance = {{ up_to_instance }})
  ldl <- biomarker_LDL_mgdL(data, after_instance = {{ after_instance }}, up_to_instance = {{ up_to_instance }}, combine_instances = "max")

  return(self_dx %|% icd10 %|% meds %|% (ldl >= 190))
}

#' @rdname diagnosed_placeholder
#' @export
diagnosed_copd <- function(data,
                           after_instance = default_after_inst(),
                           up_to_instance = default_up_to_inst(),
                           diagnosis_col = f.20002.0.0.Non_cancer_illness_code_self_reported) {
  codings <- dx_self_text_to_codes("
    chronic obstructive airways disease/copd
  ")
  self_dx <- dx_self_report_lookup(data, codings, after_instance = {{ after_instance }}, up_to_instance = {{ up_to_instance }}, diagnosis_col = {{ diagnosis_col }})
  icd10 <- icd10_copd(data, after_instance = {{ after_instance }}, up_to_instance = {{ up_to_instance }})

  return(self_dx %|% icd10)
}

#' @rdname diagnosed_placeholder
#' @export
diagnosed_pHTN <- function(data,
                           after_instance = default_after_inst(),
                           up_to_instance = default_up_to_inst(),
                           diagnosis_col = f.20002.0.0.Non_cancer_illness_code_self_reported) {
  icd10 <- icd10_pulm_htn(data, after_instance = {{ after_instance }}, up_to_instance = {{ up_to_instance }})
  meds <- medication_pulm_endothelin(data, after_instance = {{ after_instance }}, up_to_instance = {{ up_to_instance }})
  return(icd10 %|% meds)
}

#' @rdname diagnosed_placeholder
#' @export
diagnosed_pulm_embolism <- function(data,
                                    after_instance = default_after_inst(),
                                    up_to_instance = default_up_to_inst(),
                                    diagnosis_col = f.20002.0.0.Non_cancer_illness_code_self_reported) {
  codings <- dx_self_text_to_codes("
    pulmonary embolism +/- dvt
  ")
  self_dx <- dx_self_report_lookup(data, codings, after_instance = {{ after_instance }}, up_to_instance = {{ up_to_instance }}, diagnosis_col = {{ diagnosis_col }})
  icd10 <- icd10_pulm_embolism(data, after_instance = {{ after_instance }}, up_to_instance = {{ up_to_instance }})

  return(self_dx %|% icd10)
}

#' @rdname diagnosed_placeholder
#' @export
diagnosed_diabetes <- function(data,
                               after_instance = default_after_inst(),
                               up_to_instance = default_up_to_inst(),
                               diagnosis_col = f.20002.0.0.Non_cancer_illness_code_self_reported) {
  codings <- dx_self_text_to_codes("
    diabetes
    type 1 diabetes
    type 2 diabetes
  ")
  self_dx <- dx_self_report_lookup(data, codings, after_instance = {{ after_instance }}, up_to_instance = {{ up_to_instance }}, diagnosis_col = {{ diagnosis_col }})
  icd10 <- icd10_diabetes(data, after_instance = {{ after_instance }}, up_to_instance = {{ up_to_instance }})
  insulin <- medication_insulin(data, after_instance = {{ after_instance }}, up_to_instance = {{ up_to_instance }})
  oha_meds <- medication_oralhypoglycemic(data, after_instance = {{ after_instance }}, up_to_instance = {{ up_to_instance }})
  a1c <- biomarker_A1c_percent(data, after_instance = {{ after_instance }}, up_to_instance = {{ up_to_instance }})

  return(self_dx %|% icd10 %|% oha_meds %|% insulin %|% (a1c >= 6.5))
}
