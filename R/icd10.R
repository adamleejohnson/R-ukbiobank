#' ICD10 Lookup
#' @inheritParams ukbiobank
#' @param icd10_field example ICD10 field
#' @param icd10_date_field example ICD10 date field
#' @param codes ICD10 codes to look up
#' @export
icd10_lookup <- function(data,
                         codes,
                         up_to_instance = 3,
                         icd10_field = f.41270.0.0.Diagnoses_ICD10,
                         icd10_date_field = f.41280.0.0.Date_of_first_in_patient_diagnosis_ICD10,
                         date_of_instance_field = f.53.0.0.Date_of_attending_assessment_centre) {

  # convert icd10 codes to ukb codings
  icd10_lookups <-
    tibble(icd10 = codes) %>%
    left_join(coding_icd10, by = "icd10") %>%
    pull("coding")

  # remove any columns with only NAs
  data %<>% remove_na_columns()
  codes %<>% unique()

  # generate lists of column names
  date_of_instance_colnames <- expand_instances(data, {{ date_of_instance_field }})
  icd10_colnames <- expand_array(data, {{ icd10_field }})
  icd10_date_colnames <- expand_array(data, {{ icd10_date_field }})
  stopifnot(length(icd10_colnames) == length(icd10_date_colnames))

  # define function that will return whether the patient has icd10 codes
  # and were diagnosed on or before the date of the instance/assessment
  icd10_by_instance <- function(inst) {

    # Check if icd10 codes match the provided search codes
    icd_match <-
      data %>%
      select(!!!icd10_colnames) %>%
      mutate(across(everything(), ~ .x %in% icd10_lookups)) %>%
      as.matrix()

    # compare dates from the icd fields to dates of the instance,
    # and create a filter based on whether the date is allowed
    inst_dates <-
      data %>%
      pull(!!date_of_instance_colnames[[inst + 1]]) %>%
      as.Date() %>%
      as.integer() %>%
      matrix(., length(.), length(icd10_date_colnames))
    icd_dates <-
      data %>%
      select(!!!icd10_date_colnames) %>%
      mutate(across(everything(), ~ as.integer(as.Date(.x)))) %>%
      as.matrix()
    keep_icd <- icd_dates <= inst_dates
    keep_icd <- ifelse(is.na(keep_icd), F, keep_icd)

    # Apply the filter based on whether the date of the icd diagnosis is allowed
    icd_match <- icd_match & keep_icd
    icd_match %>% any_by_row()
  }


  up_to_instance_combiner(
    data,
    lookup_by_instance_fn = icd10_by_instance,
    up_to_instance = {{ up_to_instance }}
  )
}

#' @noRd
icd10_lines_to_codes <- function(text) {
  codes <-
    text %>%
    stringr::str_trim(.) %>%
    stringr::str_split(., "\n", simplify = T) %>%
    stringr::str_trim(.) %>%
    stringr::str_split(., "\\s", 2, simplify = T)
  codes <- codes[, 1]
  codes <- codes[which(codes != "")]
  codes %<>% unique()
}

#' @rdname icd10_lookup
#' @export
icd10_htn <- function(data,
                      up_to_instance = 3,
                      icd10_field = f.41270.0.0.Diagnoses_ICD10) {
  codings <- icd10_lines_to_codes("
    I10 Essential (primary) hypertension
    I11 Hypertensive heart disease
    I11.0 Hypertensive heart disease with (congestive) heart failure
    I11.9 Hypertensive heart disease without (congestive) heart failure
    I12 Hypertensive renal disease
    I12.0 Hypertensive renal disease with renal failure
    I12.9 Hypertensive renal disease without renal failure
    I13 Hypertensive heart and renal disease
    I13.0 Hypertensive heart and renal disease with (congestive) heart failure
    I13.1 Hypertensive heart and renal disease with renal failure
    I13.2 Hypertensive heart and renal disease with both (congestive) heart failure and renal failure
    I13.9 Hypertensive heart and renal disease, unspecified
    I15 Secondary hypertension
    I15.0 Renovascular hypertension
    I15.1 Hypertension secondary to other renal disorders
    I15.2 Hypertension secondary to endocrine disorders
    I15.8 Other secondary hypertension
    I15.9 Seconday hypertension, unspecified
  ")
  icd10_lookup(data, codings, icd10_field = {{ icd10_field }}, up_to_instance = {{ up_to_instance }})
}

#' @rdname icd10_lookup
#' @export
icd10_hld <- function(data,
                      up_to_instance = 3,
                      icd10_field = f.41270.0.0.Diagnoses_ICD10) {
  codings <- icd10_lines_to_codes("
    E78.0 Pure hypercholesterolaemia
    E78.1 Pure hyperglyceridaemia
    E78.2 Mixed hyperlipidaemia
    E78.4 Other hyperlipidaemia
    E78.5 Hyperlipidaemia, unspecified
  ")
  icd10_lookup(data, codings, icd10_field = {{ icd10_field }}, up_to_instance = {{ up_to_instance }})
}

#' @rdname icd10_lookup
#' @export
icd10_heart_failure <- function(data,
                                up_to_instance = 3,
                                icd10_field = f.41270.0.0.Diagnoses_ICD10) {
  codings <- icd10_lines_to_codes("
    I50 Heart failure
    I50.0 Congestive heart failure
    I50.1 Left ventricular failure
    I50.9 Heart failure, unspecified
    I11.0 Hypertensive heart disease with (congestive) heart failure
  ")
  icd10_lookup(data, codings, icd10_field = {{ icd10_field }}, up_to_instance = {{ up_to_instance }})
}

#' @rdname icd10_lookup
#' @export
icd10_pulm_htn <- function(data,
                           up_to_instance = 3,
                           icd10_field = f.41270.0.0.Diagnoses_ICD10) {
  codings <- icd10_lines_to_codes("
    I27.0 Primary pulmonary hypertension
    I27.2 Other secondary pulmonary hypertension
    I27.8 Other specified pulmonary heart diseases
    I27.9 Pulmonary heart disease, unspecified
  ")
  icd10_lookup(data, codings, icd10_field = {{ icd10_field }}, up_to_instance = {{ up_to_instance }})
}

#' @rdname icd10_lookup
#' @export
icd10_pulm_embolism <- function(data,
                                up_to_instance = 3,
                                icd10_field = f.41270.0.0.Diagnoses_ICD10) {
  codings <- icd10_lines_to_codes("
    I26 Pulmonary embolism
    I26.0 Pulmonary embolism with mention of acute cor pulmonale
    I26.9 Pulmonary embolism without mention of acute cor pulmonale
  ")
  icd10_lookup(data, codings, icd10_field = {{ icd10_field }}, up_to_instance = {{ up_to_instance }})
}

#' @rdname icd10_lookup
#' @export
icd10_pulm_misc <- function(data,
                            up_to_instance = 3,
                            icd10_field = f.41270.0.0.Diagnoses_ICD10) {
  codings <- icd10_lines_to_codes("
    I28 Other diseases of pulmonary vessels
    I28.0 Arteriovenous fistula of pulmonary vessels
    I28.1 Aneurysm of pulmonary artery
    I28.8 Other specified diseases of pulmonary vessels
    I28.9 Disease of pulmonary vessels, unspecified
    I37 Pulmonary valve disorders
    I37.0 Pulmonary valve stenosis
    I37.1 Pulmonary valve insufficiency
    I37.2 Pulmonary valve stenosis with insufficiency
    I37.8 Other pulmonary valve disorders
    I37.9 Pulmonary valve disorder, unspecified
    I39.3 Pulmonary valve disorders in diseases classified elsewhere
    Q21.4 Aortopulmonary septal defect
    Q22 Congenital malformations of pulmonary and tricuspid valves
    Q22.0 Pulmonary valve atresia
    Q22.1 Congenital pulmonary valve stenosis
    Q22.2 Congenital pulmonary valve insufficiency
    Q22.3 Other congenital malformations of pulmonary valve
    Q24.3 Pulmonary infundibular stenosis
    Q25.5 Atresia of pulmonary artery
    Q25.6 Stenosis of pulmonary artery
    Q25.7 Other congenital malformations of pulmonary artery
    Q26.2 Total anomalous pulmonary venous connexion
    Q26.3 Partial anomalous pulmonary venous connexion
    Q26.4 Anomalous pulmonary venous connexion, unspecified
    S25.4 Injury of pulmonary blood vessels
  ")
  icd10_lookup(data, codings, icd10_field = {{ icd10_field }}, up_to_instance = {{ up_to_instance }})
}

#' @rdname icd10_lookup
#' @export
icd10_pulm_ILD <- function(data,
                           up_to_instance = 3,
                           icd10_field = f.41270.0.0.Diagnoses_ICD10) {
  codings <- icd10_lines_to_codes("
    J70.2 Acute drug-induced interstitial lung disorders
    J70.3 Chronic drug-induced interstitial lung disorders
    J70.4 Drug-induced interstitial lung disorder, unspecified
    J84 Other interstitial pulmonary diseases
    J84.1 Other interstitial pulmonary diseases with fibrosis
    J84.8 Other specified interstitial pulmonary diseases
    J84.9 Interstitial pulmonary disease, unspecified
    E84.0 Cystic fibrosis with pulmonary manifestations
  ")
  icd10_lookup(data, codings, icd10_field = {{ icd10_field }}, up_to_instance = {{ up_to_instance }})
}

#' @rdname icd10_lookup
#' @export
icd10_copd <- function(data,
                       up_to_instance = 3,
                       icd10_field = f.41270.0.0.Diagnoses_ICD10) {
  codings <- icd10_lines_to_codes("
    J43 Emphysema
    J43.1 Panlobular emphysema
    J43.2 Centrilobular emphysema
    J43.8 Other emphysema
    J43.9 Emphysema, unspecified
    J44 Other chronic obstructive pulmonary disease
    J44.0 Chronic obstructive pulmonary disease with acute lower respiratory infection
    J44.1 Chronic obstructive pulmonary disease with acute exacerbation, unspecified
    J44.8 Other specified chronic obstructive pulmonary disease
    J44.9 Chronic obstructive pulmonary disease, unspecified
    J98.2 Interstitial emphysema
    J98.3 Compensatory emphysema
  ")
  icd10_lookup(data, codings, icd10_field = {{ icd10_field }}, up_to_instance = {{ up_to_instance }})
}

#' @rdname icd10_lookup
#' @export
icd10_heart_pulm_transplant <- function(data,
                                        up_to_instance = 3,
                                        icd10_field = f.41270.0.0.Diagnoses_ICD10) {
  codings <- icd10_lines_to_codes("
    T86.2 Heart transplant failure and rejection
    T86.3 Heart-lung transplant failure and rejection
    Z94.2 Lung transplant status
    Z94.3 Heart and lungs transplant status
    Z99.4 Dependence on artificial heart
  ")
  icd10_lookup(data, codings, icd10_field = {{ icd10_field }}, up_to_instance = {{ up_to_instance }})
}

#' @rdname icd10_lookup
#' @export
icd10_diabetes <- function(data,
                           up_to_instance = 3,
                           icd10_field = f.41270.0.0.Diagnoses_ICD10) {
  codings <- icd10_lines_to_codes("
    E10 Insulin-dependent diabetes mellitus
    E10.0 With coma
    E10.1 With ketoacidosis
    E10.2 With renal complications
    E10.3 With ophthalmic complications
    E10.4 With neurological complications
    E10.5 With peripheral circulatory complications
    E10.6 With other specified complications
    E10.7 With multiple complications
    E10.8 With unspecified complications
    E10.9 Without complications
    E11 Non-insulin-dependent diabetes mellitus
    E11.0 With coma
    E11.1 With ketoacidosis
    E11.2 With renal complications
    E11.3 With ophthalmic complications
    E11.4 With neurological complications
    E11.5 With peripheral circulatory complications
    E11.6 With other specified complications
    E11.7 With multiple complications
    E11.8 With unspecified complications
    E11.9 Without complications
    E12 Malnutrition-related diabetes mellitus
    E12.0 With coma
    E12.1 With ketoacidosis
    E12.2 With renal complications
    E12.3 With ophthalmic complications
    E12.4 With neurological complications
    E12.5 With peripheral circulatory complications
    E12.6 With other specified complications
    E12.7 With multiple complications
    E12.8 With unspecified complications
    E12.9 Without complications
    E13 Other specified diabetes mellitus
    E13.0 With coma
    E13.1 With ketoacidosis
    E13.2 With renal complications
    E13.3 With ophthalmic complications
    E13.4 With neurological complications
    E13.5 With peripheral circulatory complications
    E13.6 With other specified complications
    E13.7 With multiple complications
    E13.8 With unspecified complications
    E13.9 Without complications
    E14 Unspecified diabetes mellitus
    E14.0 With coma
    E14.1 With ketoacidosis
    E14.2 Withrenal complications
    E14.3 With ophthalmic complications
    E14.4 With neurological complications
    E14.5 With peripheral circulatory complications
    E14.6 With other specified complications
    E14.7 With multiple complications
    E14.8 With unspecified complications
    E14.9 Without complications
    N08.3 Glomerular disorders in diabetes mellitus
    O24.0 Pre-existing diabetes mellitus, insulin-dependent
    O24.1 Pre-existing diabetes mellitus, noninsulin-dependent
    O24.2 Pre-existing malnutrition-related diabetes mellitus
    O24.3 Pre-existing diabetes mellitus, unspecified
  ")
  icd10_lookup(data, codings, icd10_field = {{ icd10_field }}, up_to_instance = {{ up_to_instance }})
}