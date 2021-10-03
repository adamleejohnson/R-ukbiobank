#' ICD10 Lookup
#' @param data data table
#' @param .icd10_field example ICD10 field
#' @param ... ICD10 codes to look up
#' @export
ICD10_lookup <- function(data, ..., .icd10_field = f.41270.0.0.Diagnoses_ICD10) {

  icd10_columns <- expand_array(data, {{ .icd10_field }})
  icd10_lookups <-
    tibble(icd10 = c(...)) %>%
    left_join(coding_icd10, by = "icd10") %>%
    pull(coding)

  data %>%
    select(!!!icd10_columns) %>%
    mutate(across(everything(), ~.x %in% icd10_lookups)) %>%
    rowwise() %>% mutate(any(c_across(everything()))) %>%
    pull()
}

#' @noRd
icd10_lines_to_codes <- function(text) {
  codes <-
    text %>%
    str_trim() %>% str_split("\n", simplify = T) %>%
    str_trim() %>% str_split("\\s", 2, simplify = T)
  codes[,1]
}

#' @rdname ICD10_lookup
#' @export
ICD10_htn <- function(data, .icd10_field = f.41270.0.0.Diagnoses_ICD10) {
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
  ICD10_lookup(data, codings, .icd10_field = {{.icd10_field}})
}

#' @rdname ICD10_lookup
#' @export
ICD10_hld <- function(data, .icd10_field = f.41270.0.0.Diagnoses_ICD10) {
  codings <- icd10_lines_to_codes("
E78.0 Pure hypercholesterolaemia
E78.1 Pure hyperglyceridaemia
E78.2 Mixed hyperlipidaemia
E78.4 Other hyperlipidaemia
E78.5 Hyperlipidaemia, unspecified
  ")
  ICD10_lookup(data, codings, .icd10_field = {{.icd10_field}})
}

#' @rdname ICD10_lookup
#' @export
ICD10_heart_failure <- function(data, .icd10_field = f.41270.0.0.Diagnoses_ICD10) {
  codings <- icd10_lines_to_codes("
    I50 Heart failure
    I50.0 Congestive heart failure
    I50.1 Left ventricular failure
    I50.9 Heart failure, unspecified
    I11.0 Hypertensive heart disease with (congestive) heart failure
  ")
  ICD10_lookup(data, codings, .icd10_field = {{.icd10_field}})
}

#' @rdname ICD10_lookup
#' @export
ICD10_pulm_htn <- function(data, .icd10_field = f.41270.0.0.Diagnoses_ICD10) {
  codings <- icd10_lines_to_codes("
    I27.0 Primary pulmonary hypertension
    I27.2 Other secondary pulmonary hypertension
    I27.8 Other specified pulmonary heart diseases
    I27.9 Pulmonary heart disease, unspecified
  ")
  ICD10_lookup(data, codings, .icd10_field = {{.icd10_field}})
}

#' @rdname ICD10_lookup
#' @export
ICD10_pulm_embolism <- function(data, .icd10_field = f.41270.0.0.Diagnoses_ICD10) {
  codings <- icd10_lines_to_codes("
    I26 Pulmonary embolism
    I26.0 Pulmonary embolism with mention of acute cor pulmonale
    I26.9 Pulmonary embolism without mention of acute cor pulmonale
  ")
  ICD10_lookup(data, codings, .icd10_field = {{.icd10_field}})
}

#' @rdname ICD10_lookup
#' @export
ICD10_pulm_misc <- function(data, .icd10_field = f.41270.0.0.Diagnoses_ICD10) {
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
  ICD10_lookup(data, codings, .icd10_field = {{.icd10_field}})
}

#' @rdname ICD10_lookup
#' @export
ICD10_pulm_ILD <- function(data, .icd10_field = f.41270.0.0.Diagnoses_ICD10) {
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
  ICD10_lookup(data, codings, .icd10_field = {{.icd10_field}})
}

#' @rdname ICD10_lookup
#' @export
ICD10_pulm_emphysema <- function(data, .icd10_field = f.41270.0.0.Diagnoses_ICD10) {
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
  ICD10_lookup(data, codings, .icd10_field = {{.icd10_field}})
}

#' @rdname ICD10_lookup
#' @export
ICD10_heart_pulm_transplant <- function(data, .icd10_field = f.41270.0.0.Diagnoses_ICD10) {
  codings <- icd10_lines_to_codes("
    T86.2 Heart transplant failure and rejection
    T86.3 Heart-lung transplant failure and rejection
    Z94.2 Lung transplant status
    Z94.3 Heart and lungs transplant status
    Z99.4 Dependence on artificial heart
  ")
  ICD10_lookup(data, codings, .icd10_field = {{.icd10_field}})
}
