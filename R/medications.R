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
#' These functions are aliases for [medication_lookup()], using hard-coded `codes` that have been pre-selected for each disease/medication class.
#'
#' @inheritParams ukbiobank
#' @param ... Arguments passed to [medication_lookup()] (excluding the `codes` argument, which is pre-set by the `medication_` lookup functions).
#'
#' Specifically:
#' * `data`
#' * `up_to_instance = 3`
#' * `medication_field = f.20003.0.0.Treatment_medication_code`
NULL

#' @noRd
meds_statin <- "
  simvastatin
  fluvastatin
  pravastatin
  eptastatin
  velastatin
  atorvastatin
  rosuvastatin
"

#' @noRd
meds_antiHLD_other <- "
  ezetimibe
  bezafibrate
  clofibrate
  fenofibrate
  ciprofibrate
  bezafibrate product
  gemfibrozil
  gemfibrozil product
  niacin
"

#' @noRd
meds_PDE5i <- "
  sildenafil
  tadalafil
  vardenafil
"

#' @noRd
meds_betablocker <- "
  nadolol
  pindolol
  metoprolol tartrate+chlorthalidone 100mg/12.5mg tablet
  nadolol+bendrofluazide 40mg/5mg tablet
  nadolol+bendrofluazide 80mg/5mg tablet
  penbutolol sulphate+frusemide 40mg/20mg tablet
  pindolol+clopamide 10mg/5mg tablet
  timolol maleate+co-amilozide 10mg/2.5mg/25mg tablet
  timolol maleate+bendrofluazide 10mg/2.5mg tablet
  timolol maleate+bendrofluazide 20mg/5mg tablet
  metoprolol tartrate+hydrochlorothiazide 100mg/12.5mg tablet
  propranolol hydrochloride+bendrofluazide 80mg/2.5mg capsule
  acebutolol+hydrochlorothiazide 200mg/12.5mg tablet
  atenolol+nifedipine 50mg/20mg m/r capsule
  bisoprolol fumarate+hydrochlorothiazide 10mg/6.25mg tablet
  acebutolol
  atenolol
  apsolol 10mg tablet
  berkolol 10mg tablet
  bisoprolol
  metoprolol
  oxprenolol
  penbutolol
  propranolol
  timolol
  prindolol
  sloprolol 80mg m/r capsule
  atenolol+chlorthalidone
  atenolol+bendrofluazide
  atenolol+co-amilozide
  nebivolol
  atenolol+chlortalidone
  nadolol+bendroflumethiazide 40mg/5mg tablet
  timolol maleate+bendroflumethiazide 10mg/2.5mg tablet
  atenolol+bendroflumethiazide
  carvedilol
  labetalol
  angilol 10mg tablet
  sotalol hydrochloride+hydrochlorothiazide 80mg/12.5mg tablet
  sotalol
"

#' @noRd
meds_ACEi <- "
  lisinopril
  quinapril
  quinalapril+hydrochlorothiazide 10mg/12.5mg tablet
  captopril
  acepril 12.5mg tablet
  captopril+hydrochlorothiazide 25mg/12.5mg tablet
  enalapril maleate+hydrochlorothiazide 20mg/12.5mg tablet
  ramipril
  cilazapril
  trandolapril
  lisinopril+hydrochlorothiazide 10mg/12.5mg tablet
  enalapril
  fosinopril
  perindopril
  moexipril
  hypapril 12.5mg tablet
  trandolapril+verapamil hydrochloride
  imidapril hydrochloride
  felodipine+ramipril
  tensopril 12.5mg tablet
  perindopril+indapamide
"

#' @noRd
meds_ARB <- "
  losartan
  valsartan
  losartan potassium+hydrochlorothiazide 50mg/12.5mg tablet
  irbesartan
  candesartan cilexetil
  telmisartan
  eprosartan
  irbesartan+hydrochlorothiazide 150mg/12.5mg tablet
  telmisartan+hydrochlorothiazide 40mg/12.5mg tablet
  olmesartan
  valsartan+hydrochlorothiazide 80mg/12.5mg tablet
"

#' @noRd
meds_CCB <- "
  atenolol+nifedipine 50mg/20mg m/r capsule
  nifedipine
  isradipine
  lacidipine
  nimodipine
  amlodipine
  nicardipine
  felodipine
  adipine mr 10 m/r tablet
  nisoldipine
  slofedipine 20mg m/r tablet
  lercanidipine
  felodipine+ramipril
  diltiazem
  kentiazem 60mg m/r capsule
  diltiazem hcl+hydrochlorothiazide 150mg/12.5mg m/r capsule
  viazem xl 120mg m/r capsule
  verapamil
  trandolapril+verapamil hydrochloride
"

#' @noRd
meds_loopdiuretic <- "
  penbutolol sulphate+frusemide 40mg/20mg tablet
  frusemide
  triamterene+frusemide 50mg/40mg tablet
  frusemide+potassium 20mg/10mmol m/r tablet
  torasemide
  furosemide
  triamterene+furosemide 50mg/40mg tablet
  furosemide+potassium 20mg/10mmol m/r tablet
  bumetanide
  amiloride hydrochloride+bumetanide 5mg/1mg tablet
  bumetanide+potassium 500micrograms/7.7mmol m/r tablet
  ethacrynic acid
  ethacrynic acid 50mg tablet
  ethacrynate sodium
  sodium ethacrynate
  ethacrynic acid product
"

#' @noRd
meds_thiazide <- "
  sotalol hydrochloride+hydrochlorothiazide 80mg/12.5mg tablet
  metoprolol tartrate+hydrochlorothiazide 100mg/12.5mg tablet
  acebutolol+hydrochlorothiazide 200mg/12.5mg tablet
  methyldopa+hydrochlorothiazide 250mg/15mg tablet
  quinalapril+hydrochlorothiazide 10mg/12.5mg tablet
  captopril+hydrochlorothiazide 25mg/12.5mg tablet
  enalapril maleate+hydrochlorothiazide 20mg/12.5mg tablet
  bisoprolol fumarate+hydrochlorothiazide 10mg/6.25mg tablet
  lisinopril+hydrochlorothiazide 10mg/12.5mg tablet
  hydroflumethiazide
  methyclothiazide
  polythiazide
  chlorothiazide
  cyclopenthiazide
  hydrochlorothiazide
  triamterene+benzthiazide 50mg/25mg capsule
  amiloride hcl+cyclopenthiazide 2.5mg/250micrograms tablet
  diltiazem hcl+hydrochlorothiazide 150mg/12.5mg m/r capsule
  losartan potassium+hydrochlorothiazide 50mg/12.5mg tablet
  irbesartan+hydrochlorothiazide 150mg/12.5mg tablet
  telmisartan+hydrochlorothiazide 40mg/12.5mg tablet
  bendroflumethiazide
  bendroflumethiazide+potassium 2.5mg/7.7mmol m/r tablet
  nadolol+bendroflumethiazide 40mg/5mg tablet
  timolol maleate+bendroflumethiazide 10mg/2.5mg tablet
  atenolol+bendroflumethiazide
  valsartan+hydrochlorothiazide 80mg/12.5mg tablet
"

#' @noRd
meds_antiHTN_other <- "
  hydralazine
  isosorbide mononitrate
  isosorbide dinitrate
  isdn - isosorbide dinitrate
  ismn - isosorbide mononitrate
  ismo - isosorbide mononitrate
  isosorbide mononitrate product
  isosorbide mononitrate+aspirin
"

#' @noRd
meds_insulin <- "
  insulin product
"

#' @noRd
meds_OHA <- "
  glipizide
  glipizide product
  glimepiride
  metformin
  rosiglitazone 1mg / metformin 500mg tablet
  troglitazone
  pioglitazone
  rosiglitazone
  repaglinide
  nateglinide
  acarbose
"

#' @noRd
meds_pulm_endothelin <- "
  bosentan
"

#' @rdname medication_lookup_by_disease
#' @export
medication_statin <- function(data, up_to_instance = 3, medication_field = f.20003.0.0.Treatment_medication_code) {
  codes <- med_text_to_codings(meds_statin)
  medication_lookup(data, codes = codes, up_to_instance = {{ up_to_instance }}, medication_field = {{ medication_field }})
}

#' @rdname medication_lookup_by_disease
#' @export
medication_antiHLD <- function(data, up_to_instance = 3, medication_field = f.20003.0.0.Treatment_medication_code) {
  codes <- c(
    med_text_to_codings(meds_statin),
    med_text_to_codings(meds_antiHLD_other)
  )
  medication_lookup(data, codes = codes, up_to_instance = {{ up_to_instance }}, medication_field = {{ medication_field }})
}

#' @rdname medication_lookup_by_disease
#' @export
medication_PDE5i <- function(data, up_to_instance = 3, medication_field = f.20003.0.0.Treatment_medication_code) {
  codes <- med_text_to_codings(meds_PDE5i)
  medication_lookup(data, codes = codes, up_to_instance = {{ up_to_instance }}, medication_field = {{ medication_field }})
}

#' @rdname medication_lookup_by_disease
#' @export
medication_BB <- function(data, up_to_instance = 3, medication_field = f.20003.0.0.Treatment_medication_code) {
  codes <- med_text_to_codings(meds_betablocker)
  medication_lookup(data, codes = codes, up_to_instance = {{ up_to_instance }}, medication_field = {{ medication_field }})
}

#' @rdname medication_lookup_by_disease
#' @export
medication_ACEi <- function(data, up_to_instance = 3, medication_field = f.20003.0.0.Treatment_medication_code) {
  codes <- med_text_to_codings(meds_ACEi)
  medication_lookup(data, codes = codes, up_to_instance = {{ up_to_instance }}, medication_field = {{ medication_field }})
}

#' @rdname medication_lookup_by_disease
#' @export
medication_ARB <- function(data, up_to_instance = 3, medication_field = f.20003.0.0.Treatment_medication_code) {
  codes <- med_text_to_codings(meds_ARB)
  medication_lookup(data, codes = codes, up_to_instance = {{ up_to_instance }}, medication_field = {{ medication_field }})
}

#' @rdname medication_lookup_by_disease
#' @export
medication_CCB <- function(data, up_to_instance = 3, medication_field = f.20003.0.0.Treatment_medication_code) {
  codes <- med_text_to_codings(meds_CCB)
  medication_lookup(data, codes = codes, up_to_instance = {{ up_to_instance }}, medication_field = {{ medication_field }})
}

#' @rdname medication_lookup_by_disease
#' @export
medication_loopdiuretic <- function(data, up_to_instance = 3, medication_field = f.20003.0.0.Treatment_medication_code) {
  codes <- med_text_to_codings(meds_loopdiuretic)
  medication_lookup(data, codes = codes, up_to_instance = {{ up_to_instance }}, medication_field = {{ medication_field }})
}

#' @rdname medication_lookup_by_disease
#' @export
medication_thiazide <- function(data, up_to_instance = 3, medication_field = f.20003.0.0.Treatment_medication_code) {
  codes <- med_text_to_codings(meds_thiazide)
  medication_lookup(data, codes = codes, up_to_instance = {{ up_to_instance }}, medication_field = {{ medication_field }})
}

#' @rdname medication_lookup_by_disease
#' @export
medication_antiHTN <- function(data, up_to_instance = 3, medication_field = f.20003.0.0.Treatment_medication_code) {
  codes <- c(
    med_text_to_codings(meds_antiHTN_other),
    med_text_to_codings(meds_ACEi),
    med_text_to_codings(meds_ARB),
    med_text_to_codings(meds_CCB),
    med_text_to_codings(meds_betablocker),
    med_text_to_codings(meds_thiazide)
  )
  medication_lookup(data, codes = codes, up_to_instance = {{ up_to_instance }}, medication_field = {{ medication_field }})
}

#' @rdname medication_lookup_by_disease
#' @export
medication_insulin <- function(data, up_to_instance = 3, medication_field = f.20003.0.0.Treatment_medication_code) {
  codes <- med_text_to_codings(meds_insulin)
  medication_lookup(data, codes = codes, up_to_instance = {{ up_to_instance }}, medication_field = {{ medication_field }})
}

#' @rdname medication_lookup_by_disease
#' @export
medication_oralhypoglycemic <- function(data, up_to_instance = 3, medication_field = f.20003.0.0.Treatment_medication_code) {
  codes <- med_text_to_codings(meds_OHA)
  medication_lookup(data, codes = codes, up_to_instance = {{ up_to_instance }}, medication_field = {{ medication_field }})
}

#' @rdname medication_lookup_by_disease
#' @export
medication_pulm_endothelin <- function(data, up_to_instance = 3, medication_field = f.20003.0.0.Treatment_medication_code) {
  codes <- med_text_to_codings(meds_pulm_endothelin)
  medication_lookup(data, codes = codes, up_to_instance = {{ up_to_instance }}, medication_field = {{ medication_field }})
}
