#' @title Biomarker lookups
#' @description Perform lookup of biomarkers.
#' @seealso [measurement_lookup()]
#' @inheritParams ukbiobank
#' @param biomarker_col Template field (i.e. the column name) used to look up biomarker values, e.g. `f.30750.0.0.Glycated_haemoglobin_HbA1c`
#' @inheritSection ukbiobank Instancing
#' @inheritSection ukbiobank Column names
#' @export
biomarker_lookup <- function(data,
                             biomarker_col,
                             combine_instances = c("last", "first", "min", "max", "mean"),
                             after_instance = default_after_inst(),
                             up_to_instance = default_up_to_inst()) {
  combine_instances <- match.arg(combine_instances)

  # remove any columns with only NAs
  data %<>% remove_na_columns()

  biomarker_by_instance <- function(i) {
    # this should only extract one column:
    biomarker_colnames <- select_instance_and_array(data,
      {{ biomarker_col }},
      instance = i,
      array = 0
    )
    stopifnot(length(biomarker_colnames) <= 1)

    if (length(biomarker_colnames) > 0) {
      data %>%
        select(!!!biomarker_colnames) %>%
        pull()
    } else {
      NA
    }
  }

  instance_combiner(
    data,
    lookup_by_instance_fn = biomarker_by_instance,
    after_instance = {{ after_instance }},
    up_to_instance = {{ up_to_instance }},
    combine_instances = combine_instances
  )
}

#' @rdname biomarker_lookup
#' @export
biomarker_A1c_percent <- function(data,
                                  biomarker_col = f.30750.0.0.Glycated_haemoglobin_HbA1c,
                                  after_instance = default_after_inst(),
                                  up_to_instance = default_up_to_inst(),
                                  combine_instances = c("last", "first", "min", "max", "mean")) {
  v <- biomarker_lookup(data, biomarker_col = {{ biomarker_col }}, after_instance = {{ after_instance }}, up_to_instance = {{ up_to_instance }}, combine_instances = combine_instances)

  # convert from mmol/mol to %
  (v / 10.929) + 2.15
}

#' @rdname biomarker_lookup
#' @export
biomarker_LDL_mgdL <- function(data,
                               biomarker_col = f.30780.0.0.LDL_direct,
                               after_instance = default_after_inst(),
                               up_to_instance = default_up_to_inst(),
                               combine_instances = c("last", "first", "min", "max", "mean")) {
  v <- biomarker_lookup(data, biomarker_col = {{ biomarker_col }}, after_instance = {{ after_instance }}, up_to_instance = {{ up_to_instance }}, combine_instances = combine_instances)

  # convert from mmol/L to mg/dL
  v * 38.67
}

#' @rdname biomarker_lookup
#' @export
biomarker_SCr_mgdL <- function(data,
                               biomarker_col = f.30700.0.0.Creatinine,
                               after_instance = default_after_inst(),
                               up_to_instance = default_up_to_inst(),
                               combine_instances = c("last", "first", "min", "max", "mean")) {
  v <- biomarker_lookup(data, biomarker_col = {{ biomarker_col }}, after_instance = {{ after_instance }}, up_to_instance = {{ up_to_instance }}, combine_instances = combine_instances)

  # convert from umol/L to mg/dL
  v / 88.4
}