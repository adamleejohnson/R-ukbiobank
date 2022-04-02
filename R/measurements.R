#' @title General measurement lookups
#' @description Perform lookups for various measurements. Because many measurements are repeated across instances, and sometimes within instances, two special arguments (`combine_instances` and `combine_array`) are used to specify how multiple measurements should be aggregated.
#'
#' The `measurement_lookup()` function is the general function for lookups. The `measurement_lookup_with_alt()` function is similar, but also looks for an alternate value if the primary value is NA. This is common when manual measurements (of, for example, blood pressure) are made after an automated method fails.
#'
#' Several convenience `physio_*()` functions are provided to facilitate lookup of commonly used measurements (e.g., blood pressure, BMI, etc.).
#'
#' @seealso [biomarker_lookup()]
#' @inheritParams ukbiobank
#' @inheritSection ukbiobank Instancing
#' @inheritSection ukbiobank Column names
#' @export
measurement_lookup <- function(data,
                               measurement_col,
                               after_instance = default_after_inst(),
                               up_to_instance = default_up_to_inst(),
                               combine_instances = c("last", "first", "min", "max", "mean"),
                               combine_array = c("last", "first", "min", "max", "mean")) {
  combine_instances <- match.arg(combine_instances)
  combine_array <- match.arg(combine_array)
  data %<>% remove_na_columns()
  array_reduce_fn <- get_reduce_fn(combine_array)

  measurement_by_instance <- function(i) {
    measurement_colnames <-
      select_instance_and_expand_array(
        data,
        {{ measurement_col }},
        instance = i
      )

    data %>%
      select(!!!measurement_colnames) %>%
      reduce_by_row(array_reduce_fn)
  }

  instance_combiner(
    data,
    lookup_by_instance_fn = measurement_by_instance,
    after_instance = {{ after_instance }},
    up_to_instance = {{ up_to_instance }},
    combine_instances = combine_instances
  )
}

#' @rdname measurement_lookup
#' @export
measurement_lookup_with_alt <- function(data,
                                        measurement_col,
                                        measurement_col_alt,
                                        after_instance = default_after_inst(),
                                        up_to_instance = default_up_to_inst(),
                                        combine_instances = c("last", "first", "min", "max", "mean"),
                                        combine_array = c("last", "first", "min", "max", "mean")) {
  combine_instances <- match.arg(combine_instances)
  combine_array <- match.arg(combine_array)
  data %<>% remove_na_columns()
  array_reduce_fn <- get_reduce_fn(combine_array)

  measurement_by_instance <- function(i) {
    measurement_colnames <-
      select_instance_and_expand_array(
        data,
        {{ measurement_col }},
        instance = i
      )
    measurement_colnames_alt <-
      select_instance_and_expand_array(
        data,
        {{ measurement_col_alt }},
        instance = i
      )

    primary <- data %>%
      select(!!!measurement_colnames) %>%
      reduce_by_row(array_reduce_fn)
    primary <- primary %||% NA
    alt <- data %>%
      select(!!!measurement_colnames_alt) %>%
      reduce_by_row(array_reduce_fn)
    alt <- alt %||% NA
    array_reduce_fn(primary, alt)
  }

  instance_combiner(
    data,
    lookup_by_instance_fn = measurement_by_instance,
    after_instance = {{ after_instance }},
    up_to_instance = {{ up_to_instance }},
    combine_instances = combine_instances
  )
}

# ===================================================================================================================

#' @rdname measurement_lookup
#' @export
physio_systolicBP <- function(data,
                              after_instance = default_after_inst(),
                              up_to_instance = default_up_to_inst(),
                              combine_instances = "last",
                              combine_array = "mean",
                              measurement_col = f.4080.0.0.Systolic_blood_pressure_automated_reading,
                              measurement_col_alt = f.93.0.0.Systolic_blood_pressure_manual_reading) {
  measurement_lookup_with_alt(
    data,
    measurement_col = {{ measurement_col }},
    measurement_col_alt = {{ measurement_col_alt }},
    after_instance = {{ after_instance }},
    up_to_instance = {{ up_to_instance }},
    combine_instances = combine_instances,
    combine_array = combine_array
  )
}

#' @rdname measurement_lookup
#' @export
physio_diastolicBP <- function(data,
                               after_instance = default_after_inst(),
                               up_to_instance = default_up_to_inst(),
                               combine_instances = "last",
                               combine_array = "mean",
                               measurement_col = f.4079.0.0.Diastolic_blood_pressure_automated_reading,
                               measurement_col_alt = f.94.0.0.Diastolic_blood_pressure_manual_reading) {
  measurement_lookup_with_alt(
    data,
    measurement_col = {{ measurement_col }},
    measurement_col_alt = {{ measurement_col_alt }},
    after_instance = {{ after_instance }},
    up_to_instance = {{ up_to_instance }},
    combine_instances = combine_instances,
    combine_array = combine_array
  )
}

#' @rdname measurement_lookup
#' @export
physio_height_cm <- function(data,
                             after_instance = default_after_inst(),
                             up_to_instance = default_up_to_inst(),
                             combine_instances = "last",
                             combine_array = "mean",
                             measurement_col = f.50.0.0.Standing_height) {
  measurement_lookup(
    data,
    measurement_col = {{ measurement_col }},
    after_instance = {{ after_instance }},
    up_to_instance = {{ up_to_instance }},
    combine_instances = combine_instances,
    combine_array = combine_array
  )
}

#' @rdname measurement_lookup
#' @export
physio_weight_kg <- function(data,
                             after_instance = default_after_inst(),
                             up_to_instance = default_up_to_inst(),
                             combine_instances = "last",
                             combine_array = "mean",
                             measurement_col = f.21002.0.0.Weight) {
  measurement_lookup(
    data,
    measurement_col = {{ measurement_col }},
    after_instance = {{ after_instance }},
    up_to_instance = {{ up_to_instance }},
    combine_instances = combine_instances,
    combine_array = combine_array
  )
}

#' @rdname measurement_lookup
#' @export
physio_bmi <- function(data,
                       measurement_col = f.21001.0.0.Body_mass_index_BMI,
                       after_instance = default_after_inst(),
                       up_to_instance = default_up_to_inst(),
                       combine_instances = "last",
                       combine_array = "mean") {
  measurement_lookup(
    data,
    measurement_col = {{ measurement_col }},
    after_instance = {{ after_instance }},
    up_to_instance = {{ up_to_instance }},
    combine_instances = combine_instances,
    combine_array = combine_array
  )
}

#' @rdname measurement_lookup
#' @export
physio_bsa <- function(data,
                       after_instance = default_after_inst(),
                       up_to_instance = default_up_to_inst(),
                       combine_instances = "last",
                       combine_array = "mean",
                       height_col = f.50.0.0.Standing_height,
                       weight_col = f.21002.0.0.Weight) {
  height <- physio_height_cm(
    data,
    measurement_col = {{ height_col }},
    after_instance = {{ after_instance }},
    up_to_instance = {{ up_to_instance }},
    combine_instances = combine_instances,
    combine_array = combine_array
  )
  weight <- physio_weight_kg(
    data,
    measurement_col = {{ weight_col }},
    after_instance = {{ after_instance }},
    up_to_instance = {{ up_to_instance }},
    combine_instances = combine_instances,
    combine_array = combine_array
  )
  0.20247 * (weight^0.425) * ((height / 100)^0.725)
}
