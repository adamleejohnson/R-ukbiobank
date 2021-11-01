#' General measurement lookups
#'
#' @inheritParams ukbiobank
#' @param measurement_field Template field (i.e. the column name) used to look up biomarker values, e.g. `f.4080.0.0.Systolic_blood_pressure_automated_reading`
#' @param combine_array If a measurement field has multiple array values (e.g. blood pressure recordings are made in duplicate at each instance), specify how these values should be combined. See `combine_instances` for details.
#'
#' @export
measurement_lookup <- function(data,
                               measurement_field,
                               combine_instances = c("last", "first", "min", "max", "mean"),
                               combine_array = c("last", "first", "min", "max", "mean"),
                               after_instance = DEFAULT_AFTER_INST,
                               up_to_instance = DEFAULT_UP_TO_INST) {
  combine_instances <- match.arg(combine_instances)
  combine_array <- match.arg(combine_array)

  # remove any columns with only NAs
  data %<>% remove_na_columns()

  array_reduce_fn <- get_reduce_fn(combine_array)

  measurement_by_instance <- function(i) {
    measurement_colnames <-
      select_instance_and_expand_array(
        data,
        {{ measurement_field }},
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
physio_systolicBP <- function(data,
                              measurement_field = f.4080.0.0.Systolic_blood_pressure_automated_reading,
                              after_instance = DEFAULT_AFTER_INST,
                              up_to_instance = DEFAULT_UP_TO_INST,
                              combine_instances = "last",
                              combine_array = "mean") {
  measurement_lookup(
    data,
    measurement_field = {{ measurement_field }},
    after_instance = {{ after_instance }},
    up_to_instance = {{ up_to_instance }},
    combine_instances = combine_instances,
    combine_array = combine_array
  )
}

#' @rdname measurement_lookup
#' @export
physio_diastolicBP <- function(data,
                               measurement_field = f.4079.0.0.Diastolic_blood_pressure_automated_reading,
                               after_instance = DEFAULT_AFTER_INST,
                               up_to_instance = DEFAULT_UP_TO_INST,
                               combine_instances = "last",
                               combine_array = "mean") {
  measurement_lookup(
    data,
    measurement_field = {{ measurement_field }},
    after_instance = {{ after_instance }},
    up_to_instance = {{ up_to_instance }},
    combine_instances = combine_instances,
    combine_array = combine_array
  )
}

#' @rdname measurement_lookup
#' @export
physio_height_cm <- function(data,
                             measurement_field = f.50.0.0.Standing_height,
                             after_instance = DEFAULT_AFTER_INST,
                             up_to_instance = DEFAULT_UP_TO_INST,
                             combine_instances = "last",
                             combine_array = "mean") {
  measurement_lookup(
    data,
    measurement_field = {{ measurement_field }},
    after_instance = {{ after_instance }},
    up_to_instance = {{ up_to_instance }},
    combine_instances = combine_instances,
    combine_array = combine_array
  )
}

#' @rdname measurement_lookup
#' @export
physio_weight_kg <- function(data,
                             measurement_field = f.21002.0.0.Weight,
                             after_instance = DEFAULT_AFTER_INST,
                             up_to_instance = DEFAULT_UP_TO_INST,
                             combine_instances = "last",
                             combine_array = "mean") {
  measurement_lookup(
    data,
    measurement_field = {{ measurement_field }},
    after_instance = {{ after_instance }},
    up_to_instance = {{ up_to_instance }},
    combine_instances = combine_instances,
    combine_array = combine_array
  )
}