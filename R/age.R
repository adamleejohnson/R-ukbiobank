#' Age group from vector of ages
#' @param age Input vector of ages
#' @export
convert_to_age_group <- function(age) {
  stopifnot(is.numeric(age))
  case_when(
    age < 10 ~ "0-9",
    age >= 10 & age < 20 ~ "10-19",
    age >= 20 & age < 30 ~ "20-29",
    age >= 30 & age < 40 ~ "30-39",
    age >= 40 & age < 50 ~ "40-49",
    age >= 50 & age < 60 ~ "50-59",
    age >= 60 & age < 70 ~ "60-69",
    age >= 70 & age < 80 ~ "70-79",
    age >= 80 & age < 90 ~ "80-89",
    age >= 90 & age < 100 ~ "90-99",
    age >= 100 ~ "100+"
  )
}

#' Age at date
#'
#' Get age (in years) as of a particular date. The alternate function, **`age_group_at_date()`**, returns a string describing the age group in decades (e.g. "20-29") for a 24-year-old patient.
#'
#' NOTE: This result is approximate, since only the year and month of birth are provided. The day of birth is assumed to be 15.
#'
#' @inheritParams ukbiobank
#' @return Numeric vector of age in years (`age_at_date()`), or character vector of age groups (`age_group_at_date()`)
#' @export
age_at_date <- function(data,
                        date,
                        year_of_birth_field = f.34.0.0.Year_of_birth,
                        month_of_birth_field = f.52.0.0.Month_of_birth) {
  data %>%
    mutate(
      birth_date = lubridate::make_date({{ year_of_birth_field }}, {{ month_of_birth_field }}, 15),
      target_date = lubridate::as_date(as.character({{ date }})),
      diff = lubridate::interval(birth_date, target_date)
    ) %>%
    pull(diff) %>%
    as.numeric("years")
}

#' Age at instance
#'
#' Age, in years, at the time of the specific instance.
#'
#' `age_at_instance` uses the coded fields that contain the age (rounded to the nearest year) at each instance visit.
#' `age_at_instance_computed` calculates a slightly more precise age based on the partipants year and month of birth.
#'
#' @inheritParams ukbiobank
#' @export
age_at_instance <- function(data,
                            instance_num,
                            age_at_instance_field = f.21003.0.0.Age_when_attended_assessment_centre) {
  age_at_instance_field_cols <- expand_instances(data, {{ age_at_instance_field }})

  data %>%
    mutate(
      case_when(
        {{ instance_num }} == 0 ~ !!age_at_instance_field_cols[[1]],
        {{ instance_num }} == 1 ~ !!age_at_instance_field_cols[[2]],
        {{ instance_num }} == 2 ~ !!age_at_instance_field_cols[[3]],
        {{ instance_num }} == 3 ~ !!age_at_instance_field_cols[[4]],
      )
    ) %>%
    pull()
}

#' @rdname age_at_instance
#' @export
age_at_instance_computed <- function(data,
                            instance_num,
                            date_of_instance_field = f.53.0.0.Date_of_attending_assessment_centre,
                            year_of_birth_field = f.34.0.0.Year_of_birth,
                            month_of_birth_field = f.52.0.0.Month_of_birth) {

  date_of_instance_field_cols <- expand_instances(data, {{ date_of_instance_field }})

  data %>%
    mutate(
      birth_date = lubridate::make_date({{ year_of_birth_field }}, {{ month_of_birth_field }}, 15),
      inst_date = case_when(
        {{ instance_num }} == 0 ~ !!date_of_instance_field_cols[[1]],
        {{ instance_num }} == 1 ~ !!date_of_instance_field_cols[[2]],
        {{ instance_num }} == 2 ~ !!date_of_instance_field_cols[[3]],
        {{ instance_num }} == 3 ~ !!date_of_instance_field_cols[[4]],
      ),
      diff = lubridate::interval(birth_date, inst_date)
    ) %>%
    pull(diff) %>%
    as.numeric("years")
}
