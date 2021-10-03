#' Death status
#' @param data input data
#' @param .death_date_field death field
#' @export
Death_status <- function(data, .death_date_field = f.40000.0.0.Date_of_death) {
  data %>%
    pull({{.death_date_field}}) %>%
    is.na() %>%
    not()
}

#' @export
Years_from_date_to_death <- function(data, date, .death_date_field = f.40000.0.0.Date_of_death) {
  initial_date <- lubridate::as_date(date)
  death_date <- data %>%
    pull({{.death_date_field}}) %>%
    lubridate::as_date()

  lubridate::interval(initial_date, death_date) %>%
    as.numeric('years')
}

#' @export
Years_from_instance_to_death <- function(data,
                                         instance_column,
                                         .instance_date_field = f.53.0.0.Date_of_attending_assessment_centre,
                                         .death_date_field = f.40000.0.0.Date_of_death) {

  instance_date_cols <- expand_instances(data, {{ .instance_date_field }} )

  initial_date <-
    data %>%
    mutate(
      Date_at_Instance = case_when(
        {{instance_column}} == 0 ~ !!instance_date_cols[[1]],
        {{instance_column}} == 1 ~ !!instance_date_cols[[2]],
        {{instance_column}} == 2 ~ !!instance_date_cols[[3]],
        {{instance_column}} == 3 ~ !!instance_date_cols[[4]],
      )
    ) %>%
    pull() %>%
    lubridate::as_date()

  death_date <- data %>%
    pull({{.death_date_field}}) %>%
    lubridate::as_date()

  lubridate::interval(initial_date, death_date) %>%
    as.numeric('years')
}
