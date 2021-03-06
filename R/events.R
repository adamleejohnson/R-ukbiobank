#' @title Death status (true/false)
#' @description Returns whether a death event has been reported for each participant.
#' @seealso [years_from_date_to_death()], [years_from_instance_to_death()]
#' @inheritParams ukbiobank
#' @inheritSection ukbiobank Column names
#' @export
death_status <- function(data, death_date_col = f.40000.0.0.Date_of_death) {
  data %>%
    pull({{ death_date_col }}) %>%
    is.na(.) %>%
    magrittr::not(.)
}

#' @title Years from date to death
#' @inheritParams ukbiobank
#' @inheritSection ukbiobank Column names
#' @export
years_from_date_to_death <- function(data,
                                     date,
                                     death_date_col = f.40000.0.0.Date_of_death) {
  initial_date <- lubridate::as_date(date)
  death_date <- data %>%
    pull({{ death_date_col }}) %>%
    lubridate::as_date()

  lubridate::interval(initial_date, death_date) %>%
    as.numeric("years")
}

#' @title Years from instance to death
#' @inheritParams ukbiobank
#' @inheritSection ukbiobank Column names
#' @export
years_from_instance_to_death <- function(data,
                                         instance_num,
                                         date_of_instance_col = f.53.0.0.Date_of_attending_assessment_centre,
                                         death_date_col = f.40000.0.0.Date_of_death) {
  instance_date_cols <- expand_instances(data, {{ date_of_instance_col }})

  initial_date <-
    data %>%
    mutate(
      Date_at_Instance = case_when(
        {{ instance_num }} == 0 ~ !!instance_date_cols[[1]],
        {{ instance_num }} == 1 ~ !!instance_date_cols[[2]],
        {{ instance_num }} == 2 ~ !!instance_date_cols[[3]],
        {{ instance_num }} == 3 ~ !!instance_date_cols[[4]],
      )
    ) %>%
    pull() %>%
    lubridate::as_date()

  death_date <- data %>%
    pull({{ death_date_col }}) %>%
    lubridate::as_date()

  lubridate::interval(initial_date, death_date) %>%
    as.numeric("years")
}