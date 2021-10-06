#' Age at date
#'
#' Get age (in years) as of a particular date. The alternate function, **`age_group_at_date()`**, returns a string describing the age group in decades (e.g. "20-29") for a 24-year-old patient.
#'
#' @inheritParams ukbiobank
#' @param .year_of_birth <[`data-masking`][dplyr_data_masking]> Name of column containing birth year
#' @param .month_of_birth <[`data-masking`][dplyr_data_masking]> Name of column containing birth month
#'
#' @return Numberic vector of age in years (`age_at_date`), or character vector of age groups (`age_group_at_date`)
#'
#' @export
age_at_date <- function(data,
                        date,
                        .year_of_birth = f.34.0.0.Year_of_birth,
                        .month_of_birth = f.52.0.0.Month_of_birth)
{
  target_date <- lubridate::as_date(date)
  birth_date <- data %>%
    mutate(birthString = lubridate::make_date({{.year_of_birth}}, {{.month_of_birth}}, 15)) %>%
    pull()
  lubridate::interval(birth_date, target_date) %>%
    as.numeric('years')
}

#' @rdname age_at_date
#' @export
age_group_at_date <- function(data,
                              date,
                              .year_of_birth = f.34.0.0.Year_of_birth,
                              .month_of_birth = f.52.0.0.Month_of_birth)
{
  age_at_date(data, date, .year_of_birth = {{.year_of_birth}}, .month_of_birth = {{.month_of_birth}}) %>%
    age_group()
}
