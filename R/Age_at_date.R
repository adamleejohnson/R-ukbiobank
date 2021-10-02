#' Age at date
#'
#' @param data input data
#' @param date date string
#' @param .year_of_birth year of birth column
#' @param .month_of_birth month of birth column
#'
#' @import lubridate
#'
#' @export
Age_at_date <- function(data,
                        date = Sys.Date(),
                        .year_of_birth = f.34.0.0.Year_of_birth,
                        .month_of_birth = f.52.0.0.Month_of_birth)
{
  target_date <- lubridate::as_date(date)
  birth_date <- data %>%
    mutate(birthString = lubridate::make_date({{.year_of_birth}}, {{.month_of_birth}}, 1)) %>%
    pull()
  lubridate::interval(birth_date, target_date) %>%
    as.numeric('years')
}
