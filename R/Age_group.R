#' Age group from vector of ages
#' @param age Input vector of ages
#' @export
age_group <- function(age) {
  tibble(age) %>%
  mutate(ageGroup = case_when(
    age < 10                ~ "0-9",
    age >= 10 & age < 20    ~ "10-19",
    age >= 20 & age < 30    ~ "20-29",
    age >= 30 & age < 40    ~ "30-39",
    age >= 40 & age < 50    ~ "40-49",
    age >= 50 & age < 60    ~ "50-59",
    age >= 60 & age < 70    ~ "60-69",
    age >= 70 & age < 80    ~ "70-79",
    age >= 80 & age < 90    ~ "80-89",
    age >= 90 & age < 100   ~ "90-99",
    age >= 100              ~ "100+"
  )) %>%
  pull()
}
