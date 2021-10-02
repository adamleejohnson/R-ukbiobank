#' Age at instance
#'
#' @param data input data
#' @param instance_col name of column containing instance numbers
#' @param .id_col id column
#' @param .age_at_assessment_col age at assessment
#' @param .date_at_assessment_col date at assessment
#' @param .year_of_birth year of birth
#' @param .month_of_birth month of birth
#'
#' @export
Age_at_instance <- function(data,
                            instance_col,
                            .id_col = f.eid,
                            .age_at_assessment_col = f.21003.0.0.Age_when_attended_assessment_centre,
                            .date_at_assessment_col = f.53.0.0.Date_of_attending_assessment_centre,
                            .year_of_birth = 	f.34.0.0.Year_of_birth,
                            .month_of_birth = f.52.0.0.Month_of_birth)
{
  age_at_assessment_cols <- expand_instances( {{ .age_at_assessment_col }} )

  data %>%
    mutate(
      Age_at_Instance = case_when(
        {{instance_col}} == 0 ~ !!age_at_assessment_cols[[1]],
        {{instance_col}} == 1 ~ !!age_at_assessment_cols[[2]],
        {{instance_col}} == 2 ~ !!age_at_assessment_cols[[3]],
        {{instance_col}} == 3 ~ !!age_at_assessment_cols[[4]],
      )
    ) %>%
    pull()
    # select({{.id_col}}, {{instance_col}}, Age_at_Instance)
}
