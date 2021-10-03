#' Age at instance
#'
#' @param data input data
#' @param instance_column name of column containing instance numbers
#' @param .age_at_assessment_col age at assessment
#'
#' @export
Age_at_instance <- function(data,
                            instance_column,
                            .age_at_assessment_col = f.21003.0.0.Age_when_attended_assessment_centre)
{
  stopifnot(!is_missing(instance_column))
  age_at_assessment_cols <- expand_instances( {{ .age_at_assessment_col }} )

  data %>%
    mutate(
      Age_at_Instance = case_when(
        {{instance_column}} == 0 ~ !!age_at_assessment_cols[[1]],
        {{instance_column}} == 1 ~ !!age_at_assessment_cols[[2]],
        {{instance_column}} == 2 ~ !!age_at_assessment_cols[[3]],
        {{instance_column}} == 3 ~ !!age_at_assessment_cols[[4]],
      )
    ) %>%
    pull()
}


#' @rdname Age_at_instance
#' @export
Age_group_at_instance <- function(data,
                                  instance_column,
                                  .age_at_assessment_col = f.21003.0.0.Age_when_attended_assessment_centre)
{
  Age_at_instance(data, {{ instance_column }}, .age_at_assessment_col = {{ .age_at_assessment_col }}) %>%
    Age_group()
}
