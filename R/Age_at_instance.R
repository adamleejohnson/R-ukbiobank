#' Age at instance
#'
#' Age, in years, at the time of the specific instance.
#'
#' @inheritParams ukbiobank
#' @param .age_at_assessment <[`data-masking`][dplyr_data_masking]> Name of column containing age at instance
#'
#' @export
age_at_instance <- function(data,
                            instance_num,
                            .age_at_assessment = f.21003.0.0.Age_when_attended_assessment_centre)
{
  age_at_assessment_cols <- expand_instances(data, {{ .age_at_assessment }} )

  data %>%
    mutate(
      Age_at_Instance = case_when(
        {{instance_num}} == 0 ~ !!age_at_assessment_cols[[1]],
        {{instance_num}} == 1 ~ !!age_at_assessment_cols[[2]],
        {{instance_num}} == 2 ~ !!age_at_assessment_cols[[3]],
        {{instance_num}} == 3 ~ !!age_at_assessment_cols[[4]],
      )
    ) %>%
    pull()
}


#' @rdname age_at_instance
#' @export
age_group_at_instance <- function(data,
                                  instance_num,
                                  .age_at_assessment = f.21003.0.0.Age_when_attended_assessment_centre)
{
  age_at_instance(data, {{ instance_num }}, .age_at_assessment = {{ .age_at_assessment }}) %>%
    age_group()
}
