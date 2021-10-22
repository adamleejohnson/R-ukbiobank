#' @name ukbiobank
#' @title ukbiobank
#' @description This package contains a collection of tools and functions to facilitatoe analysis of UK Biobank phenotype data.
#'
#' @param data The primary data frame.
#'
#' This data frame includes all necessary columns required to perform look-ups (e.g. ICD10 code columns, medication code columns, age, sex, etc.).
#'
#' @param instance_num An integer or string specifying the instance number, or the <[`data-masking`][dplyr_data_masking]> name of the column containing instance numbers to use in the current function.
#'
#' The instance number is also known as the assessment visit number, of which there are 4 (labeled 0...3) in the UK BioBank study. This column does not exist in a standard distribution of the UKB data set, and should be defined and joined to the data set before performing analyses with this function.
#'
#' @param up_to_instance An integer or string specifying the instance number, or the <[`data-masking`][dplyr_data_masking]> name of the column containing instance numbers to use as the maximum allowed instance. Same syntax as `instance_num`, but used to include all data *up to and including* the specified instance number.
#'
#' The instance number is also known as the assessment visit number, of which there are 4 (labeled 0...3) in the UK BioBank study. This column does not exist in a standard distribution of the UKB data set, and should be defined and joined to the data set before performing analyses with this function.
#'
#' @param combine_instances The method used by [up_to_instance_combiner()] or other Reduce-like functions when combining results of a lookup (e.g. a medication lookup, icd10 lookup, biomarker lookup) across multiple instances.
#'
#' For example, when looking up whether a participant is on a medication, the result may differ depending on the instance number. In such a case, one would want to apply the `"any"` method so that results for instance 2 will be [Reduce()]-ed with the `or` operator applied to the results of instances 0 and 1. In the case of numeric lookups (e.g. biomarkers that are recorded at multiple instances), one might want to use the `"mean"` method to average results across instances.
#'
#' Can be one of:
#' * `"any"` - use Boolean `or` (note: requires that lookup results are `logical`)
#' * `"min"` - use the minimum non-NA value
#' * `"max"` - use the maximum non-NA value
#' * `"first"` - use the first/earliest non-NA value
#' * `"last"` - use the last/latest non-NA value
#' * `"mean"` - use the mean of non-NA values
#'
#' Functions that call `combine_instances()` may restrict the choice of options (e.g. it doesn't make sense to apply `"any"` to numeric data).
#'
#' @param date Target date, as a string which is then passed to [lubridate::as_date()].
#' @param death_date_field e.g. f.40000.0.0.Date_of_death
#' @param age_at_instance_field Template field (using <[`data-masking`][dplyr_data_masking]> rules) for the age at each instance.
#' @param date_of_instance_field Template field (using <[`data-masking`][dplyr_data_masking]> rules) for the date associated with each instance.
#' @param year_of_birth_field <[`data-masking`][dplyr_data_masking]> Name of column containing birth year
#' @param month_of_birth_field <[`data-masking`][dplyr_data_masking]> Name of column containing birth month
#' @param diagnosis_field e.g. f.20002.0.0.Non_cancer_illness_code_self_reported
#' @param icd10_field e.g. f.41270.0.0.Diagnoses_ICD10
#'
#' @import dplyr
#' @import rlang
#' @importFrom magrittr %>% %<>%
#'
#' @docType package
NULL
