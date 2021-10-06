#' @name ukbiobank
#' @title ukbiobank
#' @description his package contains a collection of tools and functions to facilitatoe analysis of UK Biobank phenotype data.
#'
#' @param data The primary data frame.
#'
#' This data frame includes all necessary columns required to perform look-ups (e.g. ICD10 code columns, medication code columns, sex).
#'
#' @param instance_num An integer or string specifying the instance number, or the <[`data-masking`][dplyr_data_masking]> name of the column containing instance numbers to use in the current function.
#'
#' The instance number is also known as the assessment visit number, of which there are 4 (labeled 0...3) in the UK Biobank study. This column does not exist in a standard distribution of the UKB data set, and should be joined to the data set before performing analyses with this function.
#'
#' @param date Target date, as a string which is then passed to `lubridate`.
#'
#' @import dplyr
#' @import rlang
#' @importFrom magrittr %>% %<>%
#'
#' @docType package
NULL
