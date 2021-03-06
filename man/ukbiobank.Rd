% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aaa.R
\docType{package}
\name{ukbiobank}
\alias{ukbiobank}
\title{ukbiobank}
\arguments{
\item{data}{The primarydata frame.

Thisdata frame includes all necessary columns required to perform look-ups (e.g. ICD10 code columns, medication code columns, age, sex, etc.).}

\item{instance_num}{An integer specifying the instance number, or the name of the column (using <\code{\link[=dplyr_data_masking]{data-masking}}> rules) containing instance numbers to use in the current function.}

\item{after_instance}{An integer specifying an instance number, or the name of the column (using <\code{\link[=dplyr_data_masking]{data-masking}}> rules) containing instance numbers to use as the \strong{minimum instance (non-inclusive)}. Used to include alldata \strong{after but NOT including} the specified instance number. Defaults to \code{default_after_inst()}, which is typically -1 (i.e., include instances 0 and later).}

\item{up_to_instance}{An integer specifying an instance number, or the name of the column (using <\code{\link[=dplyr_data_masking]{data-masking}}> rules) containing instance numbers to use as the \strong{maximum instance (inclusive)}. Used to include alldata \strong{up to and including} the specified instance number. Defaults to \code{default_up_to_inst()}, which as of this writing returns \code{3}.}

\item{combine_instances}{The method used by \code{\link[=instance_combiner]{instance_combiner()}} or other Reduce-like functions when combining results of a lookup (e.g. a medication lookup, icd10 lookup, biomarker lookup) across multiple instances.

For example, when looking up whether a participant is on a medication, the result may differ depending on the instance number. In such a case, one would want to apply the \code{"any"} method so that results for instance 2 will be \code{\link[=Reduce]{Reduce()}}-ed with the \code{or} operator applied to the results of instances 0 and 1. In the case of numeric lookups (e.g. biomarkers that are recorded at multiple instances), one might want to use the \code{"mean"} method to average results across instances.

Can be one of:
\itemize{
\item \code{"any"} - use Boolean \code{or} (note: requires that lookup results are \code{logical})
\item \code{"min"} - use the minimum non-NA value
\item \code{"max"} - use the maximum non-NA value
\item \code{"first"} - use the first/earliest non-NA value
\item \code{"last"} - use the last/latest non-NA value
\item \code{"mean"} - use the mean of non-NA values
}

Functions that call \code{combine_instances()} may restrict the choice of options (e.g. it doesn't make sense to apply \code{"any"} to numeric data).}

\item{combine_array}{If a measurement field has multiple array values (e.g. blood pressure recordings are made in duplicate), specify how these values should be combined. See \code{combine_instances} for details and options.}

\item{date}{Target date, as a string which is then passed to \code{\link[lubridate:as_date]{lubridate::as_date()}}.}

\item{death_date_col}{Template column name for date of death.\cr Default = \code{f.40000.0.0.Date_of_death}.}

\item{age_at_instance_col}{Template column name for age at instance.\cr Default = \code{f.21003.0.0.Age_when_attended_assessment_centre}.}

\item{date_of_instance_col}{Template column name for date of each instance.\cr Default = \code{f.53.0.0.Date_of_attending_assessment_centre}.}

\item{year_of_birth_col}{Template column name for year of birth.\cr Default = \code{f.34.0.0.Year_of_birth}.}

\item{month_of_birth_col}{Template column name for month of birth.\cr Default = \code{f.52.0.0.Month_of_birth}.}

\item{diagnosis_col}{Template column name for self-reported diagnosis codes.\cr Default = \code{f.20002.0.0.Non_cancer_illness_code_self_reported}.}

\item{medication_col}{Template column name for self-reported medication codes.\cr Default = \code{f.20003.0.0.Treatment_medication_code}.}

\item{icd10_col}{Template column name for ICD10 codes.\cr Default = \code{f.41270.0.0.Diagnoses_ICD10}.}

\item{icd10_date_col}{Template column name for dates of ICD10 diagnoses.\cr Default = \code{f.41280.0.0.Date_of_first_in_patient_diagnosis_ICD10}.}

\item{ethnicity_col}{Template column name for ethnic background.\cr Default = \code{f.21000.0.0.Ethnic_background}.}

\item{measurement_col}{Template column name for general measurements.\cr Example = \code{f.4080.0.0.Systolic_blood_pressure_automated_reading}.}

\item{measurement_col_alt}{Alternate template column name, used specifically for manual readings of measurements when automated methods return NA.}

\item{height_col}{Template column name for height.\cr Default = \code{f.50.0.0.Standing_height}.}

\item{weight_col}{Template column name for weight.\cr Default = \code{f.21002.0.0.Weight}.}
}
\description{
This package contains a collection of tools and functions to facilitate analysis of UK Biobank phenotype data.
}
\section{Instancing}{
 The UK Biobank records visits as separate "instances." As of this writing, there are 4 instances labeled 0 through 3. At each instance, various information can be recorded or re-recorded. For example, blood pressure is typically recorded at most in-person evaluations. Therefore, there may be 4 separate columns for blood pressure recordings (actually, there could be more because the blood pressure may be recorded multiple times at each instance). Almost all of the functions in this package will utilize instance numbers to specify from which time pointsdata should be retrieved. For example, we may want to know the state of ICD10 diagnoses \emph{before} instance 2. In this case, we would specify \code{up_to_instance = 1} (search up to instance 1, inclusive) in functions that take this as an argument.

Arguments like \code{up_to_instance} and \code{after_instance} can take a constant instance number as their value. But they can also take the name of a column that contains an instance number, so that different instance limits can be used for each participant. For example, some participants undergo MRI at instance 2, and others at instance 3. If we want to know the state of a diagnosis up to and including the time of MRI, we would want to assign \code{up_to_instance} to the name of the column that specifies which instance the MRI occurred at. This column typically has to be generated by the user and attached to thedata frame beforehand.
}

\section{Column names}{
 Most functions in this package will take column names (\verb{*_col}) as optional arguments (otherwise a default column names are used) which are then used as templates to find all other columns with the same field number, but different instance and array numbers. These functions will automatically find all matching instances (and arrays within each instance) within the specified parameters. Internally, a set of \verb{expand_instance_*()} helper functions, which themselves rely on the \code{column_expansion_helper()} function, perform the work of searching for matching columns.
}

\keyword{internal}
