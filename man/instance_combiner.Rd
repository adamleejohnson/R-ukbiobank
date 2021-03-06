% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/instance_array_helper.R
\name{instance_combiner}
\alias{instance_combiner}
\title{Instance combiner helper function}
\usage{
instance_combiner(
  data,
  lookup_by_instance_fn,
  combine_instances = c("any", "max", "min", "first", "last", "mean"),
  up_to_instance = default_up_to_inst(),
  after_instance = default_after_inst()
)
}
\arguments{
\item{data}{The primarydata frame.

Thisdata frame includes all necessary columns required to perform look-ups (e.g. ICD10 code columns, medication code columns, age, sex, etc.).}

\item{lookup_by_instance_fn}{Function that takes a target instance as its only argument, and returns a vector of data.}

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

\item{up_to_instance}{An integer specifying an instance number, or the name of the column (using <\code{\link[=dplyr_data_masking]{data-masking}}> rules) containing instance numbers to use as the \strong{maximum instance (inclusive)}. Used to include alldata \strong{up to and including} the specified instance number. Defaults to \code{default_up_to_inst()}, which as of this writing returns \code{3}.}

\item{after_instance}{An integer specifying an instance number, or the name of the column (using <\code{\link[=dplyr_data_masking]{data-masking}}> rules) containing instance numbers to use as the \strong{minimum instance (non-inclusive)}. Used to include alldata \strong{after but NOT including} the specified instance number. Defaults to \code{default_after_inst()}, which is typically -1 (i.e., include instances 0 and later).}
}
\description{
Helper to apply a lookup function to a range of instances, and combine the result.
}
\keyword{internal}
