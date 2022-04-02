# R-ukbiobank

![build](https://raw.githubusercontent.com/dwyl/repo-badges/main/svg/build-passing.svg)
![release_date](https://img.shields.io/github/release-date/adamleejohnson/R-ukbiobank)
![license](https://img.shields.io/github/license/adamleejohnson/R-ukbiobank)
![code_size](https://img.shields.io/github/languages/code-size/adamleejohnson/R-ukbiobank)

An R package to facilitate UK Biobank analysis

## Introduction

This package contains a collection of functions that facilitate the extraction of phenotypes from UK Biobank data. For example, the `icd10_lookup(...)` function facilitates the indentification of patients whose inpatient hospitalization records match any of the provided ICD codes.

Additionally, several functions have been defined to identify patients diagnosed with various diseases, such as diabetes, heart failure, coronary artery disease, etc. These generally use a combination of sources within the database (ICD10 codes, self-reported diagnoses, medications, lab values) specific to each particular disease. Refer to the source code for each of these functions for details.

## Installation

Use the `devtools` package in R to install as follows:

```r
devtools::install_github("adamleejohnson/R-ukbiobank")
```

## General Usage

Most functions in this package take an entire data frame (containing UKB data) as the first argument. The user then specifies the names of columns (with [data masking](https://dplyr.tidyverse.org/reference/dplyr_data_masking.html)) in the data frame that correspond to relevant UKB fields.

The default names of the fields are in the following format, which may differ depending on how the UKB data has been imported into R:

    f.{FieldNum}.{InstanceNum}.{ArrayNum}.{Field_Description}

For example, field 53, containing the date of visit to the assessment center, is (by default) referenced as follows:

    f.53.0.0.Date_of_attending_assessment_centre

The field names can be specified and overriden if they have been formatted differently in the UKB data frame. However, this may break the field name expansion functionality (see below), so it is therefore recommended to at least have the leading portion of the field name in the above format.

For code clarity, it is recommended to refer to functions using `::` notation (e.g. `ukbiobank::Age_at_date()`), rather than loading the library with `library(ukbiobank)`.

### Data masking

Functions that require the name of a field (i.e., column) in the data frame can accept quoted (i.e. symbolic) field names to facilitate tidy data masking. See https://dplyr.tidyverse.org/reference/dplyr_data_masking.html.

### Field name expansion

Some functions (for example those that look-up ICD10 codes) will need to search many consecutive fields that are indexed by instance number or array number. In this case, only one field name needs to be provided and the function will identify all related fields. For example, the ICD10 lookup function needs to know that field `f.41270.0.0.Diagnoses_ICD10` is used for ICD10 diagnoses, but will automatically search all fields of the form `f.41270.###.###.Diagnoses_ICD10`. This means, however, that each field must include "Field.Instance.Array" in `###.###.###` format somewhere in the name.

## Examples

### Age as of a particular date

To return a numeric vector of the age (in years) of all patients at a specific date, the following call is made:

```r
ukb_data_frame %>%
  ukbiobank::Age_at_date(
    "2020-01-01",
    year_of_birth = f.34.0.0.Year_of_birth,
    month_of_birth = f.52.0.0.Month_of_birth
  )
```

### Mutating new columns with dplyr

Note that if using the pipe `%>%` operator with functions like `dplyr::mutate()`, the `.` symbol must be passed to the first argument of most functions in this package (i.e., most functions require the entire dataset be passed as the first argument). See documentation for the [`magrittr "dot"`](https://magrittr.tidyverse.org/reference/pipe.html) for an explanation of the `%>%` and `.` notation.

```r
ukb_data_frame %>%
  dplyr::mutate(
    has_CHF = ukbiobank::diagnosed_chf(.)
  )
```

---

Written by Adam L. Johnson, MD [✉️](mailto:sealant.06.sirloin@icloud.com?subject=AJTools%20R%20Package) © 2022
