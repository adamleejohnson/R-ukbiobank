test_that("instance expander works", {
  expect_equal(
    {
      df_test %>%
        expand_instances(f.21003.0.0.Age_when_attended_assessment_centre) %>%
        length()
    },
    4
  )
})

test_that("instance expander with max works", {
  expect_equal(
    {
      df_test %>%
        expand_instances(f.21003.0.0.Age_when_attended_assessment_centre, 0, 2) %>%
        length()
    },
    3
  )
})

test_that("array expander works", {
  expect_equal(
    {
      df_test %>%
        expand_array(f.41270.0.0.Diagnoses_ICD10) %>%
        length()
    },
    226
  )
})

test_that("array+instance expander works", {
  expect_equal(
    {
      df_test %>%
        expand_instances_and_array(f.20002.0.0.Non_cancer_illness_code_self_reported) %>%
        length()
    },
    136
  )
})

test_that("instance+array expander with instance filter works", {
  expect_equal(
    {
      df_test %>%
        expand_instances_and_array(
          f.20002.0.0.Non_cancer_illness_code_self_reported,
          min_instance = 1,
          max_instance = 2
        ) %>%
        length()
    },
    68
  )
})

test_that("instance+array expander with array filter works", {
  expect_equal(
    {
      df_test %>%
        expand_instances_and_array(
          f.20002.0.0.Non_cancer_illness_code_self_reported,
          min_array = 10,
          max_array = 19
        ) %>%
        length()
    },
    40
  )
})

test_that("instance+array expander with instance+array filter works", {
  expect_equal(
    {
      df_test %>%
        expand_instances_and_array(
          f.20002.0.0.Non_cancer_illness_code_self_reported,
          min_array = 10,
          max_array = 15,
          min_instance = 1,
          max_instance = 2
        ) %>%
        length()
    },
    12
  )
})

test_that("array+instance expander with remove_NA works", {
  expect_equal(
    {
      df_test %>%
        remove_na_columns() %>%
        expand_instances_and_array(f.20002.0.0.Non_cancer_illness_code_self_reported) %>%
        length()
    },
    40
  )
})

test_that("catch bad field name", {
  expect_error({
    df_test %>%
      expand_instances_and_array(f.20002.0)
  })
  expect_error({
    df_test %>%
      expand_instances_and_array(f.20002.X.0)
  })
  expect_error({
    df_test %>%
      expand_instances_and_array("f.20002.0")
  })
})

test_that("catch field not found", {
  expect_error({
    df_test %>%
      select_instance_and_expand_array(f.9999999.0.0.Dummy)
  }, regexp = "There are no columns in the dataframe that match the template column name")
})

test_that("check number of biomarker columns", {
  expect_equal(
    df_test %>%
      select_instance_and_array(
        f.30750.0.0.Glycated_haemoglobin_HbA1c,
        instance = 1,
        array = 0
      ),
    list(as.symbol("f.30750.1.0.Glycated_haemoglobin_HbA1c"))
  )

  expect_equal(
    df_test %>%
      select_instance_and_array(
        f.30750.0.0.Glycated_haemoglobin_HbA1c,
        instance = 1,
        array = 1
      ) %>% length(),
    0
  )
})
