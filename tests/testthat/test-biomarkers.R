test_that("test biomarker lookups", {
  expect_equal(
    df_test %>%
      biomarker_lookup(f.30750.0.0.Glycated_haemoglobin_HbA1c, up_to_instance = 2, combine_instances = "max"),
    c(34.6, 35.4, 36.7, 39.1, 36, 35.6, NA, 37.5, 35.4, 33.9)
  )

  expect_equal(
    df_test %>%
      biomarker_lookup(f.30750.0.0.Glycated_haemoglobin_HbA1c, up_to_instance = 2, combine_instances = "min"),
    c(34.6, 32.5, 36.7, 39.1, 36, 35.6, NA, 37.5, 35.4, 33.9)
  )

  expect_equal(
    df_test %>%
      biomarker_lookup(f.30750.0.0.Glycated_haemoglobin_HbA1c, up_to_instance = 2, combine_instances = "first"),
    c(34.6, 32.5, 36.7, 39.1, 36, 35.6, NA, 37.5, 35.4, 33.9)
  )

  expect_equal(
    df_test %>%
      biomarker_lookup(f.30750.0.0.Glycated_haemoglobin_HbA1c, up_to_instance = 2, combine_instances = "last"),
    c(34.6, 35.4, 36.7, 39.1, 36, 35.6, NA, 37.5, 35.4, 33.9)
  )

  expect_equal(
    df_test %>%
      biomarker_lookup(f.30750.0.0.Glycated_haemoglobin_HbA1c, up_to_instance = 2, combine_instances = "mean"),
    c(34.6, 33.95, 36.7, 39.1, 36, 35.6, NaN, 37.5, 35.4, 33.9)
  )
})

test_that("test A1c lookups", {
  expect_equal(
    biomarker_A1c_percent(df_test, up_to_instance = 0)[2] %>% round(2),
    5.12
  )

  expect_equal(
    biomarker_A1c_percent(df_test, up_to_instance = 1)[2] %>% round(2),
    5.39
  )

  expect_equal(
    biomarker_A1c_percent(df_test, up_to_instance = 2, combine_instances = "mean")[2] %>% round(2),
    5.26
  )
})
