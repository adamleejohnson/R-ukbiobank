test_that("age at birth date", {
  expect_true({
    all(
      df_test %>%
      mutate(bd = lubridate::make_date(
        f.34.0.0.Year_of_birth,
        f.52.0.0.Month_of_birth,
        15
      )
    ) %>% age_at_date(bd) == 0
    )
  })
})

test_that("age at instance", {
  expect_equal(
    df_test %>% age_at_instance(0),
    c(49, 55, 42, 52, 64, 69, 53, 63, 60, 61)
  )

  expect_equal(
    df_test %>% age_at_instance(2),
    c(60, 64, 52, 63, 73, 79, 61, 73, 71, 68)
  )

  expect_equal(
    df_test %>% age_at_instance(UKB_Instance),
    c(60, 64, 52, 63, 73, 79, 61, 73, 71, 68)
  )
})

test_that("age at birth date", {
  expect_equal(
    df_test %>%
      age_at_instance_computed(2) %>%
      mean() %>%
      round(2),
    66.94
  )
})

test_that("age group", {
  expect_equal(
    convert_to_age_group(c(5, 23, 52, 63, 73, 79, 101)) %>% toString(),
    "0-9, 20-29, 50-59, 60-69, 70-79, 70-79, 100+"
  )
})
