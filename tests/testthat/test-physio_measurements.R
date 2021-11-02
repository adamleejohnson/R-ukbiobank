test_that("array combination works", {
  expect_equal(
    df_test %>% physio_systolicBP(up_to_instance = 0, combine_array = "last"),
    c(133, 136, 125, 124, 178, 167, 131, 163, 171, 127)
  )

  expect_equal(
    df_test %>% physio_systolicBP(up_to_instance = 0, combine_array = "mean"),
    c(137.5, 138.5, 134.5, 130.5, 187, 167, 135, 162.5, 172.5, 132.5)
  )
})

test_that("after_instance works", {
  expect_equal(
    df_test %>% physio_systolicBP(after_instance = 1, up_to_instance = 3, combine_array = "mean"),
    c(136.5, 141.5, 151.5, 151, 175, 191, 164.5, 170, 152.5, 141.5)
  )

  expect_equal(
    df_test %>% physio_systolicBP(after_instance = UKB_Instance, up_to_instance = 3, combine_array = "mean"),
    c(NA, NA, 151.5, NA, NA, NA, 164.5, NA, NA, NA)
  )
})

test_that("throw error if after_instance == up_to_instance", {
  expect_error(
    df_test %>% physio_systolicBP(after_instance = 1, up_to_instance = 1, combine_array = "mean")
  )
})
