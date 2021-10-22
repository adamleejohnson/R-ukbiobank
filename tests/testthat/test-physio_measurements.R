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
