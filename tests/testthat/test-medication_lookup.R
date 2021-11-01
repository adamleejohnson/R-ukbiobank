test_that("medication codings", {
  expect_equal(
    med_text_to_codings("
      atorvastatin
      fluvastatin
      pravastatin
      rosuvastatin
      simvastatin
      velastatin
    ") %>% toString(),
    "1140861958, 1140888594, 1140888648, 1140910654, 1141146234, 1141192410"
  )

  expect_equal(
    df_test %>% medication_statin(up_to_instance = DEFAULT_UP_TO_INST),
    c(F, T, F, T, T, T, F, F, T, F)
  )

  expect_equal(
    df_test %>% medication_PDE5i(up_to_instance = DEFAULT_UP_TO_INST),
    c(F, F, F, F, F, F, F, F, F, F)
  )
})