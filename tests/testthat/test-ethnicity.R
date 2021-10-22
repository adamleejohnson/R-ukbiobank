test_that("get ethnicity columns", {
  expect_equal(
    df_test %>% expand_instances("f.21000.0.0.Ethnic_background") %>% toString(),
    "f.21000.0.0.Ethnic_background, f.21000.1.0.Ethnic_background, f.21000.2.0.Ethnic_background"
  )
})

test_that("check ethnicity", {
  expect_equal(
    df_test %>% ethnicity_self_reported() %>% toString(),
    "White, White, White, White, White, White, White, White, White, White"
  )
})
