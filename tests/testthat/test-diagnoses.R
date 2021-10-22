test_that("dx works", {
  expect_equal(df_test %>% diagnosed_hld(up_to_instance = UKB_Instance), c(T,T,F,T,T,T,F,F,T,F))

  expect_equal(df_test %>% diagnosed_hld(up_to_instance = 1), c(T,F,F,T,T,F,F,F,T,F))

  expect_equal(df_test %>% diagnosed_hld(), c(T,T,F,T,T,T,F,F,T,F))
})
