test_that("remove_na_columns works", {
  expect_equal(
    df_test %>% remove_na_columns() %>% length(),
    650
  )

  expect_equal(
    df_test[1:5] %>% remove_na_columns() %>% length(),
    5
  )
})

test_that("modified OR works", {
  a <- c(rep(T,3),rep(F,3),rep(NA,3))
  b <- rep(c(T,F,NA),3)

  expect_equal(
    a %|% b,
    c(T,T,T,T,F,F,T,F,NA)
  )
})
