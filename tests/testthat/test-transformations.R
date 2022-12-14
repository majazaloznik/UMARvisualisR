test_that("transformations work", {
  df <- data.frame(value = c(1:10))
  expect_equal(dim(rolling_average(df)), c(10,2))
  prep_l <- list(df, interval="M")
  expect_equal(dim(add_rolling_average(prep_l)[[1]]), c(10,2))
  expect_equal(length(prep_l), 2)
})
