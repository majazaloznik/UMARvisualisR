test_that("transformations work", {
  df <- data.frame(value = c(1:10))
  expect_equal(dim(UMARvisualisR:::rolling_average(df)), c(10,2))
  prep_l <- list(df, interval="M")
  expect_equal(dim(add_rolling_average(prep_l)[[1]]), c(10,2))
  expect_equal(length(prep_l), 2)
  df <- data.frame(value = c(1:24), period = c(24:1))
  expect_true(any(is.na(yoy_change(df, 12)$value)))
  prep_l <- list(single = df, interval="M")
  prep_l <- add_yoy_change(prep_l)
  expect_equal(dim(prep_l[[1]]), c(24,2))
  expect_equal(length(prep_l), 4)
  prep_l <- list(single = df, interval="M")
  prep_l <- add_yoy_of_rolling(prep_l)
  expect_equal(length(prep_l), 4)
  expect_equal(dim(prep_l[[1]]), c(24,2))
})





