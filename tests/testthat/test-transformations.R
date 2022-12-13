test_that("transformations work", {
  df <- data.frame(value = c(1:10))
  expect_equal(dim(rolling_average(df)), c(10,2))
})
