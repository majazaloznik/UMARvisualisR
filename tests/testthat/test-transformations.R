test_that("transformations work", {
  df <- data.frame(value = c(1:10), period = c(10,1))
  expect_equal(dim(UMARvisualisR:::rolling_average(df)), c(10,3))
  expect_equal(dim(add_rolling_average(df)[[1]]), c(10,3))
  df <- data.frame(value = c(1:24), period = c(24:1))
  expect_true(any(is.na(yoy_change(df, 12)$value)))
  prep_l <- add_yoy_change(df, "M")
  expect_equal(dim(prep_l[[1]]), c(24,2))
  expect_equal(length(prep_l), 3)
  prep_l <- add_yoy_of_rolling(df, interval ="M")
  expect_equal(length(prep_l), 3)
  expect_equal(dim(prep_l[[1]]), c(24,2))
  df <- data.frame(value = c(1:24), period = c(24:1))
  input_data <- list(data_points = list(df), rolling_average_periods = 3, rolling_average_alignment = "c",
                year_on_year = FALSE, interval = "M")
  expect_equal(length(do_transformations(input_data)), 6)
  expect_equal(dim(do_transformations(input_data)$data_points[[1]]), c(24,3))
})





