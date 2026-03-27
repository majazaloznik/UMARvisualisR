# tests/testthat/test-user_api.R

# === prep_chart: format detection ===

test_that("wide format is detected correctly", {
  df <- data.frame(
    date = as.Date("2020-01-01") + 0:4,
    A = 1:5, B = 6:10
  )
  chart <- prep_chart(df)
  expect_s3_class(chart, "umar_chart")
  expect_length(chart$datapoints, 2)
  expect_equal(chart$series[[1]]$legend_txt, "A")
  expect_equal(chart$series[[2]]$legend_txt, "B")
})

test_that("single series wide format works", {
  df <- data.frame(date = as.Date("2020-01-01") + 0:4, value = 1:5)
  chart <- prep_chart(df)
  expect_length(chart$datapoints, 1)
})

test_that("long format is detected correctly", {
  df <- data.frame(
    date = rep(as.Date("2020-01-01") + 0:2, 2),
    value = 1:6,
    group = rep(c("X", "Y"), each = 3)
  )
  chart <- prep_chart(df)
  expect_length(chart$datapoints, 2)
  expect_equal(chart$series[[1]]$legend_txt, "X")
})

test_that("non-dataframe input errors", {
  expect_error(prep_chart(list(1, 2)), "data must be a data.frame")
})

test_that("missing Date column errors", {
  df <- data.frame(x = 1:5, y = 6:10)
  expect_error(prep_chart(df), "No Date column found")
})

test_that("ambiguous format errors", {
  df <- data.frame(
    date = as.Date("2020-01-01") + 0:4,
    val = 1:5, grp = letters[1:5], extra = 6:10
  )
  expect_error(prep_chart(df), "Cannot detect format")
})

# === prep_chart: period string support ===

test_that("monthly period strings are converted", {
  df <- data.frame(
    period = paste0("2023M", sprintf("%02d", 1:12)),
    value = rnorm(12)
  )
  chart <- prep_chart(df)
  expect_s3_class(chart$datapoints[[1]]$date, "Date")
})

test_that("quarterly period strings are converted", {
  df <- data.frame(period = paste0("2023Q", 1:4), value = 1:4)
  chart <- prep_chart(df)
  expect_s3_class(chart$datapoints[[1]]$date, "Date")
})

test_that("annual period strings are converted", {
  df <- data.frame(year = as.character(2015:2020), value = 1:6)
  chart <- prep_chart(df)
  expect_s3_class(chart$datapoints[[1]]$date, "Date")
})

# === prep_chart: type validation ===

test_that("type is recycled for all series", {
  df <- data.frame(date = as.Date("2020-01-01") + 0:4, A = 1:5, B = 6:10)
  chart <- prep_chart(df, type = "bar")
  expect_equal(chart$series[[1]]$type, "bar")
  expect_equal(chart$series[[2]]$type, "bar")
})

test_that("per-series type works", {
  df <- data.frame(date = as.Date("2020-01-01") + 0:4, A = 1:5, B = 6:10)
  chart <- prep_chart(df, type = c("bar", "line"))
  expect_equal(chart$series[[1]]$type, "bar")
  expect_equal(chart$series[[2]]$type, "line")
})

test_that("invalid type errors", {
  df <- data.frame(date = as.Date("2020-01-01") + 0:4, value = 1:5)
  expect_error(prep_chart(df, type = "scatter"), "type must be")
})

test_that("wrong length type errors", {
  df <- data.frame(date = as.Date("2020-01-01") + 0:4, A = 1:5, B = 6:10)
  expect_error(prep_chart(df, type = c("line", "bar", "line")), "type must be length")
})

# === prep_chart: colours ===

test_that("default colours are assigned from palette", {
  df <- data.frame(date = as.Date("2020-01-01") + 0:4, A = 1:5, B = 6:10)
  chart <- prep_chart(df)
  expect_equal(chart$series[[1]]$colour, unname(umar_cols()[1]))
  expect_equal(chart$series[[2]]$colour, unname(umar_cols()[2]))
})

test_that("palette index colours work", {
  df <- data.frame(date = as.Date("2020-01-01") + 0:4, A = 1:5, B = 6:10)
  chart <- prep_chart(df, colours = c(3, 5))
  expect_equal(chart$series[[1]]$colour, unname(umar_cols()[3]))
  expect_equal(chart$series[[2]]$colour, unname(umar_cols()[5]))
})

test_that("hex colours work", {
  df <- data.frame(date = as.Date("2020-01-01") + 0:4, value = 1:5)
  chart <- prep_chart(df, colours = "#FF0000")
  expect_equal(unname(chart$series[[1]]$colour), "#FF0000")
})

test_that("wrong number of colours errors", {
  df <- data.frame(date = as.Date("2020-01-01") + 0:4, A = 1:5, B = 6:10)
  expect_error(prep_chart(df, colours = c(1, 2, 3)), "colours must have 2 elements")
})

test_that("out of range colour index errors", {
  df <- data.frame(date = as.Date("2020-01-01") + 0:4, value = 1:5)
  expect_error(prep_chart(df, colours = 9), "Colour index must be between 1 and 8")
})

# === prep_chart: date limits ===

test_that("xmin and xmax are stored correctly", {
  df <- data.frame(date = as.Date("2020-01-01") + 0:99, value = rnorm(100))
  chart <- prep_chart(df, xmin = "2020-02-01", xmax = "2020-03-01")
  expect_equal(chart$config$xmin, as.Date("2020-02-01"))
  expect_equal(chart$config$xmax, as.Date("2020-03-01"))
})

test_that("xmin >= xmax errors", {
  df <- data.frame(date = as.Date("2020-01-01") + 0:4, value = 1:5)
  expect_error(prep_chart(df, xmin = "2020-03-01", xmax = "2020-02-01"),
               "xmin must be earlier than xmax")
})

test_that("invalid date string errors", {
  df <- data.frame(date = as.Date("2020-01-01") + 0:4, value = 1:5)
  expect_error(prep_chart(df, xmin = "not-a-date"), "must be a Date")
})

# === prep_chart: legend ===

test_that("custom legend labels work", {
  df <- data.frame(date = as.Date("2020-01-01") + 0:4, A = 1:5, B = 6:10)
  chart <- prep_chart(df, legend = c("First", "Second"))
  expect_equal(chart$series[[1]]$legend_txt, "First")
  expect_equal(chart$series[[2]]$legend_txt, "Second")
})

test_that("wrong length legend errors", {
  df <- data.frame(date = as.Date("2020-01-01") + 0:4, A = 1:5, B = 6:10)
  expect_error(prep_chart(df, legend = "Only one"), "legend must have 2 elements")
})

# === prep_chart: stacked ===

test_that("stacked works with all bars", {
  df <- data.frame(date = as.Date("2020-01-01") + 0:4, A = 1:5, B = 6:10)
  chart <- prep_chart(df, type = "bar", stacked = TRUE)
  expect_true(chart$config$stacked)
})

test_that("stacked with no bars errors", {
  df <- data.frame(date = as.Date("2020-01-01") + 0:4, A = 1:5, B = 6:10)
  expect_warning(prep_chart(df, type = "line", stacked = TRUE),
               "stacked ")
})

test_that("stacked with mixed types works", {
  df <- data.frame(date = as.Date("2020-01-01") + 0:4, A = 1:5, B = 6:10)
  chart <- prep_chart(df, type = c("bar", "line"), stacked = TRUE)
  expect_true(chart$config$stacked)
})

# === prep_chart: emphasis ===

test_that("emphasis NULL stores NULL (auto-detect)", {
  df <- data.frame(date = as.Date("2020-01-01") + 0:4, value = 1:5)
  chart <- prep_chart(df)
  expect_null(chart$config$emphasis)
})

test_that("emphasis FALSE stores FALSE", {
  df <- data.frame(date = as.Date("2020-01-01") + 0:4, value = 1:5)
  chart <- prep_chart(df, emphasis = FALSE)
  expect_false(chart$config$emphasis)
})

test_that("emphasis numeric stores values", {
  df <- data.frame(date = as.Date("2020-01-01") + 0:4, value = 1:5)
  chart <- prep_chart(df, emphasis = c(0, 100))
  expect_equal(chart$config$emphasis, c(0, 100))
})

test_that("emphasis non-numeric errors", {
  df <- data.frame(date = as.Date("2020-01-01") + 0:4, value = 1:5)
  expect_error(prep_chart(df, emphasis = "zero"), "emphasis must be NULL, FALSE, or a numeric")
})

# === prep_chart: transformations ===

test_that("rolling average works", {
  df <- data.frame(
    date = seq(as.Date("2020-01-01"), by = "month", length.out = 12),
    value = 1:12
  )
  chart <- prep_chart(df, rolling = 3)
  expect_true(is.na(chart$datapoints[[1]]$value[1]))
  expect_false(is.na(chart$datapoints[[1]]$value[12]))
  expect_equal(chart$config$rolling, 3)
})

test_that("invalid rolling errors", {
  df <- data.frame(date = as.Date("2020-01-01") + 0:4, value = 1:5)
  expect_error(prep_chart(df, rolling = 1), "rolling must be a single integer >= 2")
  expect_error(prep_chart(df, rolling = c(2, 3)), "rolling must be a single integer >= 2")
})

test_that("growth sets y_axis to %", {
  df <- data.frame(
    date = seq(as.Date("2020-01-01"), by = "month", length.out = 24),
    value = 100 + cumsum(rnorm(24))
  )
  chart <- prep_chart(df, growth = "YOY")
  expect_equal(chart$config$y_axis, "%")
  expect_equal(chart$config$growth, "YOY")
})

test_that("growth doesn't override explicit y_axis", {
  df <- data.frame(
    date = seq(as.Date("2020-01-01"), by = "month", length.out = 24),
    value = 100 + cumsum(rnorm(24))
  )
  chart <- prep_chart(df, growth = "YOY", y_axis = "Growth rate, %")
  expect_equal(chart$config$y_axis, "Growth rate, %")
})

test_that("invalid growth errors", {
  df <- data.frame(date = as.Date("2020-01-01") + 0:4, value = 1:5)
  expect_error(prep_chart(df, growth = "WOW"), "growth must be")
})

test_that("index sets y_axis", {
  df <- data.frame(
    date = seq(as.Date("2020-01-01"), by = "month", length.out = 24),
    value = 100 + cumsum(rnorm(24))
  )
  chart <- prep_chart(df, index = "2020")
  expect_true(grepl("Indeks", chart$config$y_axis))
  expect_equal(chart$config$index, "2020")
})

test_that("growth and index together errors", {
  df <- data.frame(
    date = seq(as.Date("2020-01-01"), by = "month", length.out = 24),
    value = 100 + cumsum(rnorm(24))
  )
  expect_error(prep_chart(df, growth = "YOY", index = "2020"),
               "growth and index are mutually exclusive")
})

test_that("invalid index format errors", {
  df <- data.frame(date = as.Date("2020-01-01") + 0:4, value = 1:5)
  expect_error(prep_chart(df, index = "Jan 2020"), "index must be a year")
})

# === prep_chart: config storage ===

test_that("all config fields are stored", {
  df <- data.frame(date = as.Date("2020-01-01") + 0:4, value = 1:5)
  chart <- prep_chart(df, title = "Test", y_axis = "%", emphasis = 0,
                      legend_columns = 3, rolling = 3)
  expect_equal(chart$config$title, "Test")
  expect_equal(chart$config$y_axis, "%")
  expect_equal(chart$config$emphasis, 0)
  expect_equal(chart$config$legend_columns, 3)
  expect_equal(chart$config$rolling, 3)
})

# === print method ===

test_that("print method returns invisible chart", {
  df <- data.frame(date = as.Date("2020-01-01") + 0:4, value = 1:5)
  chart <- prep_chart(df, title = "Test")
  expect_output(print(chart), "UMAR chart configuration")
  expect_output(print(chart), "Title:")
  expect_invisible(print(chart))
})

# === draw_emphasis ===

test_that("draw_emphasis with FALSE does nothing", {
  plot(1, 1)
  expect_invisible(draw_emphasis(FALSE, NULL, c(0, 10)))
})

test_that("draw_emphasis auto-detects 0", {
  # just check it doesn't error — visual correctness needs vdiffr
  plot(1, 1)
  expect_invisible(draw_emphasis(NULL, NULL, c(-5, 5)))
})

test_that("draw_emphasis auto-detects 100 for index", {
  plot(1, 1)
  expect_invisible(draw_emphasis(NULL, "Indeks", c(90, 110)))
})

test_that("draw_emphasis excludes values on limits", {
  plot(1, 1)
  # 0 is exactly on the lower limit — should NOT be drawn
  expect_invisible(draw_emphasis(c(0), NULL, c(0, 10)))
})


# === convert_period_column ===

test_that("convert_period_column skips if Date already exists", {
  df <- data.frame(date = as.Date("2020-01-01") + 0:4, value = 1:5)
  result <- convert_period_column(df)
  expect_identical(result, df)
})

test_that("convert_period_column converts monthly strings", {
  df <- data.frame(period = c("2023M01", "2023M02"), value = 1:2)
  result <- convert_period_column(df)
  expect_s3_class(result$period, "Date")
})

test_that("convert_period_column ignores non-period strings", {
  df <- data.frame(label = c("foo", "bar"), value = 1:2)
  result <- convert_period_column(df)
  expect_type(result$label, "character")
})

