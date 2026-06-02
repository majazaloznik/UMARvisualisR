# === to_internal_config ===
umar_font("sans")
test_that("to_internal_config maps all fields correctly", {
  df <- data.frame(date = as.Date("2020-01-01") + 0:4, A = 1:5, B = 6:10)
  chart <- prep_chart(df, title = "Test", y_axis = "EUR", colours = c(1, 3),
                      stacked = FALSE, legend_columns = 1)
  config <- UMARvisualisR:::to_internal_config(chart)
  expect_equal(config$title, "Test")
  expect_equal(config$y_axis_label, "EUR")
  expect_false(config$stacked)
  expect_equal(config$legend_columns, 1)
  expect_false(config$x_sub_annual)
  expect_false(config$dual_y)
  expect_length(config$series, 2)
  expect_equal(config$series[[1]]$type, "line")
  expect_equal(config$series[[1]]$colour, unname(umar_cols()[1]))
  expect_equal(config$series[[2]]$colour, unname(umar_cols()[3]))
  expect_equal(config$series[[1]]$legend_txt_si, "A")
  expect_equal(config$series[[1]]$legend_txt_en, "A")
  expect_equal(config$series[[1]]$unit, "EUR")
  expect_false(config$series[[1]]$mio_eur)
})

test_that("to_internal_config handles NULL y_axis", {
  df <- data.frame(date = as.Date("2020-01-01") + 0:4, value = 1:5)
  chart <- prep_chart(df)
  config <- UMARvisualisR:::to_internal_config(chart)
  expect_equal(config$y_axis_label, NULL)
  expect_equal(config$series[[1]]$unit, "")
})

test_that("to_internal_config handles bar types", {
  df <- data.frame(date = as.Date("2020-01-01") + 0:4, A = 1:5, B = 6:10)
  chart <- prep_chart(df, type = c("bar", "line"))
  config <- UMARvisualisR:::to_internal_config(chart)
  expect_equal(config$series[[1]]$type, "bar")
  expect_equal(config$series[[2]]$type, "line")
})

test_that("to_internal_config passes xmin and xmax", {
  df <- data.frame(date = as.Date("2020-01-01") + 0:99, value = rnorm(100))
  chart <- prep_chart(df, xmin = "2020-02-01", xmax = "2020-03-01")
  config <- UMARvisualisR:::to_internal_config(chart)
  expect_equal(config$xmin, as.Date("2020-02-01"))
  expect_equal(config$xmax, as.Date("2020-03-01"))
})

# === draw_emphasis ===

test_that("draw_emphasis FALSE does nothing", {
  plot(1, 1)
  expect_invisible(draw_emphasis(FALSE, NULL, c(0, 10)))
})

test_that("draw_emphasis NULL auto-detects zero", {
  plot(1, 1, ylim = c(-5, 5))
  # should not error

  expect_invisible(UMARvisualisR:::draw_emphasis(NULL, NULL, c(-5, 5)))
})

test_that("draw_emphasis NULL does not detect zero when not in range", {
  plot(1, 1, ylim = c(5, 15))
  expect_invisible(UMARvisualisR:::draw_emphasis(NULL, NULL, c(5, 15)))
})

test_that("draw_emphasis NULL auto-detects 100 for indeks", {
  plot(1, 1, ylim = c(90, 110))
  expect_invisible(UMARvisualisR:::draw_emphasis(NULL, "Indeks", c(90, 110)))
})

test_that("draw_emphasis NULL auto-detects 100 for index (English)", {
  plot(1, 1, ylim = c(90, 110))
  expect_invisible(UMARvisualisR:::draw_emphasis(NULL, "Index, 2015=100", c(90, 110)))
})

test_that("draw_emphasis NULL does not detect 100 for non-index", {
  plot(1, 1, ylim = c(90, 110))
  expect_invisible(UMARvisualisR:::draw_emphasis(NULL, "EUR", c(90, 110)))
})

test_that("draw_emphasis explicit values in range are drawn", {
  plot(1, 1, ylim = c(0, 10))
  expect_invisible(UMARvisualisR:::draw_emphasis(c(5), NULL, c(0, 10)))
})

test_that("draw_emphasis excludes values on limits", {
  plot(1, 1, ylim = c(0, 10))
  # 0 is on the limit — should not be drawn by explicit emphasis
  expect_invisible(UMARvisualisR:::draw_emphasis(c(0, 10), NULL, c(0, 10)))
})

test_that("draw_emphasis explicit values outside range are excluded", {
  plot(1, 1, ylim = c(0, 10))
  expect_invisible(UMARvisualisR:::draw_emphasis(c(-5, 15), NULL, c(0, 10)))
})


# === %||% operator ===

test_that("null coalescing operator works", {
  `%||%` <- UMARvisualisR:::`%||%`
  expect_equal(NULL %||% "default", "default")
  expect_equal("value" %||% "default", "value")
})

# === view_chart input validation ===

test_that("view_chart rejects non-chart input", {
  expect_error(view_chart(list(a = 1)), "chart must be a 'umar_chart' object")
  expect_error(view_chart(data.frame(x = 1)), "chart must be a 'umar_chart' object")
  expect_error(view_chart("not a chart"), "chart must be a 'umar_chart' object")
})

# === view_chart restores par on exit ===

test_that("view_chart restores graphics params on exit", {
  df <- data.frame(date = as.Date("2020-01-01") + 0:4, value = 1:5)
  chart <- prep_chart(df, title = "Test")
  op_before <- par("mar")
  view_chart(chart)
  op_after <- par("mar")
  expect_equal(op_before, op_after)
})

# === view_chart with manual ylim ===

test_that("view_chart respects manual ylim", {
  df <- data.frame(date = as.Date("2020-01-01") + 0:4, value = c(95, 100, 105, 98, 102))
  chart <- prep_chart(df, ylim = c(90, 110))
  # should not error
  expect_no_error(view_chart(chart))
})

# === view_chart with emphasis ===

test_that("view_chart draws emphasis for index chart", {
  df <- data.frame(
    date = seq(as.Date("2020-01-01"), by = "month", length.out = 24),
    value = 100 + cumsum(rnorm(24, 0, 2))
  )
  chart <- prep_chart(df, y_axis = "Indeks, 2015=100")
  expect_no_error(view_chart(chart))
})

test_that("view_chart draws explicit emphasis", {
  df <- data.frame(
    date = seq(as.Date("2020-01-01"), by = "month", length.out = 24),
    value = rnorm(24, 0, 3)
  )
  chart <- prep_chart(df, emphasis = c(-2, 0, 2))
  expect_no_error(view_chart(chart))
})

# === view_chart returns invisible chart for piping ===

test_that("view_chart returns chart invisibly", {
  df <- data.frame(date = as.Date("2020-01-01") + 0:4, value = 1:5)
  chart <- prep_chart(df)
  result <- view_chart(chart)
  expect_identical(result, chart)
  expect_invisible(view_chart(chart))
})

# === view_chart axis break with clipped y-axis ===

test_that("view_chart draws axis break for clipped y-axis", {
  df <- data.frame(
    date = seq(as.Date("2020-01-01"), by = "month", length.out = 12),
    value = 95 + cumsum(rnorm(12, 0.5, 1))
  )
  chart <- prep_chart(df)
  expect_no_error(view_chart(chart))
})

# === view_chart with single series hides legend ===

test_that("view_chart hides legend for single series", {
  df <- data.frame(date = as.Date("2020-01-01") + 0:4, value = 1:5)
  chart <- prep_chart(df, title = "Single series")
  # should not error — legend skipped for 1 series
  expect_no_error(view_chart(chart))
})

# === view_chart combo chart ===

test_that("view_chart renders combo bar+line chart", {
  df <- data.frame(
    date = seq(as.Date("2018-01-01"), by = "year", length.out = 7),
    Growth = c(2.1, 3.4, -5.2, 8.1, 4.2, 1.8, 0.5),
    Trend = c(2.5, 2.8, -1.0, 3.5, 3.0, 2.2, 1.5)
  )
  chart <- prep_chart(df, type = c("bar", "line"))
  expect_no_error(view_chart(chart))
})

test_that("view_chart handles single-row data without error", {
  df <- data.frame(
    date = as.Date("2026-01-01"),
    A = 16, B = 34, C = 36, D = 10, E = 19
  )
  chart <- prep_chart(df, type = "bar")
  expect_no_error(view_chart(chart))
})

test_that("view_chart handles single-row long format without error", {
  df <- data.frame(
    date = rep(as.Date("2026-01-01"), 3),
    value = c(10, 20, 30),
    category = c("A", "B", "C")
  )
  chart <- prep_chart(df, type = "bar")
  expect_no_error(view_chart(chart))
})

test_that("view_chart handles single-row single-series data", {
  df <- data.frame(date = as.Date("2026-01-01"), value = 42)
  chart <- prep_chart(df, type = "bar")
  expect_no_error(view_chart(chart))
})
