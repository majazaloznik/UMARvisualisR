

test_that("x-axis limits are calculated correctly", {
  dittodb::with_mock_db({
    con <- DBI::dbConnect(RPostgres::Postgres(),
                          dbname = "platform",
                          host = "localhost",
                          port = 5432,
                          user = "mzaloznik",
                          password = Sys.getenv("PG_local_MAJA_PSW"))
    DBI::dbExecute(con, "set search_path to test_platform")
    x <- openxlsx::read.xlsx(test_path("testdata", "pub_test_df.xlsx"), sheet = "Sheet22")[1:2,]
    results <- prep_data(x, con, "test_platform")
    datapoints <- cut_to_x_range(results$datapoints, results$config)
    xlims <- get_x_lims(datapoints)
    expect_equal(xlims, structure(c(min_date = 14625, max_date = 19539), class = "Date"))
    results$config$xmax <- "2022-01-01"
    datapoints <- cut_to_x_range(results$datapoints, results$config)
    xlims <- get_x_lims(datapoints)
    expect_equal(xlims, structure(c(min_date = 14625, max_date = 18977), class = "Date"))
    # x <- openxlsx::read.xlsx(test_path("testdata", "pub_test_df.xlsx"), sheet = "Sheet26")
    # results <- prep_data(x, con, "test_platform")
    # datapoints <- cut_to_x_range(results$datapoints, results$config)
    # xlims <- get_x_lims(datapoints)
    # expect_equal(xlims, structure(c(min_date = 13194, max_date = 20043), class = "Date"))
  })
})


test_that("top margins are calculated correctly", {
  config <- list(
    series = list(
      list(legend_txt_si = "a"),
      list(legend_txt_si = "b"),
      list(legend_txt_si = "c")
    ),
    legend_columns = 2,
    title = "Very very long title that just keeps going on and on and hopefully takes up at least two lines for me to be able to test this shit properly."
  )
  top <- get_top_margin_and_title(config, 10)
  expect_true(top[[1]] > 3.5 & top[[1]] < 3.6)
  expect_equal(top[[2]], 1.85)
})

test_that("empty plot is drawn correctly", {
  p <- function() empty_plot(c(10, 100), list(ylim = c(-10, 110), y_breaks = seq(-10, 110, 10)), "Index")
  vdiffr::expect_doppelganger("empty_plot", p)
})

test_that("bar plot is drawn correctly", {
  config <- list(series = list(list(type="bar", colour = umar_cols()[1], legend_txt_si = "serija 1",
                                    legend_txt_en = "series 1"),
                               list(type="bar", colour = umar_cols()[2],  legend_txt_si = "serija 2",
                                    legend_txt_en = "series 2")),
                 y_axis_label = "leva os",
                 stacked = TRUE)
  datapoints <- list(data.frame(date = c("2023-01-01", "2023-02-01", "2023-03-01", "2023-04-01"),
                                value = c(2,3,4,5)),
                     data.frame(date = c("2023-01-01", "2023-02-01", "2023-03-01", "2023-04-01"),
                                value = c(2,3,4,5)))
  y_axis <- find_pretty_ylim(get_data_values(datapoints, config))
  p <- function() base_barplot(datapoints, config,y_axis)
  vdiffr::expect_doppelganger("bar_plot", p)

  config$stacked <- FALSE
  p <- function() base_barplot(datapoints, config,y_axis)
  vdiffr::expect_doppelganger("bar_plot grouped", p)


  config$stacked <- TRUE
  config$y_axis_label <- "Indeks"
  datapoints <- list(data.frame(date = c("2023-01-01", "2023-02-01", "2023-03-01", "2023-04-01"),
                                value = c(52,63,74,-5)),
                     data.frame(date = c("2023-01-01", "2023-02-01", "2023-03-01", "2023-04-01"),
                                value = c(22,33,44,-2)))
  y_axis <- find_pretty_ylim(get_data_values(datapoints, config))
  p <- function() base_barplot(datapoints, config,y_axis)
  vdiffr::expect_doppelganger("bar_plot w index", p)
})

test_that("x axis tickmarks", {
  plot.new()
  config <- list(series = list(list(type="bar", colour = umar_cols()[1], legend_txt_si = "serija 1",
                                    legend_txt_en = "series 1"),
                               list(type="bar", colour = umar_cols()[2],  legend_txt_si = "serija 2",
                                    legend_txt_en = "series 2")),
                 y_axis_label = "leva os",
                 stacked = TRUE)
  datapoints <- list(data.frame(date = c("2019-01-01", "2020-01-01", "2021-01-01", "2022-01-01"),
                                value = c(52,63,74,-5)),
                     data.frame(date = c("2019-01-01", "2020-01-01", "2021-01-01", "2022-01-01"),
                                value = c(22,33,44,-2)))
  config$x_sub_annual <- FALSE
  out <- x_axis_lims_tickmarks(datapoints, config)
  expect_equal(out$tickmarks, as.Date(c( "2019-01-01", "2020-01-01", "2021-01-01", "2022-01-01")))

  datapoints <- list(data.frame(date = c("2020-01-01", "2020-04-01", "2020-07-01", "2020-10-01"),
                                value = c(52,63,74,-5)),
                     data.frame(date = c("2020-01-01", "2020-04-01", "2020-07-01", "2020-10-01"),
                                value = c(22,33,44,-2)))
  out <- x_axis_lims_tickmarks(datapoints, config)
  expect_equal(out$tickmarks, as.Date(c( "2020-01-01", "2020-04-01", "2020-07-01", "2020-10-01",
                                         "2021-01-01")))
  datapoints <- list(data.frame(date = c("2020-01-01", "2020-04-01", "2020-07-01", "2020-10-01", "2021-01-01"),
                                value = c(52,63,74,-5, 2)),
                     data.frame(date = c("2020-01-01", "2020-04-01", "2020-07-01", "2020-10-01", "2021-01-01"),
                                value = c(22,33,44,-2, 2)))
  out <- x_axis_lims_tickmarks(datapoints, config)
  expect_equal(out$tickmarks, as.Date(c( "2020-01-01", "2020-04-01", "2020-07-01", "2020-10-01",
                                         "2021-01-01", "2021-04-01")))
  # different lengths
  datapoints <- list(data.frame(date = c("2020-01-01", "2020-04-01", "2020-07-01", "2020-10-01", "2021-01-01"),
                                value = c(52,63,74,-5, 2)),
                     data.frame(date = c("2020-01-01", "2020-04-01", "2020-07-01", "2020-10-01"),
                                value = c(22,33,44,-2)))
  out <- x_axis_lims_tickmarks(datapoints, config)
  expect_equal(out$tickmarks, as.Date(c( "2020-01-01", "2020-04-01", "2020-07-01", "2020-10-01",
                                         "2021-01-01", "2021-04-01")))
})

test_that("x axis params", {
  config <- list(series = list(list(type="bar", colour = umar_cols()[1], legend_txt_si = "serija 1",
                                    legend_txt_en = "series 1"),
                               list(type="bar", colour = umar_cols()[2],  legend_txt_si = "serija 2",
                                    legend_txt_en = "series 2")),
                 y_axis_label = "leva os",
                 x_sub_annual = FALSE,
                 stacked = TRUE)
  datapoints <- list(data.frame(date = c("2022-07-01", "2022-10-01", "2023-01-01", "2023-04-01"),
                                value = c(2,3,4,5)),
                     data.frame(date = c("2019-01-01", "2020-01-01", "2021-01-01", "2022-01-01"),
                                value = c(2,3,4,5)))
  out <- x_axis_lims_tickmarks(datapoints, config)
  result <- x_axis_label_params(datapoints, config, out$tickmarks, out$x_lims, bar = FALSE, x_values = NULL, language = "si")
  expect_equal(result$x_labels, c("2019", "2020",  "2021", "2022", "Q2-23"))

  datapoints <- list(data.frame(date = c("2022-01-01", "2022-04-01", "2022-07-01", "2022-10-01" ),
                                value = c(2,3,4,5)),
                     data.frame(date = c("2019-01-01", "2020-01-01", "2021-01-01", "2022-01-01"),
                                value = c(2,3,4,5)))
  out <- x_axis_lims_tickmarks(datapoints, config)
  result <- x_axis_label_params(datapoints, config, out$tickmarks, out$x_lims, bar = FALSE, x_values = NULL, language = "si")
  expect_equal(result$x_labels, c("2019", "2020",  "2021", "2022"))

  datapoints <- list(data.frame(date = c("2019-01-01", "2020-01-01", "2021-01-01", "2022-01-01" ),
                                value = c(2,3,4,5)),
                     data.frame(date = c("2019-01-01", "2020-01-01", "2021-01-01", "2022-01-01"),
                                value = c(2,3,4,5)))
  out <- x_axis_lims_tickmarks(datapoints, config)
  result <- x_axis_label_params(datapoints, config, out$tickmarks, out$x_lims, bar = FALSE, x_values = NULL, language = "si")
  expect_equal(result$x_labels, c("2019", "2020",  "2021", "2022"))
  datapoints <- list(data.frame(date = c("2019-01-01", "2020-01-01", "2021-01-01", "2022-01-01" ),
                                value = c(2,3,4,5)),
                     data.frame(date = c("2022-01-01", "2022-02-01", "2022-03-01", "2022-04-01"),
                                value = c(2,3,4,5)))
  out <- x_axis_lims_tickmarks(datapoints, config)
  result <- x_axis_label_params(datapoints, config, out$tickmarks, out$x_lims, bar = FALSE, x_values = NULL, language = "si")
  expect_equal(result$x_labels, c("2019", "2020",  "2021", "2022"))
})
# === x_axis_lims_tickmarks - interval types ===

test_that("quarterly data under 2 years gets quarterly tickmarks", {
  config <- list(x_sub_annual = FALSE)
  datapoints <- list(data.frame(
    date = as.Date(c("2020-01-15", "2020-04-15", "2020-07-15", "2020-10-15", "2021-01-15")),
    value = 1:5
  ))
  out <- x_axis_lims_tickmarks(datapoints, config)
  expect_equal(out$interval_type, "quarterly")
  expect_equal(out$tickmarks[1], as.Date("2020-01-01"))
  expect_equal(out$tickmarks[length(out$tickmarks)], as.Date("2021-04-01"))
})

test_that("quarterly data over 2 years gets annual tickmarks", {
  config <- list(x_sub_annual = FALSE)
  datapoints <- list(data.frame(
    date = seq(as.Date("2018-01-15"), by = "quarter", length.out = 20),
    value = 1:20
  ))
  out <- x_axis_lims_tickmarks(datapoints, config)
  expect_equal(out$interval_type, "annual")
})

test_that("monthly data under 2 years gets monthly tickmarks", {
  config <- list(x_sub_annual = FALSE)
  datapoints <- list(data.frame(
    date = seq(as.Date("2025-01-15"), by = "month", length.out = 8),
    value = 1:8
  ))
  out <- x_axis_lims_tickmarks(datapoints, config)
  expect_equal(out$interval_type, "monthly")
})

test_that("monthly data over 2 years gets annual tickmarks", {
  config <- list(x_sub_annual = FALSE)
  datapoints <- list(data.frame(
    date = seq(as.Date("2020-01-15"), by = "month", length.out = 36),
    value = 1:36
  ))
  out <- x_axis_lims_tickmarks(datapoints, config)
  expect_equal(out$interval_type, "annual")
})

test_that("daily data under 2 months gets daily tickmarks", {
  config <- list(x_sub_annual = FALSE)
  datapoints <- list(data.frame(
    date = seq(as.Date("2026-04-01"), by = "day", length.out = 30),
    value = 1:30
  ))
  out <- x_axis_lims_tickmarks(datapoints, config)
  expect_equal(out$interval_type, "daily")
})

test_that("daily data over 2 months gets monthly tickmarks", {
  config <- list(x_sub_annual = FALSE)
  datapoints <- list(data.frame(
    date = seq(as.Date("2026-01-01"), by = "day", length.out = 100),
    value = 1:100
  ))
  out <- x_axis_lims_tickmarks(datapoints, config)
  expect_equal(out$interval_type, "monthly")
})

test_that("annual data always gets annual tickmarks regardless of span", {
  config <- list(x_sub_annual = FALSE)
  datapoints <- list(data.frame(
    date = as.Date(c("2023-01-01", "2024-01-01", "2025-01-01")),
    value = 1:3
  ))
  out <- x_axis_lims_tickmarks(datapoints, config)
  expect_equal(out$interval_type, "annual")
})

test_that("daily tickmark spacing adapts to span", {
  config <- list(x_sub_annual = FALSE)
  # under 14 days - daily
  dp_short <- list(data.frame(
    date = seq(as.Date("2026-05-01"), by = "day", length.out = 10),
    value = 1:10
  ))
  out <- x_axis_lims_tickmarks(dp_short, config)
  expect_true(all(diff(out$tickmarks) == 1))

  # 15-28 days - every 3 days
  dp_mid <- list(data.frame(
    date = seq(as.Date("2026-05-01"), by = "day", length.out = 20),
    value = 1:20
  ))
  out <- x_axis_lims_tickmarks(dp_mid, config)
  expect_true(all(diff(out$tickmarks[1:(length(out$tickmarks)-1)]) == 3))

  # 29-60 days - weekly
  dp_long <- list(data.frame(
    date = seq(as.Date("2026-04-01"), by = "day", length.out = 45),
    value = 1:45
  ))
  out <- x_axis_lims_tickmarks(dp_long, config)
  expect_true(all(diff(out$tickmarks[1:(length(out$tickmarks)-1)]) == 7))
})

test_that("monthly tickmarks end at data endpoint, not month end", {
  config <- list(x_sub_annual = FALSE)
  datapoints <- list(data.frame(
    date = seq(as.Date("2025-01-15"), by = "month", length.out = 6),
    value = 1:6
  ))
  out <- x_axis_lims_tickmarks(datapoints, config)
  expect_true(out$x_lims[2] >= as.Date("2025-06-15"))
})

# === x_axis_label_params - quarterly ===

test_that("quarterly labels are positioned between tickmarks", {
  plot.new()
  config <- list(x_sub_annual = FALSE)
  datapoints <- list(data.frame(
    date = as.Date(c("2020-01-15", "2020-04-15", "2020-07-15", "2020-10-15")),
    value = 1:4
  ))
  out <- x_axis_lims_tickmarks(datapoints, config)
  result <- x_axis_label_params(datapoints, config, out$tickmarks, out$x_lims,
                                bar = FALSE, x_values = NULL,
                                interval_type = "quarterly")
  expect_equal(length(result$x_labels), length(out$tickmarks) - 1)
  # labels should be between consecutive tickmarks
  for (i in seq_along(result$x_positions)) {
    expect_true(result$x_positions[i] > out$tickmarks[i])
    expect_true(result$x_positions[i] < out$tickmarks[i + 1])
  }
})

# === x_axis_label_params - monthly ===

test_that("monthly labels have year on first and January", {
  plot.new()
  config <- list(x_sub_annual = FALSE)
  datapoints <- list(data.frame(
    date = seq(as.Date("2025-10-15"), by = "month", length.out = 6),
    value = 1:6
  ))
  out <- x_axis_lims_tickmarks(datapoints, config)
  result <- x_axis_label_params(datapoints, config, out$tickmarks, out$x_lims,
                                bar = FALSE, x_values = NULL,
                                interval_type = "monthly")
  # first label should contain year
  expect_true(grepl("25", result$x_labels[1]))
  # find January label — should contain year
  jan_idx <- which(lubridate::month(result$x_positions) == 1)
  if (length(jan_idx) > 0) {
    expect_true(grepl("26", result$x_labels[jan_idx[1]]))
  }
  # non-January, non-first labels should NOT contain year
  other_idx <- setdiff(seq_along(result$x_labels), c(1, jan_idx))
  if (length(other_idx) > 0) {
    expect_false(any(grepl("\\d{2}$", result$x_labels[other_idx])))
  }
})

test_that("monthly data with short span still gets monthly tickmarks", {
  config <- list(x_sub_annual = FALSE)
  datapoints <- list(data.frame(
    date = as.Date(c("2026-04-15", "2026-05-15")),
    value = 1:2
  ))
  out <- x_axis_lims_tickmarks(datapoints, config)
  expect_equal(out$interval_type, "monthly")
})
# === x_axis_label_params - daily ===

test_that("daily labels have month on first and month changes", {
  plot.new()
  config <- list(x_sub_annual = FALSE)
  datapoints <- list(data.frame(
    date = seq(as.Date("2026-04-25"), by = "day", length.out = 14),
    value = 1:14
  ))
  out <- x_axis_lims_tickmarks(datapoints, config)
  result <- x_axis_label_params(datapoints, config, out$tickmarks, out$x_lims,
                                bar = FALSE, x_values = NULL,
                                interval_type = "daily")
  # first label should contain month and year
  expect_true(grepl("apr", result$x_labels[1], ignore.case = TRUE) ||
                grepl("26", result$x_labels[1]))
  # labels at month change should also have month
  may_idx <- which(lubridate::month(result$x_positions) == 5 &
                     c(TRUE, diff(lubridate::month(result$x_positions)) != 0))
  if (length(may_idx) > 0) {
    expect_true(grepl("maj|may", result$x_labels[may_idx[1]], ignore.case = TRUE))
  }
})

# === x_axis_label_params - annual with interval_type passed ===

test_that("annual label params still work with interval_type argument", {
  plot.new()
  config <- list(x_sub_annual = FALSE)
  datapoints <- list(
    data.frame(date = as.Date(c("2019-01-15", "2020-01-15", "2021-01-15", "2022-01-15")),
               value = c(2, 3, 4, 5)),
    data.frame(date = as.Date(c("2019-01-15", "2020-01-15", "2021-01-15", "2022-01-15")),
               value = c(2, 3, 4, 5))
  )
  out <- x_axis_lims_tickmarks(datapoints, config)
  result <- x_axis_label_params(datapoints, config, out$tickmarks, out$x_lims,
                                bar = FALSE, x_values = NULL,
                                interval_type = "annual")
  expect_equal(result$x_labels, c("2019", "2020", "2021", "2022"))
})

# === calculate_smallest_gap edge cases ===

test_that("calculate_smallest_gap returns Inf for single label", {
  plot.new()
  expect_equal(calculate_smallest_gap(1, "label"), Inf)
})

test_that("calculate_smallest_gap errors on mismatched lengths", {
  plot.new()
  expect_error(calculate_smallest_gap(1:3, c("a", "b")),
               "Length of x_positions and x_labels must be the same")
})

# === find_pretty_ylim ===

test_that("find_pretty_ylim adds padding when data near limits", {
  result <- find_pretty_ylim(c(5.01, 9.99))
  expect_true(result$ylim[1] < 5)
  expect_true(result$ylim[2] > 10)
})

test_that("find_pretty_ylim preserves zero lower bound", {
  result <- find_pretty_ylim(c(0, 5))
  expect_equal(result$ylim[1], 0)
})

test_that("find_pretty_ylim handles negative values", {
  result <- find_pretty_ylim(c(-5, 5))
  expect_true(result$ylim[1] < -5)
  expect_true(result$ylim[2] > 5)
})

# === cut_to_x_range ===

test_that("cut_to_x_range respects xmin only", {
  config <- list(xmin = as.Date("2020-06-01"), xmax = NULL)
  datapoints <- list(data.frame(
    date = seq(as.Date("2020-01-01"), by = "month", length.out = 12),
    value = 1:12
  ))
  result <- cut_to_x_range(datapoints, config)
  expect_true(min(result[[1]]$date) >= as.Date("2020-06-01"))
})

test_that("cut_to_x_range respects xmax only", {
  config <- list(xmin = NULL, xmax = as.Date("2020-06-01"))
  datapoints <- list(data.frame(
    date = seq(as.Date("2020-01-01"), by = "month", length.out = 12),
    value = 1:12
  ))
  result <- cut_to_x_range(datapoints, config)
  expect_true(max(result[[1]]$date) <= as.Date("2020-06-01"))
})

test_that("cut_to_x_range ignores non-overlapping user range", {
  config <- list(xmin = as.Date("2025-01-01"), xmax = as.Date("2025-12-01"))
  datapoints <- list(data.frame(
    date = seq(as.Date("2020-01-01"), by = "month", length.out = 12),
    value = 1:12
  ))
  expect_message(result <- cut_to_x_range(datapoints, config), "xmin")
  expect_equal(nrow(result[[1]]), 12)
})

# === draw_lines with single line series on combo chart ===
### fix tomorrow!
test_that("draw_lines handles single line series in combo chart", {
  plot(1, 1, xlim = c(0, 10), ylim = c(0, 10))
  config <- list(series = list(
    list(type = "bar", colour = "#A10305"),
    list(type = "line", colour = "#4A6FB5", linestyle = "solid")
  ))
  datapoints <- list(
    data.frame(date = 1:4, value = c(2, 3, 4, 5)),
    data.frame(date = 1:4, value = c(3, 4, 5, 6))
  )
  x_values <- list(dates = as.Date(c("2020-01-01", "2020-04-01", "2020-07-01", "2020-10-01")),
                   midpoints = c(1, 3, 5, 7))
  # should not error — the drop = FALSE fix
  expect_no_error(draw_lines(datapoints, config, x_values = x_values))
})

# === only_annual_intervals with NA ===

test_that("only_annual_intervals returns FALSE for irregular data", {
  datapoints <- list(data.frame(
    date = as.Date(c("2026-01-05", "2026-01-12", "2026-01-20")),
    value = 1:3
  ))
  expect_false(only_annual_intervals(datapoints))
})

# === last_year_complete_series with irregular data ===

test_that("last_year_complete_series handles irregular interval", {
  df <- data.frame(
    date = as.Date(c("2026-01-05", "2026-01-12", "2026-02-03", "2026-03-15")),
    value = 1:4
  )
  result <- last_year_complete_series(df, 2026)
  expect_false(result)
})

# === mixed interval datapoints ===

test_that("x_axis_lims_tickmarks handles mixed intervals across series", {
  config <- list(x_sub_annual = FALSE)
  datapoints <- list(
    data.frame(date = seq(as.Date("2020-01-15"), by = "month", length.out = 12),
               value = 1:12),
    data.frame(date = seq(as.Date("2020-01-15"), by = "quarter", length.out = 4),
               value = 1:4)
  )
  # should not error
  out <- x_axis_lims_tickmarks(datapoints, config)
  expect_true(out$interval_type %in% c("annual", "monthly", "quarterly", "daily"))
})

# === interpolate_x ===

test_that("interpolate_x does linear interpolation correctly", {
  original_dates <- as.Date(c("2020-01-01", "2020-12-31"))
  x_values <- c(0, 100)
  new_dates <- as.Date("2020-07-01")
  result <- interpolate_x(original_dates, x_values, new_dates)
  # July 1 is roughly halfway through the year
  expect_true(result > 45 && result < 55)
})

test_that("interpolate_x handles endpoints exactly", {
  original_dates <- as.Date(c("2020-01-01", "2020-12-31"))
  x_values <- c(10, 50)
  expect_equal(interpolate_x(original_dates, x_values, as.Date("2020-01-01")), 10)
  expect_equal(interpolate_x(original_dates, x_values, as.Date("2020-12-31")), 50)
})


test_that("monthly tickmarks have equal spacing for regular monthly data", {
  config <- list(x_sub_annual = FALSE)
  datapoints <- list(data.frame(
    date = as.Date(c("2026-03-16", "2026-04-15")),
    value = 1:2
  ))
  out <- x_axis_lims_tickmarks(datapoints, config)
  expect_equal(out$interval_type, "monthly")
  # should have 3 tickmarks: Mar 1, Apr 1, May 1
  expect_equal(out$tickmarks, as.Date(c("2026-03-01", "2026-04-01", "2026-05-01")))
  # equal spacing
  diffs <- diff(out$tickmarks)
  expect_true(all(diffs >= 28 & diffs <= 31))
  # both data points should be inside the axis range
  expect_true(all(datapoints[[1]]$date >= out$x_lims[1]))
  expect_true(all(datapoints[[1]]$date <= out$x_lims[2]))
})

test_that("draw_forecast skips bar charts", {
  plot.new()
  plot.window(c(0, 10), c(0, 10))
  expect_invisible(UMARvisualisR:::draw_forecast(
    as.Date(c("2024-01-01", "2025-01-01")), bar = TRUE
  ))
})

test_that("draw_forecast skips NULL", {
  plot.new()
  plot.window(c(0, 10), c(0, 10))
  expect_invisible(UMARvisualisR:::draw_forecast(NULL, bar = FALSE))
})
