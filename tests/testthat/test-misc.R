test_that("range finding works ", {
  expect_true(in_range(1, range(1, 2)))
  expect_error(in_range(1, 2))
  expect_error(in_range(1, c(2,1)))
  expect_false(in_range_strict(1, range(1, 2)))
  expect_error(in_range_strict(1, 2))
  expect_error(in_range_strict(1, c(2,1)))
  expect_equal(first_up("in"), "In")
  expect_equal(first_up("iN"), "IN")
  expect_equal(find_pretty_ylim(c(1, 3, 10))$ylim, c(0, 12))
  expect_equal(find_pretty_ylim(c(1, 3, 10))$y_breaks, c(0, 2,4,6,8,10, 12))
  expect_equal(find_pretty_ylim(c(1, 2, 3))$ylim, c(0.5, 3.5))
  expect_equal(0, nrow(apply_xlims(data.frame(period = as.Date("2010/01/01"),
                                              period_id = as.Date("2010/01/01"),
                                              value = 1))))
  expect_equal(1, nrow(apply_xlims(data.frame(period = as.Date("2011/02/01"),
                                              period_id = as.Date("2011/02/01"),
                                              value = 1))))
  df <- data.frame(period = seq(as.Date("2015/1/1"), by="month", length.out = 12),
                   period_id = seq(as.Date("2015/1/1"), by="month", length.out = 12),
                   value = c(NA, NA, 1, NA, NA, 3:7, NA, NA),
                   raw = c(NA, NA, 1, NA, NA, 3:7, 8, NA))
  expect_equal(nrow(remove_head_tail_NAs(df)), 9)
  expect_equal(nrow(apply_xlims(df, xmax = as.Date("2015/08/01"))), 6)
  x <- read_csv_guess_encoding(test_path("testdata", "test_report_input.csv"))
  expect_equal(dim(x), c(46,13))
  expect_false(all_equal(df$raw))
  x <- list(data.frame(period_id = "2022"))
  expect_equal(get_interval(x), "A")
  x <- list(data.frame(period_id = "2022M02"))
  expect_equal(get_interval(x), "M")
  x <- list(data.frame(period_id = "2022Q01"))
  expect_equal(get_interval(x), "Q")
  x <- add_date_from_period_id(data.frame(period_id = c("2023M02","2023M03")))
  expect_equal(dim(x), c(2,2))
  expect_equal(x$period[1], as.Date("2023-02-01"))
  data_points <- list(
    data.frame(period_id = c("a", "b", "c"), period = c(5, 8, 3)),
    data.frame(period_id = c("d", "e", "f"), period = c(2, 7, 1)))
  expect_equal(get_max_period(data_points), "b")
  df <- data.frame(xmin = c("2010-01-01", "2010-01-01", NA, "2010-01-01"))
  expect_true(check_consistency_or_na(df$xmin))
  df2 <- data.frame(xmin = c("2010-01-01", "2010-01-02", NA, "2010-01-01"))
  expect_false(check_consistency_or_na(df2$xmin))
  x <- c(1,2,NA, NA)
  expect_true(check_uniqueness_or_na(x))
  x <- c(1,2,2, NA)
  expect_false(check_uniqueness_or_na(x))
})


dittodb::with_mock_db({
  con <- DBI::dbConnect(RPostgres::Postgres(),
                        dbname = "platform",
                        host = "localhost",
                        port = 5432,
                        user = "mzaloznik",
                        password = Sys.getenv("PG_local_MAJA_PSW"))
  DBI::dbExecute(con, "set search_path to test_platform")
  test_that("unit update works", {
    df <- read_csv_guess_encoding(testthat::test_path("testdata", "test_report_input4.csv"))
    x <- update_units(df, con)
    expect_equal(x$enota, c("Indeks (2015 = 100)", "Indeks (2015 = 100)",
                            "Odstotne točke", "%", "%", "%" ))
  })

})

test_that("get_date_from_period function works correctly", {

  # Testing for years
  expect_equal(get_date_from_period("2022"), lubridate::ymd("2022-07-01"))       # defaults to "middle"
  expect_equal(get_date_from_period("2022", "l"), lubridate::ymd("2022-01-01"))
  expect_equal(get_date_from_period("2022", "r"), lubridate::ymd("2022-12-31"))
  # Testing for quarters
  expect_equal(get_date_from_period("2022Q1"), lubridate::ymd("2022-02-15"))    # defaults to "middle"
  expect_equal(get_date_from_period("2022Q1", "l"), lubridate::ymd("2022-01-01"))
  expect_equal(get_date_from_period("2022Q1", "r"), lubridate::ymd("2022-03-31"))
  # Testing for months
  expect_equal(get_date_from_period("2022M04"), lubridate::ymd("2022-04-16"))   # defaults to "middle"
  expect_equal(get_date_from_period("2022M04", "l"), lubridate::ymd("2022-04-01"))
  expect_equal(get_date_from_period("2022M04", "r"), lubridate::ymd("2022-04-30"))
  # test middles match
  expect_equal(get_date_from_period("2023M02"), get_date_from_period("2023Q1"))
  expect_equal(get_date_from_period("2023M05"), get_date_from_period("2023Q2"))
  expect_equal(get_date_from_period("2023M08"), get_date_from_period("2023Q3"))
  expect_equal(get_date_from_period("2023M11"), get_date_from_period("2023Q4"))
  # Testing for invalid format
  expect_error(get_date_from_period("invalid_format"), "Invalid period format")

  expect_equal(replace_period_id_column(data.frame(period_id = "2023M02"))$date[1],
               as.Date("2023-02-15"))
})


test_that("text wrapping works correctly", {
  plot(c(0,1), c(0,1))
  user_x <- 0.1
  title <- "test me"
  par(ps=12)
  wrapped_title <- wrap_title(title, user_x, font = 2, family = "sans")
  expect_equal(wrapped_title[[1]], "test\nme")
  expect_equal(wrapped_title[[2]],2)
  par(ps=9)
  wrapped_title <- wrap_title(title, user_x, font = 2, family = "sans")
  expect_equal(wrapped_title[[1]], "test me")
  expect_true(wrap_title("")[[2]] == 0)
})


test_that("legend lines work correctly", {
  config <- readRDS(testthat::test_path("testdata", "config.rds"))
  x <- get_legend_lines(config$series, config$legend_columns)
  expect_equal(x, 1)
  x <- get_legend_lines(config$series, 1)
  expect_equal(x, 2)
  expect_equal(get_legend_lines("s1", 1), 0)
  expect_equal(get_legend_lines("s1", 2), 0)
  expect_equal(get_legend_lines(c("s1", "s2"), 1), 2)
  expect_equal(get_legend_lines(c("s1", "s2"), 2), 1)
  expect_equal(get_legend_lines(c("s1", "s2"), 3), 1)
  expect_equal(get_legend_lines(c("s1", "s2", "s3"), 2), 2)
  expect_equal(get_legend_lines(c("s1", "s2", "s3"), 3), 1)
})

test_that("year_squisher_medium works correctly", {
  expect_equal(year_squisher_medium(1990:1995), c("1990", "91", "92", "93", "94", "95"))
  expect_equal(year_squisher_medium(c(2000)), c("2000"))
})

test_that("year_squisher_extra works correctly", {
  expect_equal(year_squisher_extra(1990:2000), c("1990", NA, NA, NA, NA, "1995", NA, NA, NA, NA, "2000"))
})

test_that("year_squisher works correctly", {
  expect_equal(year_squisher(1990:1995, extra = FALSE), year_squisher_medium(1990:1995))
  expect_equal(year_squisher(1990:2000, extra = TRUE), year_squisher_extra(1990:2000))
})

test_that("last_year_complete_series works correctly", {
  # Complete timeseries for a year on a monthly basis
  df_complete_12 <- data.frame(date = seq(as.Date("2022-01-01"), as.Date("2022-12-01"), by = "month"))
  # Complete timeseries for a year on a quarterly basis
  df_complete_4Q <- data.frame(date = seq(as.Date("2022-01-01"), as.Date("2022-10-01"), by = "quarter"))
  # Incomplete timeseries (missing December)
  df_incomplete <- data.frame(date = seq(as.Date("2022-01-01"), as.Date("2022-11-01"), by = "month"))
  # annual
  df_complete_A <- data.frame(date = seq(as.Date("2022-01-01"), as.Date("2024-10-01"), by = "year"))

  expect_true(last_year_complete_series(df_complete_12, 2022))
  expect_true(last_year_complete_series(df_complete_4Q, 2022))
  expect_false(last_year_complete_series(df_incomplete, 2022))
  expect_true(last_year_complete_series(df_complete_A, 2024))
  expect_true(last_year_complete(list(df_complete_12, df_incomplete)))
  expect_false(last_year_complete(list(df_incomplete, df_incomplete)))

})

# Test for annual data
test_that("Date intervals are correctly identified", {
  annual_dates <- seq(as.Date("2000-01-01"), by = "year", length.out = 10)
  df <- data.frame(date = annual_dates)
  expect_equal(determine_interval(df), "A")
  quarterly_dates <- seq(as.Date("2000-01-01"), by = "quarter", length.out = 10)
  df <- data.frame(date = quarterly_dates)
  expect_equal(determine_interval(df), "Q")
  monthly_dates <- seq(as.Date("2000-01-01"), by = "month", length.out = 10)
  df <- data.frame(date = monthly_dates)
  expect_equal(determine_interval(df), "M")
  irregular_dates <- c(as.Date("2000-01-01"), as.Date("2000-02-10"), as.Date("2000-03-15"))
  df <- data.frame(date = irregular_dates)
  expect_equal(determine_interval(df), NA)
  list_a_only <- list(data.frame(date = seq(as.Date("2022-01-01"), as.Date("2024-10-01"), by = "year")),
                      data.frame(date = seq(as.Date("2019-06-01"), as.Date("2024-06-01"), by = "year")))
  list_a_not <- list(data.frame(date = seq(as.Date("2022-01-01"), as.Date("2024-10-01"), by = "quarter")),
                     data.frame(date = seq(as.Date("2019-06-01"), as.Date("2024-06-01"), by = "year")))
  expect_true(only_annual_intervals(list_a_only))
  expect_false(only_annual_intervals(list_a_not))
})

test_that("first and alst day of year are returned correctly", {
  expect_equal(first_day_of_year("2023-03-01"), as.Date("2023-01-01"))
  expect_equal(last_day_of_year("2023-03-01"), as.Date("2023-12-31"))
  expect_equal(shift_dates_by_six_months(as.Date("2023-03-01")), as.Date("2023-09-01"))
  curr_par <- par("mgp")
  par_mgp(c(1,2,3))
  expect_equal(par("mgp"), curr_par)
})

test_that("Dataframe with most recent date is returned", {
  df1 <- data.frame(date = as.Date("2020-01-01"))
  df2 <- data.frame(date = as.Date("2021-01-01"))
  df3 <- data.frame(date = as.Date("2022-01-01"))
  df4 <- data.frame(datum = as.Date("2022-01-01"))
  datapoints <- list(df1, df2, df3)
  expect_equal(get_most_recent_dataframe(datapoints), df3)
  df1 <- data.frame(date = as.Date("2020-01-01"))
  datapoints <- list(df1)
  expect_equal(get_most_recent_dataframe(datapoints), df1)
  datapoints <- list()
  expect_error(get_most_recent_dataframe(datapoints))
  df1 <- data.frame(date = as.Date("2020-01-01"))
  df2 <- data.frame(not_date ="2021-01-01")
  datapoints <- list(df1, df2)
  expect_error(get_most_recent_dataframe(datapoints))
  df1 <- data.frame(date = as.Date("2022-01-01"))
  df2 <- data.frame(date = as.Date("2022-01-01"))
  datapoints <- list(df1, df2)
  expect_equal(get_most_recent_dataframe(datapoints), df1)
  df1 <- data.frame(date = as.Date(c("2022-01-01", "2022-02-01")))
  df2 <- data.frame(date = as.Date(c("2022-01-01", "2022-04-01")))
  datapoints <- list(df1, df2)
  expect_equal(get_most_recent_interval(datapoints), "Q")
  datapoints <- list(df1, df4)
  expect_error(get_most_recent_dataframe(datapoints))
  expect_equal(quarterly_label(as.Date(c("2022-01-01", "2022-04-01"))), c("Q1-22", "Q2-22"))
  })



test_that("Basic functionality of smallest gap caplulations", {
  plot(1,1)
  x_positions <- c(0.6, 1, 1.4)
  x_labels <- c("Label1", "Label2", "Label3")
  expect_type(calculate_smallest_gap(x_positions, x_labels), "double")
  x_positions <- c(1, 1.01, 1.02)
  x_labels <- c("Overlap1", "Overlap2", "Overlap3")
  expect_true(calculate_smallest_gap(x_positions, x_labels) < 0)
  x_positions <- c(1, 1.1, 1.2)
  x_labels <- c("A", "B", "C")
  gap = calculate_smallest_gap(x_positions, x_labels)
  expect_true(gap > 0 && is.finite(gap))
  x_positions <- c(1, 2)
  x_labels <- c("Mismatch1", "Mismatch2", "Mismatch3")
  expect_error(calculate_smallest_gap(x_positions, x_labels))
})



# Mock data for config and y_axis
config <- list(y_axis_label = "EUR")
y_axis <- list(ylim = c(0, 2000000), y_breaks = c(0, 500000, 1000000, 1500000, 2000000))

# Test with EUR values not exceeding a million
test_that("EUR values under a million", {
  config$series[[1]]$unit <- "EUR"
  config$series[[1]]$mio_eur <- TRUE
  y_axis$ylim <- c(0, 800000)
  y_axis$y_breaks <- c(0, 200000, 400000, 600000, 800000)
  result <- left_axis_label_width(config, y_axis)
  expect_equal(config$y_axis_label, "EUR")
  expect_true(all(result$axis_labels < 1000000))
})

# Test with EUR values exceeding a million
test_that("EUR values over a million", {
  config$series[[1]]$unit <- "EUR"
  config$series[[1]]$mio_eur <- TRUE
  y_axis$ylim <- c(0, 2000000)
  y_axis$y_breaks <- c(0, 500000, 1000000, 1500000, 2000000)
  result <- left_axis_label_width(config, y_axis)
  expect_equal(result$unit, "Mio EUR")
  expect_true(all(result$axis_labels <= 2))
})

# Test return structure
test_that("Return structure is correct", {
  config$series[[1]]$unit <- "EUR"
  config$series[[1]]$mio_eur <- TRUE
  result <- left_axis_label_width(config, y_axis)
  expect_type(result, "list")
  expect_true(all(c("unit", "axis_labels", "axis_positions", "y_lab_lines") %in% names(result)))
})

# Test for non-EUR label handling
test_that("Non-EUR label handling", {
  config$series[[1]]$unit <- "USD"
  config$series[[1]]$mio_eur <- TRUE
  y_axis$ylim <- c(0, 2000000)
  y_axis$y_breaks <- c(0, 500000, 1000000, 1500000, 2000000)
  result <- left_axis_label_width(config, y_axis)
  expect_equal(result$unit, "USD")
  expect_true(all(result$axis_labels == y_axis$y_breaks))
})

# Test with no NAs
test_that("Function works with no NAs", {
  x_positions <- 1:5
  x_labels <- letters[1:5]
  result <- filter_na_labels(x_positions, x_labels)
  expect_equal(result$x_positions, 1:5)
  expect_equal(result$x_labels, letters[1:5])
})

# Test with some NAs
test_that("Function filters out NAs correctly", {
  x_positions <- 1:5
  x_labels <- c("a", NA, "c", NA, "e")
  result <- filter_na_labels(x_positions, x_labels)
  expect_equal(result$x_positions, c(1, 3, 5))
  expect_equal(result$x_labels, c("a", "c", "e"))
})

# Test with all NAs
test_that("Function handles all NAs", {
  x_positions <- 1:5
  x_labels <- rep(NA, 5)
  result <- filter_na_labels(x_positions, x_labels)
  expect_equal(result$x_positions, numeric(0))
  expect_equal(result$x_labels, logical(0))
})

# Test with empty vectors
test_that("Function handles empty vectors", {
  x_positions <- numeric(0)
  x_labels <- character(0)
  result <- filter_na_labels(x_positions, x_labels)
  expect_equal(result$x_positions, numeric(0))
  expect_equal(result$x_labels, character(0))
})

test_that("legend works ", {
  p <- function() {
    plot(1,1)
    legend_mz2(legend = "test123")}
  vdiffr::expect_doppelganger("legend test", p)
})

test_that("y axis labeling works ", {
  p <- function() {
    plot(1,1)
    left_axis_labels("axis title", c(0.9, 1.1),  c(0.9, 1.1), 1)}
  vdiffr::expect_doppelganger("y axis test", p)
})

test_that("get data values works for stacked bar", {
  config <- list(stacked = TRUE,
                 series = list(list( type = "bar"), list( type = "bar")))
  datapoints <- list(data.frame(date = c("2023-01-01", "2023-02-01", "2023-03-01", "2023-04-01"),
                              value = c(2,3,4,5)),
                   data.frame(date = c("2023-01-01", "2023-02-01", "2023-03-01", "2023-04-01"),
                              value = c(2,3,4,5)))
  expect_equal(get_data_values(datapoints, config), c(2,3,4,5,2,3,4,5,4,6,8,10,0))
})



