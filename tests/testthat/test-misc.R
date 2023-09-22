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





