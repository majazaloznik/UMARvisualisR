test_that("transformations work", {
  df <- data.frame(value = c(1:10))
  expect_equal(dim(rolling_average(df)), c(10,2))
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



dittodb::with_mock_db({
  con <- DBI::dbConnect(RPostgres::Postgres(),
                        dbname = "sandbox",
                        host = "localhost",
                        port = 5432,
                        user = "mzaloznik",
                        password = Sys.getenv("PG_local_MAJA_PSW"))
  DBI::dbExecute(con, "set search_path to test_platform")

  test_that("mock tests for prepping data for plot", {
    out <- prep_single_line(1625, con)
    expect_equal(length(out), 7)
    expect_true("interval" %in% names(out))
    expect_equal(dim(out[[1]]), c(273, 3))
    out <- prep_single_line(1625, con, interval = "Q")
    expect_equal(length(out), 7)
  })
})


