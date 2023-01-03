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
  test_that("multliline chart inputs are legit", {
    df <- read_csv_guess_encoding(test_path("testdata", "test_report_input.csv"))
    spl <- split(df, df$chart_no)
    expect_error(multi_checks(spl$`17`))
    expect_error(multi_checks(spl$`16`))
    expect_error(multi_checks(spl$`13`))
    expect_error(multi_checks(spl$`14`))
    expect_error(multi_checks(spl$`15`))
    expect_error(multi_checks(spl$`19`))
    expect_warning(multi_titles(spl$`18`))
    expect_warning(multi_checks(spl$`18`))
    expect_s3_class(multi_checks(spl$`18`), "data.frame")
  })
  test_that("mocks for prepping multiline data", {
    df <- read_csv_guess_encoding(test_path("testdata", "test_report_input.csv"))
    spl <- split(df, df$chart_no)
    out <- prep_multi_line(spl$`4`, con)
    expect_equal(length(out), 7)
    expect_equal(length(out[[1]]), 2)
    expect_equal(dim(out[[1]][[1]]), c(111,3))
    out <- prep_multi_line(spl$`1`, con)
    expect_equal(length(out), 7)
    expect_equal(length(out[[1]]), 1)
    expect_equal(dim(out[[1]][[1]]), c(111,3))
  })
})




