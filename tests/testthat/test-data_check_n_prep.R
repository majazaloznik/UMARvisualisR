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
    expect_error(multi_checks(spl$`17`, con))
    expect_error(multi_checks(spl$`16`, con))
    expect_error(multi_checks(spl$`13`, con))
    expect_error(multi_checks(spl$`14`, con))
    expect_error(multi_checks(spl$`15`, con))
    expect_error(multi_checks(spl$`19`, con))
    expect_warning(multi_titles(spl$`18`, con))
    expect_warning(multi_checks(spl$`18`, con))
    expect_s3_class(multi_checks(spl$`18`, con), "data.frame")
  })
  test_that("mocks for prepping multiline data", {
    df <- read_csv_guess_encoding(test_path("testdata", "test_report_input.csv"))
    spl <- split(df, df$chart_no)
    out <- prep_multi_line(spl$`4`, con)
    expect_equal(length(out), 8)
    expect_equal(length(out[[1]]), 2)
    expect_equal(dim(out[[1]][[1]]), c(111,3))
    out <- prep_multi_line(spl$`1`, con)
    expect_equal(length(out), 8)
    expect_equal(length(out[[1]]), 1)
    expect_equal(dim(out[[1]][[1]]), c(111,3))
  })

  test_that("labelling function works ok", {
    df <- data.frame(name_long = c("A -- B", "A -- B -- C"), chart_no = c(1, 1))
    expect_warning(out <- get_legend_labels_from_df(df))
    expect_true(length(out) == 2)
    df <- data.frame(name_long = c("A -- B -- C", "A -- b -- c"), chart_no = c(1, 1))
    expect_warning(out <- get_legend_labels_from_df(df))
    expect_true(length(out) == 2)
    df <- data.frame(name_long = c("A -- B -- C", "A -- B -- c"), chart_no = c(1, 1))
    out <- get_legend_labels_from_df(df)
    expect_equal(out,c("C", "c"))
    df <- data.frame(name_long = c("A", "B"), chart_no = c(1, 1))
    out <- get_legend_labels_from_df(df)
    expect_equal(out, c("A", "B"))
  })
})




