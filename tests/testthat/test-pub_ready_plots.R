dittodb::with_mock_db({
  con <- DBI::dbConnect(RPostgres::Postgres(),
                        dbname = "platform",
                        host = "localhost",
                        port = 5432,
                        user = "mzaloznik",
                        password = Sys.getenv("PG_local_MAJA_PSW"))
  DBI::dbExecute(con, "set search_path to test_platform")

  test_that("x-axis limits are calculated correctly", {
    x <- openxlsx::read.xlsx(test_path("testdata", "pub_test_df.xlsx"), sheet = "Sheet22")
    results <- prep_data(x, con)
    xlims <- get_x_lims(results$datapoints, results$config)
    expect_equal(xlims, structure(c(14610, 19539), class = "Date"))
    results$config$xmax <- "2022-01-01"
    xlims <- get_x_lims(results$datapoints, results$config)
    expect_equal(xlims, structure(c(14610, 18993), class = "Date"))
    x <- openxlsx::read.xlsx(test_path("testdata", "pub_test_df.xlsx"), sheet = "Sheet26")
    results <- prep_data(x, con)
    xlims <- get_x_lims(results$datapoints, results$config)
    expect_equal(xlims, as.Date(c("2006-02-15", "2002-03-01")))

  })
})
