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
    datapoints <- cut_to_x_range(results$datapoints, results$config)
    xlims <- get_x_lims(datapoints)
    expect_equal(xlims, structure(c(min_date = 14625, max_date = 19539), class = "Date"))
    results$config$xmax <- "2022-01-01"
    datapoints <- cut_to_x_range(results$datapoints, results$config)
    xlims <- get_x_lims(datapoints)
    expect_equal(xlims, structure(c(min_date = 14625, max_date = 18977), class = "Date"))
    x <- openxlsx::read.xlsx(test_path("testdata", "pub_test_df.xlsx"), sheet = "Sheet26")
    results <- prep_data(x, con)
    datapoints <- cut_to_x_range(results$datapoints, results$config)
    xlims <- get_x_lims(datapoints)
    expect_equal(xlims, structure(c(min_date = 13194, max_date = 19403), class = "Date"))
  })

  test_that("top margins are calculated correctly", {
    config <- list(series = c(1,2,3),
                   legend_columns = 2,
                   title = "Very very long title that just keeps going on and on and hopefully takes up at least two lines for me to be able to test this shit properly.")
    top <- get_top_margin_and_title(config, 10)
    expect_true(top[[1]]> 3.3 & top[[1]] < 3.4)
    expect_equal(top[[2]], 1.69)

  })

})
