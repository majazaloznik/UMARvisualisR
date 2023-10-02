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
    expect_equal(xlims, structure(c(709948800, 1688169600), class = c("POSIXct", "POSIXt"
    ), tzone = "UTC"))
    results$config$xmax <- "2022-01-01"
    xlims <- get_x_lims(results$datapoints, results$config)
    expect_equal(xlims, structure(c(1262304000, 1640995200), class = c("POSIXct", "POSIXt"
    ), tzone = "UTC"))
    x <- openxlsx::read.xlsx(test_path("testdata", "pub_test_df.xlsx"), sheet = "Sheet26")
    results <- prep_data(x, con)
    xlims <- get_x_lims(results$datapoints, results$config)
    expect_equal(xlims, structure(c(1139961600, 1676419200), class = c("POSIXct", "POSIXt"
    ), tzone = "UTC"))
  })

  test_that("top margins are calculated correctly", {
    config <- list(series = c(1,2,3),
                   legend_columns = 2,
                   title = "Very very long title that just keeps going on and on and hopefully takes up at least two lines for me to be able to test this shit properly.")
    top <- get_top_margin_and_title(config,)
    expect_true(top[[1]]> 4.4 & top[[1]] < 4.5)
    expect_equal(top[[2]], 2.2)

  })

})
