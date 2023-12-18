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

    config$legend_columns <- 2
    p <- function() {
      base_barplot(datapoints, config,y_axis)
      create_legend(config, 10)}
    vdiffr::expect_doppelganger("bar_plot w legend 1", p)

    config$legend_columns <- 1
    p <- function() {
      base_barplot(datapoints, config,y_axis)
      create_legend(config, 10, "en")}
    vdiffr::expect_doppelganger("bar_plot w legend en", p)
  })


})
