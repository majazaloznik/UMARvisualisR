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

    config$stacked <- FALSE
    p <- function() base_barplot(datapoints, config,y_axis)
    vdiffr::expect_doppelganger("bar_plot grouped", p)

    config$stacked <- TRUE
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

    # stacked with 100 and 0 emph and negative
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
    expect_equal(out$tickmarks, as.Date(c("2020-01-01", "2021-01-01")))
    datapoints <- list(data.frame(date = c("2020-01-01", "2020-04-01", "2020-07-01", "2020-10-01", "2021-01-01"),
                                  value = c(52,63,74,-5, 2)),
                       data.frame(date = c("2020-01-01", "2020-04-01", "2020-07-01", "2020-10-01", "2021-01-01"),
                                  value = c(22,33,44,-2, 2)))
    out <- x_axis_lims_tickmarks(datapoints, config)
    expect_equal(out$tickmarks, as.Date(c("2020-01-01", "2021-01-01")))
    # different lengths
    datapoints <- list(data.frame(date = c("2020-01-01", "2020-04-01", "2020-07-01", "2020-10-01", "2021-01-01"),
                                  value = c(52,63,74,-5, 2)),
                       data.frame(date = c("2020-01-01", "2020-04-01", "2020-07-01", "2020-10-01"),
                                  value = c(22,33,44,-2)))
    out <- x_axis_lims_tickmarks(datapoints, config)
    expect_equal(out$tickmarks, as.Date(c("2020-01-01", "2021-01-01")))

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

  test_that("x axis params squishiness", {
  temp_file <- start_controlled_plot(1000, 500)
  datapoints <- list(data.frame(date = seq(as.Date("2010-01-01"), as.Date("2024-10-01"), by = "year"),
                                                  value = 1:15))
  config <- list(series = list(list(type="line", colour = umar_cols()[1], legend_txt_si = "serija 1",
                                    legend_txt_en = "series 1", unit = "EUR",  mio_eur = FALSE)),
                 y_axis_label = "leva os",
                 x_sub_annual = FALSE, stacked = FALSE)
  values <- get_data_values(datapoints, config)
  y_axis <- find_pretty_ylim(values)
  x_axis <- x_axis_lims_tickmarks(datapoints, config)
  left <- left_axis_label_width(config, y_axis)
  empty_plot(x_axis$x_lims, y_axis, left$unit)
  result <- x_axis_label_params(datapoints, config, x_axis$tickmarks, x_axis$x_lims, bar = FALSE, x_values = NULL, language = "si")
  expect_equal(result$x_labels, as.character(2010:2024))
  end_controlled_plot(temp_file)
  temp_file <- start_controlled_plot(width = 500)
  empty_plot(x_axis$x_lims, y_axis, left$unit)
  result <- x_axis_label_params(datapoints, config, x_axis$tickmarks, x_axis$x_lims, bar = FALSE, x_values = NULL, language = "si")
  expect_equal(result$x_labels, as.character(c(2010, (11:24))))
  end_controlled_plot(temp_file)
  temp_file <- start_controlled_plot(width = 200)
  empty_plot(x_axis$x_lims, y_axis, left$unit)
  result <- x_axis_label_params(datapoints, config, x_axis$tickmarks, x_axis$x_lims, bar = FALSE, x_values = NULL, language = "si")
  expect_equal(result$x_labels, as.character(c(2010, 15, 20, 24)))
  end_controlled_plot(temp_file)

  temp_file <- start_controlled_plot(1000, 500)
  datapoints <- list(data.frame(date = seq(as.Date("2010-01-01"), as.Date("2024-01-01"), by = "quarter"),
                                value = 1:57))
  config <- list(series = list(list(type="line", colour = umar_cols()[1], legend_txt_si = "serija 1",
                                    legend_txt_en = "series 1", unit = "EUR",  mio_eur = FALSE)),
                 y_axis_label = "leva os",
                 x_sub_annual = FALSE, stacked = FALSE)
  values <- get_data_values(datapoints, config)
  y_axis <- find_pretty_ylim(values)
  x_axis <- x_axis_lims_tickmarks(datapoints, config)
  left <- left_axis_label_width(config, y_axis)
  empty_plot(x_axis$x_lims, y_axis, left$unit)
  result <- x_axis_label_params(datapoints, config, x_axis$tickmarks, x_axis$x_lims, bar = FALSE, x_values = NULL, language = "si")
  axis.Date(1,
            at = x_axis$tickmarks,
            col = umar_cols("gridlines"),
            lwd = 0, lwd.ticks =1, tck=-0.02, labels = FALSE)
  axis(1, result$x_labels,
       at = result$x_positions,
       col = umar_cols("gridlines"),
       lwd = 0, tck = 0,  family ="Myriad Pro",
       padj = 0.5, gap.axis = 0.25)
  expect_equal(result$x_labels, c(as.character(2010:2022), "Q1-24"))
  end_controlled_plot(temp_file)

  temp_file <- start_controlled_plot(width = 500)
  empty_plot(x_axis$x_lims, y_axis, left$unit)
  result <- x_axis_label_params(datapoints, config, x_axis$tickmarks, x_axis$x_lims, bar = FALSE, x_values = NULL, language = "si")
  expect_equal(result$x_labels, c(as.character(c(2010, (11:22))), "Q1-24"))
  end_controlled_plot(temp_file)
  temp_file <- start_controlled_plot(width = 200)
  empty_plot(x_axis$x_lims, y_axis, left$unit)
  result <- x_axis_label_params(datapoints, config, x_axis$tickmarks, x_axis$x_lims, bar = FALSE, x_values = NULL, language = "si")
  expect_equal(result$x_labels, c("2010", "15", "20", "Q1-24"))
  end_controlled_plot(temp_file)


  })

})
