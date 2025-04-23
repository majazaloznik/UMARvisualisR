test_that("transformations work", {
  df <- data.frame(value = c(1:10), period = c(10,1))
  expect_equal(dim(UMARvisualisR:::rolling_average(df)), c(10,3))
  expect_equal(dim(add_rolling_average(df)[[1]]), c(10,3))
  df <- data.frame(value = c(1:24), period = c(24:1))
  expect_true(any(is.na(yoy_change(df, 12)$value)))
  prep_l <- add_yoy_change(df, "M")
  expect_equal(dim(prep_l[[1]]), c(24,3))
  expect_equal(length(prep_l), 3)
  prep_l <- add_yoy_of_rolling(df, interval ="M")
  expect_equal(length(prep_l), 3)
  expect_equal(dim(prep_l[[1]]), c(24,3))
  df <- data.frame(value = c(1:24), period = c(24:1))
  input_data <- list(data_points = list(df), rolling_average_periods = 3, rolling_average_alignment = "c",
                year_on_year = FALSE, interval = "M")
  expect_equal(length(do_transformations(input_data)), 7)
  expect_equal(dim(do_transformations(input_data)$data_points[[1]]), c(24,3))

})



dittodb::with_mock_db({
  con <- DBI::dbConnect(RPostgres::Postgres(),
                        dbname = "platform",
                        host = "localhost",
                        port = 5432,
                        user = "mzaloznik",
                        password = Sys.getenv("PG_local_MAJA_PSW"))
  DBI::dbExecute(con, "set search_path to test_platform")

  test_that("transformations work as expected", {
    x <- openxlsx::read.xlsx(test_path("testdata", "pub_test_df.xlsx"), sheet = "Sheet22")[1:2,]
    df <- check_plot_inputs(x, con, "test_platform")
    config <- prep_config(df)
    datapoints <- get_data(config, con, "test_platform")
    expect_equal(transform_rolling(datapoints[[1]])[6, 2][[1]], 615899291, tolerance = 1e-8)
    expect_equal(transform_rolling(datapoints[[1]], 3, "r")[7, 2][[1]], 615899291, tolerance = 1e-8)
    expect_equal(transform_rolling(datapoints[[1]], 3, "l")[5, 2][[1]], 615899291, tolerance = 1e-8)
    df <- data.frame(date = as.Date(c('2023-01-01', '2023-01-02', '2023-01-03')),
                     value = c(1, 5, 3))
    result <- transform_rolling(df, periods = 3)
    expect_true(result$value[2] >= min(df$value) && result$value[2] <= max(df$value))
    expect_error(transform_rolling(df, periods = 3, "lkj"))
    df <- data.frame(date = as.Date(c('2023-01-01', '2023-01-02', '2023-01-03')),
                     value = c("M", 5, 3))
    expect_error(transform_rolling(df))
    expect_equal(transform_growth(datapoints[[1]])[2,2][[1]], 40.2, tolerance = 1e-2)
    expect_equal(transform_growth(datapoints[[2]])[13,2][[1]], 12.9, tolerance = 1e-2)
    expect_equal(transform_growth(datapoints[[2]], "MOM")[2,2][[1]], 10.1, tolerance = 1e-2)
    expect_equal(transform_index(datapoints[[1]], 1993)$df[2,2][[1]], 100)
    expect_warning(expect_equal(transform_index(datapoints[[1]], 1990)$df[1,2][[1]], 100))
    expect_warning(expect_equal(transform_index(datapoints[[1]], 1990)$df[1,2][[1]], 100))
    expect_warning(expect_equal(mean(transform_index(datapoints[[2]], "1996")$df[1:6,2][[1]]), 100))
    expect_warning(expect_equal(transform_index(datapoints[[2]], "1996M01")$df[1,2][[1]], 100))
    expect_equal(transform_index(datapoints[[2]], "1999M08")$df[2,2][[1]], 100)
    x <- openxlsx::read.xlsx(test_path("testdata", "pub_test_df.xlsx"), sheet = "Sheet25")
    results <- prep_data(x, con, "test_platform")
    expect_equal(results$datapoints[[1]] |> dplyr::filter(lubridate::year(date) == 2010) |>
      dplyr::pull(value) |>  mean(na.rm = TRUE), 100)
  })
})
