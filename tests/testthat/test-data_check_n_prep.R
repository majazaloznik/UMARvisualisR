dittodb::with_mock_db({
  con <- DBI::dbConnect(RPostgres::Postgres(),
                        dbname = "sandbox",
                        host = "localhost",
                        port = 5432,
                        user = "mzaloznik",
                        password = Sys.getenv("PG_local_MAJA_PSW"))
  DBI::dbExecute(con, "set search_path to test_platform")





  test_that("pub ready config prep works", {
    x <- openxlsx::read.xlsx(test_path("testdata", "pub_test_df.xlsx"), sheet = "Sheet1")
    expect_equal(prep_config(x)$xmin , as.Date("2010-01-01"))
    x <- openxlsx::read.xlsx(test_path("testdata", "pub_test_df.xlsx"), sheet = "Sheet27")
    expect_equal(prep_config(x)$xmin , as.Date("2000-12-31"))
    x <- openxlsx::read.xlsx(test_path("testdata", "pub_test_df.xlsx"), sheet = "Sheet3")
    expect_equal(prep_config(x)$title , "Naslov")
    x <- openxlsx::read.xlsx(test_path("testdata", "pub_test_df.xlsx"), sheet = "Sheet10")
    expect_equal(prep_config(x)$dual_y , TRUE)
    x <- openxlsx::read.xlsx(test_path("testdata", "pub_test_df.xlsx"), sheet = "Sheet13")
    expect_equal(prep_config(x)$series |>  purrr::map_chr(~ .[["colour"]]),
                 c("#A7AEB4", "#A10305", "#343D58", "#D46565"))
    x <- openxlsx::read.xlsx(test_path("testdata", "pub_test_df.xlsx"), sheet = "Sheet16")
    expect_equal(prep_config(x)$series |>  purrr::map_chr(~ .[["rolling_alignment"]]),
                 c("c", "r", "r"))
  })
})


dittodb::with_mock_db({
  con <- DBI::dbConnect(RPostgres::Postgres(),
                        dbname = "platform",
                        host = "localhost",
                        port = 5432,
                        user = "mzaloznik",
                        password = Sys.getenv("PG_local_MAJA_PSW"))
  DBI::dbExecute(con, "set search_path to test_platform")

  test_that("pub ready input checks work", {
    df <- read_csv_guess_encoding(test_path("testdata", "test_report_input.csv"))
    spl <- split(df, df$chart_no)
    expect_error(check_plot_inputs(spl$`3`, con, "test_platform"))
    x <- openxlsx::read.xlsx(test_path("testdata", "pub_test_df.xlsx"), sheet = "Sheet1")
    expect_true(check_plot_inputs(x, con, "test_platform")$enota == "%")
    x <- openxlsx::read.xlsx(test_path("testdata", "pub_test_df.xlsx"), sheet = "Sheet2")
    expect_true(check_plot_inputs(x, con, "test_platform")$enota == "%")
    x <- openxlsx::read.xlsx(test_path("testdata", "pub_test_df.xlsx"), sheet = "Sheet3")
    expect_message(check_plot_inputs(x, con, "test_platform"))
    x <- openxlsx::read.xlsx(test_path("testdata", "pub_test_df.xlsx"), sheet = "Sheet4")
    expect_message(check_plot_inputs(x, con, "test_platform"))
    x <- openxlsx::read.xlsx(test_path("testdata", "pub_test_df.xlsx"), sheet = "Sheet5")
    expect_message(check_plot_inputs(x, con, "test_platform"))
    x <- openxlsx::read.xlsx(test_path("testdata", "pub_test_df.xlsx"), sheet = "Sheet7")
    expect_message(check_plot_inputs(x, con, "test_platform"))
    x <- openxlsx::read.xlsx(test_path("testdata", "pub_test_df.xlsx"), sheet = "Sheet8")
    expect_message(check_plot_inputs(x, con, "test_platform"))
    x <- openxlsx::read.xlsx(test_path("testdata", "pub_test_df.xlsx"), sheet = "Sheet6")
    expect_equal(check_plot_inputs(x, con, "test_platform")$enota, c(NA, "Indeks (2015 = 100)"))
    x <- openxlsx::read.xlsx(test_path("testdata", "pub_test_df.xlsx"), sheet = "Sheet9")
    expect_error(check_plot_inputs(x, con, "test_platform"))
    expect_error(check_plot_inputs(x[1:2,], con, "test_platform"))
    x <- openxlsx::read.xlsx(test_path("testdata", "pub_test_df.xlsx"), sheet = "Sheet11")
    expect_message(check_plot_inputs(x, con, "test_platform"))
    x <- openxlsx::read.xlsx(test_path("testdata", "pub_test_df.xlsx"), sheet = "Sheet12")
    expect_message(check_plot_inputs(x, con, "test_platform"))
    x <- openxlsx::read.xlsx(test_path("testdata", "pub_test_df.xlsx"), sheet = "Sheet14")
    expect_message(check_plot_inputs(x, con, "test_platform"))
    x <- openxlsx::read.xlsx(test_path("testdata", "pub_test_df.xlsx"), sheet = "Sheet15")
    expect_message(check_plot_inputs(x, con, "test_platform"))
    x <- openxlsx::read.xlsx(test_path("testdata", "pub_test_df.xlsx"), sheet = "Sheet16")
    expect_message(check_plot_inputs(x, con, "test_platform"))
    x <- openxlsx::read.xlsx(test_path("testdata", "pub_test_df.xlsx"), sheet = "Sheet17")
    expect_message(check_plot_inputs(x, con, "test_platform"))
    x <- openxlsx::read.xlsx(test_path("testdata", "pub_test_df.xlsx"), sheet = "Sheet18")
    expect_message(check_plot_inputs(x, con, "test_platform"))
    x <- openxlsx::read.xlsx(test_path("testdata", "pub_test_df.xlsx"), sheet = "Sheet19")
    expect_message(check_plot_inputs(x, con, "test_platform"))
    x <- openxlsx::read.xlsx(test_path("testdata", "pub_test_df.xlsx"), sheet = "Sheet20")
    expect_message(check_plot_inputs(x, con, "test_platform"))
    x <- openxlsx::read.xlsx(test_path("testdata", "pub_test_df.xlsx"), sheet = "Sheet21")
    expect_s3_class(check_plot_inputs(x, con, "test_platform"), "data.frame")
    x <- openxlsx::read.xlsx(test_path("testdata", "pub_test_df.xlsx"), sheet = "Sheet23")
    expect_message(check_plot_inputs(x, con, "test_platform"))
    x <- openxlsx::read.xlsx(test_path("testdata", "pub_test_df.xlsx"), sheet = "Sheet24")
    expect_message(check_plot_inputs(x, con, "test_platform"))
    x <- openxlsx::read.xlsx(test_path("testdata", "pub_test_df.xlsx"), sheet = "Sheet27")
    expect_message(check_plot_inputs(x, con, "test_platform"))
  })


  test_that("data access and transformations work", {
    x <- openxlsx::read.xlsx(test_path("testdata", "pub_test_df.xlsx"), sheet = "Sheet22")[1:2,]
    df <- check_plot_inputs(x, con, "test_platform")
    config <- prep_config(df)
    datapoints <- get_data(config, con, "test_platform")
    expect_equal(datapoints[[1]]$value[1], 214593085)
    expect_equal(datapoints[[1]]$date[1], as.Date("1992-07-01"))
    out <- transform_data(datapoints[[1]], config$series[[1]])
    expect_true(is.na(out$df[1, 2]))
    out <- transform_data(datapoints[[2]], config$series[[2]])
    expect_true(is.na(out$df[1, 2]))
  })




})


dittodb::with_mock_db({
  con <- make_test_connection()

  test_that("data prep works", {
    x <- openxlsx::read.xlsx(test_path("testdata", "pub_test_df.xlsx"), sheet = "Sheet22")[1:2,]
    results <- prep_data(x, con, "test_platform")
    expect_true(length(results) == 2)
    expect_true(length(results$datapoints) == 2)
    expect_true(length(results$config$series) == 2)
    expect_true(is.na(results$datapoints[[1]][1, 2]))
    expect_true(is.na(results$datapoints[[2]][1, 2]))
  })
  test_that("splitting works ", {
    x <- openxlsx::read.xlsx(test_path("testdata", "pub_test_df.xlsx"), sheet = "Sheet22")[1:2,]
    results <- prep_data(x, con, "test_platform")
    out <- split_by_unit(results$datapoints, results$config)
    expect_equal(length(out$datapoints), 2)
    expect_equal(length(out$datapoints_right), 0)
    x <- openxlsx::read.xlsx(test_path("testdata", "pub_test_df.xlsx"), sheet = "Sheet26")
    results <- prep_data(x, con, "test_platform")
    out <- split_by_unit(results$datapoints, results$config)
    expect_equal(length(out$datapoints), 2)
    expect_true(is.null(out$datapoints_right))
    expect_true(is.null(out$config_right))
  })
})
