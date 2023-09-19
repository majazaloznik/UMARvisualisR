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
    expect_equal(length(out), 9)
    expect_equal(length(out[[1]]), 2)
    expect_equal(dim(out[[1]][[1]]), c(111,3))
    out <- prep_multi_line(spl$`1`, con)
    expect_equal(length(out), 9)
    expect_equal(length(out[[1]]), 1)
    expect_equal(dim(out[[1]][[1]]), c(111,3))
    out <- prep_multi_line(spl$`20`, con)
    expect_false(any(is.na(out$legend_labels)))
  })

  test_that("labelling function works ok", {
    df <- data.frame(series_name = c("A -- B", "A -- B -- C"), chart_no = c(1, 1),
                     series_code = c("c1", "c2"))
    expect_warning(out <- get_legend_labels_from_df(df))
    expect_true(length(out) == 2)
    df <- data.frame(series_name = c("A -- B", "A -- B"), chart_no = c(1, 1),
                     series_code = c("c1", "c2"))
    expect_warning(out <- get_legend_labels_from_df(df))
    expect_true(length(out) == 2)
    df <- data.frame(series_name = c("A -- B -- C", "A -- b -- c"), chart_no = c(1, 1),
                     series_code = c("c1", "c2"))
    expect_warning(out <- get_legend_labels_from_df(df))
    expect_true(length(out) == 2)
    df <- data.frame(series_name = c("A -- B -- C", "A -- B -- c"), chart_no = c(1, 1),
                     series_code = c("c1", "c2"))
    out <- get_legend_labels_from_df(df)
    expect_equal(out,c("C", "c"))
    df <- data.frame(series_name = c("A", "B"), chart_no = c(1, 1),
                     series_code = c("c1", "c2"))
    out <- get_legend_labels_from_df(df, c("C", "D"))
    expect_equal(out, c("A", "B"))
    df <- data.frame(series_name = c("A", "A"), chart_no = c(1, 1),
                     series_code = c("c1", "c2"))
    out <- get_legend_labels_from_df(df, c("C", "D"))
    expect_equal(length(out), 2)
    df <- data.frame(series_name = c("A", "A"), chart_no = c(1, 1),
                     series_code = c("c1", "c2"))
    out <- get_legend_labels_from_df(df, c("C -- m", "D -- m"))
    expect_equal(length(out), 2)
    df <- data.frame(series_name = c("A -- B", "A -- B"), chart_no = c(1, 1),
                     series_code = c("c1", "c2"))
    expect_warning(out <- get_legend_labels_from_df(df, c("Y", "X")), regexp = NA)
    expect_equal(length(out), 2)
    expect_equal(out[[2]][[1]], c("A -- B"))

  })



  test_that("pub ready config prep works", {
    x <- openxlsx::read.xlsx(test_path("testdata", "pub_test_df.xlsx"), sheet = "Sheet1")
    expect_equal(prep_config(x)$xmin , "2010-01-01")
    x <- openxlsx::read.xlsx(test_path("testdata", "pub_test_df.xlsx"), sheet = "Sheet3")
    expect_equal(prep_config(x)$title , "Naslov")
    x <- openxlsx::read.xlsx(test_path("testdata", "pub_test_df.xlsx"), sheet = "Sheet10")
    expect_equal(prep_config(x)$dual_y , TRUE)
    x <- openxlsx::read.xlsx(test_path("testdata", "pub_test_df.xlsx"), sheet = "Sheet13")
    expect_equal(prep_config(x)$series |>  purrr::map_chr(~ .[["colour"]]),
                 c("#A7AEB4", "#A10305", "#343D58", "#D46565"))
    x <- openxlsx::read.xlsx(test_path("testdata", "pub_test_df.xlsx"), sheet = "Sheet16")
    expect_equal(prep_config(x)$series |>  purrr::map_chr(~ .[["rolling_alignment"]]),
                 c("C", "R", "R"))
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
    expect_error(check_plot_inputs(spl$`3`, con))
    x <- openxlsx::read.xlsx(test_path("testdata", "pub_test_df.xlsx"), sheet = "Sheet1")
    expect_true(check_plot_inputs(x, con)$enota == "%")
    x <- openxlsx::read.xlsx(test_path("testdata", "pub_test_df.xlsx"), sheet = "Sheet2")
    expect_true(check_plot_inputs(x, con)$enota == "%")
    x <- openxlsx::read.xlsx(test_path("testdata", "pub_test_df.xlsx"), sheet = "Sheet3")
    expect_message(check_plot_inputs(x, con))
    x <- openxlsx::read.xlsx(test_path("testdata", "pub_test_df.xlsx"), sheet = "Sheet4")
    expect_message(check_plot_inputs(x, con))
    x <- openxlsx::read.xlsx(test_path("testdata", "pub_test_df.xlsx"), sheet = "Sheet5")
    expect_message(check_plot_inputs(x, con))
    x <- openxlsx::read.xlsx(test_path("testdata", "pub_test_df.xlsx"), sheet = "Sheet7")
    expect_message(check_plot_inputs(x, con))
    x <- openxlsx::read.xlsx(test_path("testdata", "pub_test_df.xlsx"), sheet = "Sheet8")
    expect_message(check_plot_inputs(x, con))
    x <- openxlsx::read.xlsx(test_path("testdata", "pub_test_df.xlsx"), sheet = "Sheet6")
    expect_equal(check_plot_inputs(x, con)$enota, c("%", "Indeks"))
    x <- openxlsx::read.xlsx(test_path("testdata", "pub_test_df.xlsx"), sheet = "Sheet9")
    expect_error(check_plot_inputs(x, con))
    expect_error(check_plot_inputs(x[1:2,], con))
    x <- openxlsx::read.xlsx(test_path("testdata", "pub_test_df.xlsx"), sheet = "Sheet11")
    expect_message(check_plot_inputs(x, con))
    x <- openxlsx::read.xlsx(test_path("testdata", "pub_test_df.xlsx"), sheet = "Sheet12")
    expect_message(check_plot_inputs(x, con))
    x <- openxlsx::read.xlsx(test_path("testdata", "pub_test_df.xlsx"), sheet = "Sheet14")
    expect_message(check_plot_inputs(x, con))
    x <- openxlsx::read.xlsx(test_path("testdata", "pub_test_df.xlsx"), sheet = "Sheet15")
    expect_message(check_plot_inputs(x, con))
    x <- openxlsx::read.xlsx(test_path("testdata", "pub_test_df.xlsx"), sheet = "Sheet16")
    expect_message(check_plot_inputs(x, con))
    x <- openxlsx::read.xlsx(test_path("testdata", "pub_test_df.xlsx"), sheet = "Sheet17")
    expect_message(check_plot_inputs(x, con))
    x <- openxlsx::read.xlsx(test_path("testdata", "pub_test_df.xlsx"), sheet = "Sheet18")
    expect_message(check_plot_inputs(x, con))
    x <- openxlsx::read.xlsx(test_path("testdata", "pub_test_df.xlsx"), sheet = "Sheet19")
    expect_message(check_plot_inputs(x, con))
    x <- openxlsx::read.xlsx(test_path("testdata", "pub_test_df.xlsx"), sheet = "Sheet20")
    expect_message(check_plot_inputs(x, con))
    x <- openxlsx::read.xlsx(test_path("testdata", "pub_test_df.xlsx"), sheet = "Sheet21")
    expect_s3_class(check_plot_inputs(x, con), "data.frame")
    x <- openxlsx::read.xlsx(test_path("testdata", "pub_test_df.xlsx"), sheet = "Sheet23")
    expect_message(check_plot_inputs(x, con))
    x <- openxlsx::read.xlsx(test_path("testdata", "pub_test_df.xlsx"), sheet = "Sheet24")
    expect_message(check_plot_inputs(x, con))
  })


  test_that("data access and transformations work", {
    x <- openxlsx::read.xlsx(test_path("testdata", "pub_test_df.xlsx"), sheet = "Sheet22")
    df <- check_plot_inputs(x, con)
    config <- prep_config(df)
    datapoints <- get_data(config, con)
    expect_equal(datapoints[[1]]$value[1],214593085)
    expect_equal(datapoints[[1]]$date[1],as.Date("1992-07-01"))
    expect_equal(datapoints[[3]]$value[5],10.062781)
    out <- transform_data(datapoints[[1]], config$series[[1]])
    expect_true(is.na(out$df[1, 2]))
    out <- transform_data(datapoints[[2]], config$series[[2]])
    expect_true(is.na(out$df[1, 2]))
    out <- transform_data(datapoints[[3]], config$series[[3]])
    expect_equal(out$df[5, 2][[1]], 479, tolerance = 1e-2)
  })

  test_that("data prep works", {
    x <- openxlsx::read.xlsx(test_path("testdata", "pub_test_df.xlsx"), sheet = "Sheet22")
    results <- prep_data(x, con)
    expect_true(length(results) == 2)
    expect_true(length(results$data_points) == 3)
    expect_true(length(results$config$series) == 3)
    expect_true(is.na(results$data_points[[1]][1, 2]))
    expect_true(is.na(results$data_points[[2]][1, 2]))
    expect_equal(results$data_points[[3]][5, 2][[1]], 479, tolerance = 1e-2)
  })
})

