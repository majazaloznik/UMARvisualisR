
dittodb::with_mock_db({
  con <- DBI::dbConnect(RPostgres::Postgres(),
                        dbname = "sandbox",
                        host = "localhost",
                        port = 5432,
                        user = "mzaloznik",
                        password = Sys.getenv("PG_local_MAJA_PSW"))
  DBI::dbExecute(con, "set search_path to test_platform")


  test_that("UNI umbrellas work", {
    p <- function() univariate_line_pipeline(1234,
                                             date_valid = NULL,
                                             xmin = "2011-01-01", xmax =NULL,
                                             rolling = FALSE, roll_periods = 3, roll_align = "center",
                                             yoy = FALSE,
                                             interval = NULL,
                                             unit = NULL,
                                             main_title = NULL,
                                             sub_title = NULL,
                                             con = con)
    vdiffr::expect_doppelganger("full uni pipeline", p)
  })


  test_that("multi umbrellas work", {
    df <- read.csv2(test_path("testdata", "test_report_input2.csv"))
    spl <- split(df, df$chart_no)
    p <- function() multiline_pipeline(spl[[1]],con = con)
    vdiffr::expect_doppelganger("no title", p)
    p <- function() multiline_pipeline(spl[[2]],con = con)
    vdiffr::expect_doppelganger("no chart", p)
    p <- function() multiline_pipeline(spl[[3]],con = con)
    vdiffr::expect_doppelganger("no chart2", p)
    p <- function() multiline_pipeline(spl[[4]],con = con)
    vdiffr::expect_doppelganger("no chart3", p)
    p <- function() multiline_pipeline(spl[[5]],con = con)
    vdiffr::expect_doppelganger("no chart4", p)
    # same series names
    p <- function() multiline_pipeline(spl[[6]],con = con)
    vdiffr::expect_doppelganger("same series", p)
  })
})
