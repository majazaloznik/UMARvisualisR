
dittodb::with_mock_db({
  con <- DBI::dbConnect(RPostgres::Postgres(),
                        dbname = "sandbox",
                        host = "localhost",
                        port = 5432,
                        user = "mzaloznik",
                        password = Sys.getenv("PG_local_MAJA_PSW"))
  DBI::dbExecute(con, "set search_path to test_platform")


  test_that("umbrellas work", {
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
})

