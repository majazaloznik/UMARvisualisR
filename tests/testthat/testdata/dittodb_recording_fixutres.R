library(DBI)
library(RPostgres)
library(dittodb)

# start_db_capturing()
# con <- dbConnect(RPostgres::Postgres(),
#                  dbname = "sandbox",
#                  host = "localhost",
#                  port = 5432,
#                  user = "mzaloznik",
#                  password = Sys.getenv("PG_local_MAJA_PSW"))
# dbExecute(con, "set search_path to test_platform")
# on.exit(dbDisconnect)
# out <- prep_single_line(1625, con)
# stop_db_capturing()
#
# start_db_capturing()
# con <- dbConnect(RPostgres::Postgres(),
#                  dbname = "sandbox",
#                  host = "localhost",
#                  port = 5432,
#                  user = "mzaloznik",
#                  password = Sys.getenv("PG_local_MAJA_PSW"))
# dbExecute(con, "set search_path to test_platform")
# on.exit(dbDisconnect)
# out <- univariate_line_pipeline(1625, con=con)
# stop_db_capturing()
#
#
#
# start_db_capturing()
# con <- dbConnect(RPostgres::Postgres(),
#                  dbname = "sandbox",
#                  host = "localhost",
#                  port = 5432,
#                  user = "mzaloznik",
#                  password = Sys.getenv("PG_local_MAJA_PSW"))
# dbExecute(con, "set search_path to test_platform")
# on.exit(dbDisconnect)
# univariate_line_pipeline(1234,
#                          date_valid = NULL,
#                          xmin = "2011-01-01", xmax =NULL,
#                          rolling = FALSE, roll_periods = 3, roll_align = "center",
#                          yoy = FALSE,
#                          interval = NULL,
#                          unit = NULL,
#                          main_title = NULL,
#                          sub_title = NULL,
#                          con = con)
# stop_db_capturing()
#
#
# start_db_capturing()
# con <- dbConnect(RPostgres::Postgres(),
#                  dbname = "sandbox",
#                  host = "localhost",
#                  port = 5432,
#                  user = "mzaloznik",
#                  password = Sys.getenv("PG_local_MAJA_PSW"))
# dbExecute(con, "set search_path to test_platform")
# on.exit(dbDisconnect)
# df <- read_csv_guess_encoding(test_path("testdata", "test_report_input.csv"))
# spl <- split(df, df$chart_no)
# out <- prep_multi_line(spl$`4`, con)
# out <- prep_multi_line(spl$`1`, con)
# stop_db_capturing()
#
#
#
# dittodb::start_db_capturing()
# con <- dbConnect(RPostgres::Postgres(),
#                  dbname = "sandbox",
#                  host = "localhost",
#                  port = 5432,
#                  user = "mzaloznik",
#                  password = Sys.getenv("PG_local_MAJA_PSW"))
# dbExecute(con, "set search_path to test_platform")
# on.exit(dbDisconnect)
# df <- read_csv_guess_encoding(test_path("testdata", "test_report_input.csv"))
# spl <- split(df, df$chart_no)
# prep_multi_line(spl$`20`, con)
# dittodb::stop_db_capturing()
#
#
# dittodb::start_db_capturing()
# con <- dbConnect(RPostgres::Postgres(),
#                  dbname = "platform",
#                  host = "localhost",
#                  port = 5432,
#                  user = "mzaloznik",
#                  password = Sys.getenv("PG_local_MAJA_PSW"))
# dbExecute(con, "set search_path to test_platform")
# on.exit(dbDisconnect)
# x <- openxlsx::read.xlsx(test_path("testdata", "pub_test_df.xlsx"), sheet = "Sheet6")
# prep_config(x, con)
# dittodb::stop_db_capturing()
#
# dittodb::start_db_capturing()
# con <- dbConnect(RPostgres::Postgres(),
#                  dbname = "platform",
#                  host = "localhost",
#                  port = 5432,
#                  user = "mzaloznik",
#                  password = Sys.getenv("PG_local_MAJA_PSW"))
# dbExecute(con, "set search_path to test_platform")
# on.exit(dbDisconnect)
# df <- read_csv_guess_encoding(testthat::test_path("testdata", "test_report_input4.csv"))
# x <- update_units(df, con)
# dittodb::stop_db_capturing()
#
#
# dittodb::start_db_capturing()
# con <- dbConnect(RPostgres::Postgres(),
#                  dbname = "platform",
#                  host = "localhost",
#                  port = 5432,
#                  user = "mzaloznik",
#                  password = Sys.getenv("PG_local_MAJA_PSW"))
# dbExecute(con, "set search_path to test_platform")
# on.exit(dbDisconnect)
# x <- openxlsx::read.xlsx(test_path("testdata", "pub_test_df.xlsx"), sheet = "Sheet9")
# prep_config(x, con)
# prep_config(x[1:2,], con)
# x <- openxlsx::read.xlsx(test_path("testdata", "pub_test_df.xlsx"), sheet = "Sheet10")
# prep_config(x, con)
# dittodb::stop_db_capturing()

# dittodb::start_db_capturing()
# con <- dbConnect(RPostgres::Postgres(),
#                  dbname = "platform",
#                  host = "localhost",
#                  port = 5432,
#                  user = "mzaloznik",
#                  password = Sys.getenv("PG_local_MAJA_PSW"))
# dbExecute(con, "set search_path to test_platform")
# on.exit(dbDisconnect)
# x <- openxlsx::read.xlsx(test_path("testdata", "pub_test_df.xlsx"), sheet = "Sheet22")
# df <- check_plot_inputs(x, con)
# config <- prep_config(df)
# datapoitns <- get_data(config, con)
# dittodb::stop_db_capturing()

dittodb::start_db_capturing()
con <- dbConnect(RPostgres::Postgres(),
                 dbname = "platform",
                 host = "localhost",
                 port = 5432,
                 user = "mzaloznik",
                 password = Sys.getenv("PG_local_MAJA_PSW"))
dbExecute(con, "set search_path to test_platform")
on.exit(dbDisconnect)
x <- openxlsx::read.xlsx(test_path("testdata", "pub_test_df.xlsx"), sheet = "Sheet25")
results <- prep_data(x, con)
dittodb::stop_db_capturing()

