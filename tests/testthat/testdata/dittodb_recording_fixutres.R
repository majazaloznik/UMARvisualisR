library(DBI)
library(RPostgres)
library(dittodb)


start_db_capturing()
con <- dbConnect(RPostgres::Postgres(),
                 dbname = "sandbox",
                 host = "localhost",
                 port = 5432,
                 user = "mzaloznik",
                 password = Sys.getenv("PG_local_MAJA_PSW"))
dbExecute(con, "set search_path to test_platform")
on.exit(dbDisconnect)
out <- prep_single_line(1625, con)
stop_db_capturing()

start_db_capturing()
con <- dbConnect(RPostgres::Postgres(),
                 dbname = "sandbox",
                 host = "localhost",
                 port = 5432,
                 user = "mzaloznik",
                 password = Sys.getenv("PG_local_MAJA_PSW"))
dbExecute(con, "set search_path to test_platform")
on.exit(dbDisconnect)
out <- univariate_line_pipeline(1625, con=con)
stop_db_capturing()



start_db_capturing()
con <- dbConnect(RPostgres::Postgres(),
                 dbname = "sandbox",
                 host = "localhost",
                 port = 5432,
                 user = "mzaloznik",
                 password = Sys.getenv("PG_local_MAJA_PSW"))
dbExecute(con, "set search_path to test_platform")
on.exit(dbDisconnect)
univariate_line_pipeline(1234,
                         date_valid = NULL,
                         xmin = "2011-01-01", xmax =NULL,
                         rolling = FALSE, roll_periods = 3, roll_align = "center",
                         yoy = FALSE,
                         interval = NULL,
                         unit = NULL,
                         main_title = NULL,
                         sub_title = NULL,
                         con = con)
stop_db_capturing()


start_db_capturing()
con <- dbConnect(RPostgres::Postgres(),
                 dbname = "sandbox",
                 host = "localhost",
                 port = 5432,
                 user = "mzaloznik",
                 password = Sys.getenv("PG_local_MAJA_PSW"))
dbExecute(con, "set search_path to test_platform")
on.exit(dbDisconnect)
df <- read_csv_guess_encoding(test_path("testdata", "test_report_input.csv"))
spl <- split(df, df$chart_no)
out <- prep_multi_line(spl$`4`, con)
out <- prep_multi_line(spl$`1`, con)
stop_db_capturing()

