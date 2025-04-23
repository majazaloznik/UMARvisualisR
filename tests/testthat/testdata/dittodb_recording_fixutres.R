source("tests/testthat/connection_helper.R")
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

# dittodb::start_db_capturing()
# con <- dbConnect(RPostgres::Postgres(),
#                  dbname = "platform",
#                  host = "localhost",
#                  port = 5432,
#                  user = "mzaloznik",
#                  password = Sys.getenv("PG_local_MAJA_PSW"))
# dbExecute(con, "set search_path to test_platform")
# on.exit(dbDisconnect)
# x <- openxlsx::read.xlsx(test_path("testdata", "pub_test_df.xlsx"), sheet = "Sheet25")
# results <- prep_data(x, con)
# dittodb::stop_db_capturing()

dittodb::start_db_capturing()
con <- dbConnect(RPostgres::Postgres(),
                 dbname = "platform",
                 host = "localhost",
                 port = 5433,
                 user = "mzaloznik",
                 password = Sys.getenv("PG_local_MAJA_PSW"))
dbExecute(con, "set search_path to test_platform")
on.exit(dbDisconnect)
x <- openxlsx::read.xlsx(test_path("testdata", "pub_test_df.xlsx"), sheet = "Sheet26")
results <- prep_data(x, con)
dittodb::stop_db_capturing()

dittodb::start_db_capturing()
con <- make_test_connection()
x <- openxlsx::read.xlsx(test_path("testdata", "pub_test_df.xlsx"), sheet = "Sheet22")[1:2,]
df <- check_plot_inputs(x, con)
config <- prep_config(df)
datapoints <- get_data(config, con, "test_platform")
dittodb::stop_db_capturing()


dittodb::start_db_capturing()
con <- make_test_connection()
df <- read_csv_guess_encoding(test_path("testdata", "test_report_input.csv"))
spl <- split(df, df$chart_no)
out <- prep_multi_line(spl$`7`, con, NULL,"test_platform")
dittodb::stop_db_capturing()

dittodb::start_db_capturing()
con <- make_test_connection()
x <- openxlsx::read.xlsx(test_path("testdata", "pub_test_df.xlsx"), sheet = "Sheet22")[1:2,]
results <- prep_data(x, con, "test_platform")
dittodb::stop_db_capturing()

dittodb::start_db_capturing()
con <- make_test_connection()
x <- openxlsx::read.xlsx(test_path("testdata", "pub_test_df.xlsx"), sheet = "Sheet26")
results <- prep_data(x, con, "test_platform")
dittodb::stop_db_capturing()

dittodb::start_db_capturing()
con <- make_test_connection()
df <- read_csv_guess_encoding(testthat::test_path("testdata", "test_report_input4.csv"))
x <- update_units(df, con, "test_platform")
dittodb::stop_db_capturing()

dittodb::start_db_capturing()
con <- make_test_connection()
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
dittodb::stop_db_capturing()

dittodb::start_db_capturing()
con <- make_test_connection()
x <- openxlsx::read.xlsx(test_path("testdata", "pub_test_df.xlsx"), sheet = "Sheet22")[1:2,]
results <- prep_data(x, con, "test_platform")
dittodb::stop_db_capturing()


