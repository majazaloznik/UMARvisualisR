library(testthat)
library(dittodb)

make_test_connection <- function() {
  con <- DBI::dbConnect(RPostgres::Postgres(),
                        dbname = "platform",
                        host = "192.168.38.21",
                        port = 5432,
                        user = "postgres",
                        password = Sys.getenv("PG_PG_PSW"))

}
