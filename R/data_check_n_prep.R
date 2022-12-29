
#' Prepare data needed for a univariate line chart
#'
#' Given the vintage id and a connection to the database, this function
#' gets the datapoints, and the unit, prepares the titles. The returned list
#' is the input for the plotting function \link[UMARvisualisR]{univariate_line_chart}.
#'
#' @param con PostgreSQL connection object created by the RPostgres package.
#' @param vintage numeric id of vintage
#' @param interval character value of interval type, defaults to NULL in which case the
#' function will get it from the database.
#' @param unit character describing unit to be written as y label. defaults to NULL in which case the
#' function will get it from the database.
#' @param main_title character..defaults to NULL in which case the
#' function will get it from the database.
#' @param sub_title character...defaults to NULL in which case the
#' function will get it from the database.
#'
#' @return a list with the data frame with values, period_ids and periods as the
#' first element, a character unit name as second, and the row wrapped main and
#' subtitles (default 100 chars max 3 lines), the date and time of the last update,
#' the last period and the interval.
#' @export
#'
prep_single_line <- function(vintage, con, interval=NULL,
                             unit = NULL, main_title = NULL, sub_title = NULL){
  if(is.null(interval)) {interval <- UMARaccessR::get_interval_from_vintage(vintage, con)}
  single <- UMARaccessR::add_date_from_period_id(
    UMARaccessR::get_data_points_from_vintage(vintage, con), interval)
  if(is.null(unit)) { unit <-first_up(UMARaccessR::get_unit_from_vintage(vintage, con))}
  if(is.null(main_title)) { main_title <- wrap_string(
    UMARaccessR::get_table_name_from_vintage(vintage, con))} else {
      main_title <- wrap_string(main_title)}
  if(is.null(sub_title)) {sub_title <- wrap_string(
    UMARaccessR::get_series_name_from_vintage(vintage, con))} else {
      sub_title <- wrap_string(sub_title)}
  updated <- UMARaccessR::get_date_published_from_vintage(vintage, con)
  last_period <- UMARaccessR::get_last_period_from_vintage(vintage, con)
  mget(c("single", "unit", "main_title" , "sub_title", "updated", "last_period", "interval"))
}
