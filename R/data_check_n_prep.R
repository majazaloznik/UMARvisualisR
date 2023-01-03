
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


#' Check input data for multi-line chart
#'
#' Helper function to check that the input data dataframe for multi line charts is
#' correct. This means that series used in a single chart have to be:
#' - multiple
#' - have the same unit
#' - have the same interval (this may be changed in the future)
#' - are either all rolling averages or not and if they are, they have the same
#' alignment and number of periods (might also change in the future)
#' - are either all y-o-y changes or not (might also change in the future).
#' If any of these is true, the function stops
#'
#' Also checks if
#' - main_titles are the same, otherwise it deletes them, but only issues a warning.
#'
#' @param df input dataframe with at least the following columns: code, unit_name
#' interval_id, rolling_average_alignment, rolling_average_periods, year_on_year
#'
#' @return input df, possibly with updated main titles.
#' @export
#'
multi_checks <- function(df){
  if(nrow(df)<=1) stop("The data is for a single line only")
  if(!all_equal(df$unit_name))  stop(
    paste("Graf števika", df$cart_no,
          "\n Vse izbrane serije morajo imeti enako enoto!"))
  if(!all_equal(df$interval_id))  stop(
    paste("Graf števika", df$cart_no,
          "\n Trenutno ve\u010dlinijski grafi niso mo\u017eni za serije z razli\u010dnimi intervali."))
  if(!all_equal(df$rolling_average_alignment)) stop(
    paste("Graf števika", df$cart_no,
          "\n Vse serije na ve\u010dlinisjkem grafu morajo uporabljati enako drse\u010do sredino."))
  if(!all_equal(df$rolling_average_periods)) stop(
    paste("Graf števika", df$cart_no,
          "\n Vse serije na ve\u010dlinisjkem grafu morajo uporabljati enako drse\u010do sredino."))
  if(!all_equal(df$year_on_year)) stop(
    paste("Graf števika", df$cart_no,
          "\n Medletno spremembo na ve\u010dlinijskem grafu je mogo\u010d uporabiti za vse serije ali za nobeno."))
  df <- multi_titles(df)
}


#' Check titles in input for multi-line chart
#'
#' Check if the main_title is the same for all series, and if not issue a warning and
#' remove all the titles
#'
#' @param df input dataframe with at least the following columns: code, unit_name
#' interval_id, rolling_average_alignment, rolling_average_periods, year_on_year
#'
#' @return input df, possibly with updated main titles.
#' @export
#'
multi_titles <- function(df){
  if(!all_equal(df$main_title))  {
    warning(paste("Graf števika", df$cart_no,
            "\n Vse izbrane serije morajo imeti enak naslov, zato je zdaj graf brez naslova."))
    df$main_title <- NA
  }
  df
}


prep_multi_line <- function(df, con, date_valid = NULL){
  interval <- unique(df$interval_id)
  unit <- first_up(unique(df$unit_name))
  main_title <- wrap_string(unique(df$main_title))
  if (nrow(df) > 1) sub_title <- NA else
    sub_title <- wrap_string(df$name_long)
  df <- df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(vintage_id = UMARaccessR::get_vintage_from_series(id, con, date_valid)$id,
                  updated = UMARaccessR::get_date_published_from_vintage(vintage_id, con)$published)
  data_points <- purrr::map(df$vintage_id, UMARaccessR::get_data_points_from_vintage, con)
  data_points <- purrr::map(data_points, UMARaccessR::add_date_from_period_id, interval)
  updated <- max(df$updated)
  max_period <- do.call("max", purrr::map(data_points, function(x) max(x$period)))
  last_period <- data_points[[1]]$period_id[data_points[[1]]$period == max_period]
  mget(c("data_points", "unit", "main_title" , "sub_title", "updated", "last_period", "interval"))
}
