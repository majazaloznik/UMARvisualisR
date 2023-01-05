
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
#' correct. This means that series used in a single chart have to:
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
#' @param con PostgreSQL connection object created by the RPostgres package.
#'
#' @return input df, possibly with updated main titles.
#' @export
#'
multi_checks <- function(df, con){
  if(!all_equal(df$unit_name))  stop(
    paste("Graf \u0161tevika", unique(df$chart_no),
          "\n Vse izbrane serije morajo imeti enako enoto!"))
  if(!all_equal(df$interval_id))  stop(
    paste("Graf \u0161tevika", unique(df$chart_no),
          "\n Trenutno ve\u010dlinijski grafi niso mo\u017eni za serije z razli\u010dnimi intervali."))
  if(!all_equal(df$rolling_average_alignment)) stop(
    paste("Graf \u0161tevika", unique(df$chart_no),
          "\n Vse serije na ve\u010dlinisjkem grafu morajo uporabljati enako drse\u010do sredino."))
  if(!all_equal(df$rolling_average_periods)) stop(
    paste("Graf \u0161tevika", unique(df$chart_no),
          "\n Vse serije na ve\u010dlinisjkem grafu morajo uporabljati enako drse\u010do sredino."))
  if(!all_equal(df$year_on_year)) stop(
    paste("Graf \u0161tevika", unique(df$chart_no),
          "\n Medletno spremembo na ve\u010dlinijskem grafu je mogo\u010d uporabiti za vse serije ali za nobeno."))
  df <- multi_titles(df, con)
}


#' Check titles in input for multi-line chart
#'
#' Check if the main_title is the same for all series, and if not issue a warning and
#' remove all the titles
#'
#' @param df input dataframe with at least the following columns: code, unit_name
#' interval_id, rolling_average_alignment, rolling_average_periods, year_on_year
#' @param con PostgreSQL connection object created by the RPostgres package.

#' @return input df, possibly with updated main titles.
#' @export
#'
multi_titles <- function(df, con){
  if(!all_equal(df$main_title))  {
    warning(paste("Graf \u0161tevika", unique(df$chart_no),
                  "\n Vse izbrane serije morajo imeti enak naslov, zato je zdaj graf brez naslova."))
    df$main_title <-  paste("Graf \u0161tevika", unique(df$chart_no))
  }
  if(all_equal(df$px_code) & is.na(unique(df$main_title))) df$main_title <- UMARaccessR::get_table_name_from_series(df$id[1], con)
  if(is.na(unique(df$main_title))) df$main_title <-  paste("Graf \u0161tevika", unique(df$chart_no))
  df
}

#' Get legend labels from input dataframe
#'
#' One of the columns is the `name_long` one, which is from the `series` table and contains
#' the dimension values separated by `--`. If these names have the same number of dimension
#' values and differ by just one of them, then that one is returned as the legend labels.
#' Warnings are issued for different numbers of dimensions or differences in more than
#' one dim, in which case NAs are returned.
#' If the labels have been entered manually, they are used instead - but must of course
#' be unique and not contain the `  -- ` character sequence.
#' At the moment no limits are placed on the length of the labels, that's coming next.
#'
#' @param df input dataframe with at least the following columns: `name_long`, `chart_no`
#'
#' @return character vector of length nrow(df) with legend labels
#' @export
#'
get_legend_labels_from_df <- function(df) {
  splt <- sapply(list(df$name_long)[[1]], function(x) strsplit(x, " -- "))
  if(!UMARvisualisR::all_equal(sapply(splt, length))) {
    warning(paste("Graf", unique(df$chart_no),
                  ": Oznak legende ni mogo\u010de dolo\u010diti avtomati\u010dno, ker so serije iz razli\u010dnih tabel."))
    diff <- rep(NA, nrow(df))} else {
      intersection <- Reduce(intersect, splt)
      if(!unique(sapply(splt, length)) == length(intersection)+1) {
        warning(paste("Graf", unique(df$chart_no),
                      ": Oznak legende ni mogo\u010e dolo\u010diti avtomati\u010dno, ker se razlikujejo po ve\u010d kot eni dimenziji"))
        diff <- rep(NA, nrow(df))} else {
          diff <- sapply(splt, function(x) setdiff(x, intersection))
          unname(diff)}
    }
}


#' Prepare data needed for multi (or single) line chart
#'
#' Uses an input table which must have the following columns: `itnerval_id`,
#' `unit_name`, `main_title`, `name_long` for the subtitle, `id` for the series id
#' (same as gets input into \link[UMARvisualisR]{multi_checks}, which is
#' run on the dataframe as the first step in this funciton, to check everything is cool with
#' the inputs.
#'
#' Prepares the list of data, structured to get plotted properly. Currently seven
#' elements are prepared: `data_points` a list of dataframes with the actual data,
#' `unit`, `main_title` , `sub_title`, `updated`, `last_period`, `interval`.
#'
#' Needs a valid connection to get the data as well as the last published date.
#'
#' @param df dataframe described above
#' @param con PostgreSQL connection object created by the RPostgres package.
#' @param date_valid validity of vintages, NULL gives most recent.
#'
#' @return A list of lentgh seven described above.
#' @export
#'
prep_multi_line <- function(df, con, date_valid = NULL){
  df <- multi_checks(df, con)
  interval <- unique(df$interval_id)
  unit <- first_up(unique(df$unit_name))
  main_title <- wrap_string(unique(df$main_title))
  if (nrow(df) > 1) {
    sub_title <- list(NA, 0)
    legend_labels <- get_legend_labels_from_df(df)}else {
    sub_title <- wrap_string(df$name_long)
    legend_labels <- NA}
  df <- df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(vintage_id = UMARaccessR::get_vintage_from_series(id, con, date_valid)$id,
                  updated = UMARaccessR::get_date_published_from_vintage(vintage_id, con)$published)
  data_points <- purrr::map(df$vintage_id, UMARaccessR::get_data_points_from_vintage, con)
  data_points <- purrr::map(data_points, UMARaccessR::add_date_from_period_id, interval)
  updated <- max(df$updated)
  max_period <- do.call("max", purrr::map(data_points, function(x) max(x$period)))
  last_period <- data_points[[1]]$period_id[data_points[[1]]$period == max_period]
  mget(c("data_points", "unit", "main_title" , "sub_title", "updated", "last_period",
         "interval", "legend_labels"))
}
