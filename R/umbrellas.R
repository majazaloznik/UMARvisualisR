#' Umbrella function for plotting a single line plot
#'
#' @param date_valid date when the vintage was valid if none is given most recent
#' i.e. currently valid vintage is returned.
#' @param xmin character date, default "2011-01-01"
#' @param xmax character date, default today
#' @param rolling logical / should a rolling average be calculated and displayed
#' @param roll_periods number of periods to roll over (defualt = 3)
#' @param roll_align alignment of rolling average (default = "center")
#' @param yoy logical - should the year on year change be calculated and displayed
#' @inheritParams common_parameters
#'
#' @return plots a univariate line chart
#' @export

univariate_line_pipeline <- function(series,
                             date_valid = NULL,
                             xmin = "2011-01-01", xmax =NULL,
                             rolling = FALSE, roll_periods = 3, roll_align = "center",
                             yoy = FALSE,
                             con) {
  vintage_id <- UMARaccessR::get_vintage_from_series(series, con, date_valid = date_valid)
  prep_l <- UMARaccessR::prep_single_line(vintage_id, con)
  if(rolling & yoy){
    prep_l <- add_yoy_of_rolling(prep_l, periods = roll_periods, align = roll_align)} else {
      if(rolling) prep_l <- add_rolling_average(prep_l, periods = roll_periods, align = roll_align)
      if(yoy) prep_l <- add_yoy_change(prep_l)}
  univariate_line_chart(prep_l, xmin, xmax)
}

