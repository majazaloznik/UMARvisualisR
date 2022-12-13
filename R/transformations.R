#' Add rolling average column
#'
#' Add a column with a roling average for a given number of periods. By default
#' centered rolling averages are used, but right and left are also possible, see
#' the documentation for the zoo package for more deets.
#'
#' @param df dataframe with the `value` column
#' @param periods number of periods to average
#' @param align defaults to "center", "right" and "left" also options
#'
#' @return a df with one more column named `rolling`
#' @export
rolling_average <- function(df, periods = 3, align = "center"){
  df %>%
    dplyr::mutate(rolling = zoo::rollmean(value, k = periods,fill= NA,align = align))
}
