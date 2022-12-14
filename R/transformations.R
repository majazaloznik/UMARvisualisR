rolling_average <- function(df, periods = 3, align = "center"){
  df %>%
    dplyr::mutate(rolling = zoo::rollmean(value, k = periods,fill= NA,align = align))
}


#' Add rolling average column to prepared data
#'
#' Add a column with a rolling average for a given number of periods. By default
#' centerred rolling averages are used, but right and left are also possible, see
#' the documentation for the zoo package for more deets.
#'
#' @param prep_l prepared list with first element a dataframe with the `value` column
#' @param periods number of periods to average
#' @param align defaults to "center", "right" and "left" also options
#'
#' @return same list with the df with one more column named `rolling`
#' @export
add_rolling_average <- function(prep_l, periods = 3, align = "center") {
  prep_l[[1]] <- rolling_average(prep_l[[1]], periods = 3, align = "center")
  prep_l <- c(prep_l, transf_txt = paste("Transf.:", periods, prep_l$interval, "drse\\u010da sredina"))
  prep_l
}
