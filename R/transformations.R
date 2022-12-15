rolling_average <- function(df, periods = 3, align = "center"){
  df %>%
    dplyr::mutate(raw = value,
                  value = zoo::rollmean(value, k = periods,fill= NA,align = align))
}

yoy_change <- function(df, lag = 12){
 df %>%
    dplyr::arrange(period) %>%
    dplyr::mutate(value = value/dplyr::lag(value,n = lag)*100)
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
#' @return same list with the df with the `value` column overwritten and the old
#' values stored in the `raw` column, plus an element with the text for describing
#' the transformation.
#'
#' @export
add_rolling_average <- function(prep_l, periods = 3, align = "center") {
  prep_l[[1]] <- rolling_average(prep_l[[1]], periods = periods, align = align)
  align_txt <- ifelse(align == "center", " (centrirana)",
                      ifelse(align == "right", " (desna)",
                             ifelse(align == "left", " (leva)", "")))
  prep_l <- c(prep_l, transf_txt = paste0("Transf.: ", periods,"-",prep_l[["interval"]],
                                          " drse\u010da sredina", align_txt))
  prep_l
}


#' Add year on year change column to prepared data
#'
#' Year-on-year change is calculated based on the type of interval and overwrites the
#' values of the original data. The unit is also changed to "%", adn the
#' transformation text is added.
#'
#' @param prep_l prepared list with first element a dataframe with the `value` column
#'
#' @return same list with the df with one the value column overwritten and new element
#' with  text for describing the transformation and the unit overwritten as well.
#' @export
#'
add_yoy_change <- function(prep_l) {
  lag <- ifelse(prep_l[["interval"]] == "M", 12,
                ifelse(prep_l[["interval"]] == "Q", 4,
                       ifelse(prep_l[["interval"]] == "S", 2,
                              ifelse(prep_l[["interval"]] == "A", 1, NA))))
  if(is.na(lag)) stop("The data interval is not appropriate for y-o-y calculations")
  prep_l[["single"]] <- yoy_change(prep_l[["single"]], lag)
  prep_l$unit <- "%"
  prep_l$transf_txt <- "Transf.: medletna rast"
  prep_l
}


#' Add Year on year change to a rolling average
#'
#' Uses both transformations: first a rolling average over a set number of periods
#' (defaults to 3) and then calculates the year on year  change of that average.
#' Also adds the transformation text to be used in the plotting.
#'
#' @param prep_l prepared list with first element a dataframe with the `value` column
#' @param periods number of periods to average
#' @param align defaults to "center", "right" and "left" also options
#'
#' @return
#' @export
add_yoy_of_rolling <- function(prep_l, periods = 3, align = "center"){
  prep_l <- add_rolling_average(prep_l, periods = periods, align = align)
  prep_l$single <- subset(prep_l$single, select = -c(raw))
  prep_l <- add_yoy_change(prep_l)
  align_txt <- ifelse(align == "center", " (centr.)",
                      ifelse(align == "right", " (desne)",
                             ifelse(align == "left", " (leve)", "")))
  prep_l$transf_txt <- paste0("Transf.: medletna rast ", periods,"-",prep_l[["interval"]],
                        " drse\u010de sredine", align_txt)
  prep_l
}
