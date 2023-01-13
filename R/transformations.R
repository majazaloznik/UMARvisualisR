rolling_average <- function(df, periods = 3, align = "center"){
  df %>%
    dplyr::arrange(period) %>%
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
#' @param data_points prepared list of dataframes with the `value` column
#' @param periods number of periods to average
#' @param align defaults to "center", "right" and "left" also options
#'
#' @return same list with the df with the `value` column overwritten and the old
#' values stored in the `raw` column, plus an element with the text for describing
#' the transformation.
#'
#' @export
add_rolling_average <- function(data_points, periods = 3, align = "center") {
  data_points <- rolling_average(data_points, periods = periods, align = align)
  align_txt <- ifelse(align == "c", " (centrirana)",
                      ifelse(align == "r", " (desna)",
                             ifelse(align == "l", " (leva)", "")))
  mget(c("data_points", "align_txt"))
}


#' Add year on year change column to prepared data
#'
#' Year-on-year change is calculated based on the type of interval and overwrites the
#' values of the original data. The unit is also changed to "%", adn the
#' transformation text is added.
#'
#' @param data_points prepared list of dataframes with the `value` column
#' @param interval char id of interval e.g. "M"
#'
#' @return same list with the df with one the value column overwritten and new element
#' with  text for describing the transformation and the unit overwritten as well.
#' @export
#'
add_yoy_change <- function(data_points, interval) {
  lag <- ifelse(interval == "M", 12,
                ifelse(interval == "Q", 4,
                       ifelse(interval == "S", 2,
                              ifelse(interval == "A", 1, NA))))
  if(is.na(lag)) stop("Trenutno je mogo\u010e medletne spremembe ra\u010unati samo za mese\u010ne, \u010etrtletne, ali letne podatke.")
  data_points <- yoy_change(data_points, lag)
  unit <- "Medletna rast, v %"
  transf_txt <- "Transf.: medletna rast"
  mget(c("data_points", "unit", "transf_txt"))

}


#' Add Year on year change to a rolling average
#'
#' Uses both transformations: first a rolling average over a set number of periods
#' (defaults to 3) and then calculates the year on year  change of that average.
#' Also adds the transformation text to be used in the plotting.
#'
#' @param data_points prepared list of dataframes with the `value` column
#' @param periods number of periods to average
#' @param align defaults to "center", "right" and "left" also options
#' @param interval char id of interval e.g. "M"
#'
#' @return same list with the df with one the value column overwritten and new element
#' with  text for describing the transformation and the unit overwritten as well.
#'
#' @export
add_yoy_of_rolling <- function(data_points, periods = 3, align = "center", interval){
  data_points <- add_rolling_average(data_points, periods = periods, align = align)$data_points
  data_points <- subset(data_points, select = -c(raw))
  data_points <- add_yoy_change(data_points, interval)$data_points
  align_txt <- ifelse(align == "c", " (centr.)",
                      ifelse(align == "r", " (desne)",
                             ifelse(align == "l", " (leve)", "")))
  unit <- "Medletna rast, v %"
  mget(c("data_points", "unit", "align_txt"))
}


#' Umbrella fun to do needed transformations on the data
#'
#' This funciton takes a list of inputs, which must include the `data_points` and
#' transformation parameters, and applies the transformations, returning the same list
#' with transformed data points and the added `transf_txt` element.
#'
#' @param input_data list with dataframe called `data_points`
#'
#' @return list with transformed data points
#' @export
#'
do_transformations <- function(input_data){
  data_points <- input_data$data_points
  rolling_average_periods <- input_data$rolling_average_periods
  rolling_average_alignment <- input_data$rolling_average_alignment
  year_on_year <- input_data$year_on_year
  interval <- input_data$interval
  unit <- input_data$unit

  # transformations:
  periods = unique(rolling_average_periods)
  align = unique(rolling_average_alignment)
  if (!is.na(periods)) rolling <- TRUE else rolling <- FALSE
  if (!is.na(unique(year_on_year)) & unique(year_on_year)) yoy <- TRUE else yoy <- FALSE

  if (rolling | yoy) {
    print(paste0("Za\u010denjam s transformacijami podatkov."))
    if(rolling & yoy){
      yoyd_rolld <- purrr::map(data_points, add_yoy_of_rolling,
                               periods, align, interval )
      data_points <- purrr::map(yoyd_rolld, 1)

      transf_txt <- paste0("Transf.: medletna rast ", periods,"-", interval,
                           " drse\u010de sredine")
      unit <- purrr::map(yoyd_rolld, 2)[[1]]
    } else {
      if(rolling){
        rolld  <- purrr::map(data_points, add_rolling_average,
                             periods,
                             align)
        data_points <- purrr::map(rolld, 1)
        transf_txt <- paste0("Transf.: ", periods,"-",interval,
                             " drse\u010da sredina", purrr::map(rolld, 2)[[1]])
        unit <- ifelse(is.null(unit), NA, unit)
      }
      if(yoy) {yoyd <- purrr::map(data_points, add_yoy_change, interval)
      data_points <- purrr::map(yoyd, 1)
      transf_txt <- purrr::map(yoyd, 3)[[1]]
      unit <- purrr::map(yoyd, 2)[[1]]
      }
    }
  } else {transf_txt <- NULL
  unit <- NA}
  output_data <- input_data
  output_data$data_points <- data_points
  output_data$transf_txt <- transf_txt
  output_data$unit <- unit
  return(output_data)
}
