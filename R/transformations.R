

#' New function for rolling average transformations
#'
#' @param df dataframe with date and value columns
#' @param periods number of periods to be averaged over
#' @param align alignment method - center is the default, right is correct and
#' left i don't know why it even exists
#'
#' @return dataframe with transformed values.
#' @export
#'
transform_rolling <- function(df, periods = 3, align = "c"){
  if(is.na(align)) align <- "c"
  df  |>
    dplyr::arrange(date)  |>
    dplyr::mutate(value = zoo::rollmean(value, k = periods,fill= NA, align = align))
}

#' New function for growth transformations
#'
#' @param df dataframe with date and value columns
#' @param type valid are YOY, QOQ and MOM
#'
#' @return df with updated valeus
#' @export
#'
transform_growth <- function(df, type = "YOY"){
  lag_matrix <- matrix(c(1, NA, NA,
                         4, 1, NA,
                         12, NA, 1),
                       nrow=3, byrow=TRUE,
                       dimnames=list(c(12, 3, 1), c("YOY", "QOQ", "MOM")))
  interval <- as.character(get_interval_in_months(df$date))
  lag_value <- lag_matrix[interval, type]
  df |>
    dplyr::arrange(date) |>
    dplyr::mutate(value = value/dplyr::lag(value, n = lag_value)*100 - 100)
}


#' Function for transformation to an index with a specific base period
#'
#' Calculates the index based on the period. The following options are allowed:
#'
#' + the base period is a year: the average will be taken of the months or quarters,
#' of course this works fine for annual data as well.
#' + if the base period is a quarter, you can only do the transformation on
#' quarterly data - but this is checked in the check_plot_inputs function
#' + if the base period is a month, you can also only use it on monthly data.
#'
#' The format of the base_period is not checked because if is checked in
#' \link[UMARvisualisR]{check_plot_inputs} already.
#'
#' If the base period does not exist in the data, the oldest value is used
#' and a warning is issued to that effect.
#'
#' @param df data frame with date and value columns
#' @param base_period either a year in YYYY format or quarter in 2023Q2 format
#' or month in 2023M03 format
#'
#' @return dataframe with trnsformed values
#' @export
#'
transform_index <- function(df, base_period){
  if (grepl("^\\d{4}$", base_period)) {
    base_value <- df |>
      dplyr::filter(lubridate::year(date) == base_period) |>
      dplyr::summarise(value= mean(value, na.rm = TRUE)) |>
      dplyr::pull(value)
    if(is.na(base_value) || is.nan(base_value) || length(base_value) == 0) {
      base_period <- lubridate::year(df |> na.omit() |>
                                       dplyr::summarise(min = min(date)) |>
                                       dplyr::pull())
      base_value <- df |>
        dplyr::filter(lubridate::year(date) == base_period) |>
        dplyr::summarise(value= mean(value, na.rm = TRUE)) |>
        dplyr::pull(value)
      warning("Bazno obdobje indeksa je spremenjeno na ", base_period, ".")
    }
  }
  if (grepl("^\\d{4}Q\\d{1}$", base_period)){
    base_start <- lubridate::yq(base_period)
    base_end <- base_start + months(3)
    base_value <- df  |>
      dplyr::filter(date >= base_start & date < base_end) |>
      dplyr::pull(value)
    if (is.na(base_value) || is.nan(base_value) || length(base_value) == 0) {
      base_record <-  df  |>
        dplyr::filter(!is.na(value))  |>
        dplyr::slice(1)
      base_value <- base_record$value
      base_period <- paste0(lubridate::year(base_record$date), "Q",
                            lubridate::quarter(base_record$date))
      warning("Bazno obdobje indeksa je spremenjeno na ", base_period, ".")
    }
  }
  if (grepl("^\\d{4}M\\d{2}$", base_period)) {
    base_start <- lubridate::ym(base_period)
    base_end <- base_start + months(1)
    base_value <- df  |>
      dplyr::filter(date >= base_start & date < base_end) |>
      dplyr::pull(value)
    if (is.na(base_value) || is.nan(base_value) || length(base_value) == 0) {
      base_record <-  df  |>
        dplyr::filter(!is.na(value))  |>
        dplyr::slice(1)
      base_value <- base_record$value
      base_period <- paste0(lubridate::year(base_record$date), "M",
                            sprintf("%02d", lubridate::month(base_record$date)))
      warning("Bazno obdobje indeksa je spremenjeno na ", base_period, ".")
    }
  }
  df <- df |>
    dplyr::mutate(value = value/base_value*100)
  list(df = df, base_period = base_period)
}
