rolling_average <- function(df, periods = 3, align = "center"){
  df %>%
    dplyr::mutate(rolling = zoo::rollmean(value, k = periods,fill= NA,align = align))
}
