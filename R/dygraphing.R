#' Dygraph line charts
#'
#' Plots univariate and multivariate line chart to dygraph interactive charts,
#' approx in line with our corporate identity.
#'
#' Data input must be prepared with the \link[UMARvisualisR]{prep_multi_line} function.
#'
#'
#' @param prep_l list of length 8+ with data.frame with data, the unit used and the
#' main and sub titles etc.. see \link[UMARvisualisR]{prep_multi_line}.
#'
#' @return nothing, plots to open device
#' @export
#'
dygraph_plotter <- function(prep_l) {
  list2env(prep_l, envir = environment())
# a single rolling average gets special treatment.
  if(ifelse(is.null(transf_txt), F, grepl("drse\u010da sredina", transf_txt)) &
     length(data_points) == 1) {
    data_points[[1]] |>
      dplyr::relocate( period) |>
      dplyr::select(-period_id) |>
      dygraphs::dygraph(main = paste("Posodobljeno:", updated, "   ", transf_txt)) |>
      dygraphs::dyOptions(colors=c(unname(umar_cols()[c(1, 3)])),
                strokeWidth = 1.5) |>
      dygraphs::dyAxis("x", drawGrid = c(FALSE)) |>
      dygraphs::dyCSS("dygraph.css") |>
      dygraphs::dyRangeSelector() |>
      dygraphs::dyLegend(show = "always", hideOnMouseOut = FALSE) |>
      dygraphs::dySeries("raw", label = sub_title[[1]]) |>
      dygraphs::dySeries("value", label = transf_txt) |>
      dygraphs::dyLegend(width = 600) |>
      dygraphs::dyAxis("y", label = prep_l$unit) |>
      dygraphs::dyLimit(0, strokePattern = "solid", color = unname(umar_cols("emph"))) -> chart
  } else {
    purrr::reduce(data_points, dplyr::left_join, by = c("period_id", "period")) %>%
      dplyr::relocate( period) |>
      dplyr::select(-dplyr::starts_with("raw")) |>
      dplyr::select(-period_id) -> data
    data_colz <- colnames(data)[-1]
    data |>
      dygraphs::dygraph(main = paste("Posodobljeno:", updated, "   ", transf_txt)) |>
      dygraphs::dyOptions(colors=c(unname(umar_cols()[1:length(data_colz)])),
                strokeWidth = 1.5) |>
      dygraphs::dyAxis("x", drawGrid = c(FALSE)) |>
      dygraphs::dyCSS("dygraph.css") |>
      dygraphs::dyRangeSelector() |>
      dygraphs::dyLegend(show = "always", hideOnMouseOut = FALSE) |>
      dygraphs::dyLegend(width = 600) |>
      dygraphs::dyAxis("y", label = unit)  -> chart
    # add labels to the legend
    if(length(data_points) == 1){
      legend_labels <- sub_title[[1]]}
    for(i in seq_along(data_colz)){
      chart <- chart |>
        dygraphs::dySeries(data_colz[i], label = legend_labels[i])}
  }
  # emphasised gridline
  if(unit %in% c("Indeks", "Index", "indeks", "index", "%")) {
    chart <- chart |>
      dygraphs::dyLimit(100, strokePattern = "solid", color = unname(umar_cols("emph")))
      }
  chart |>
    dygraphs::dyLimit(0, strokePattern = "solid", color = unname(umar_cols("emph")))
}



#' Dygraph bar and combo charts
#'
#' Plots univariate barchart and multivariate combo chart to dygraph interactive charts,
#' approx in line with our corporate identity.
#'
#' If only a single series is passed to the funciton it will automaticlaly print
#' a barchart
#'
#' if more are passed the default is that the first series becomes a barchart and the
#' rest are line (bar = TRUE), alternatively if bar == FALSE that means the first one
#' is a line chart and the rest are a stacked barchart. In both cases a single y-axis
#' is used.
#'
#' Data input must be prepared with the \link[UMARvisualisR]{prep_multi_line} function.
#'
#'
#' @param prep_l list of length 8+ with data.frame with data, the unit used and the
#' main and sub titles etc.. see \link[UMARvisualisR]{prep_multi_line}.
#' @param bar logical if first series is plotted as a bar and the rest line or vice versa.
#'
#' @return
#' @export
dygraph_plotter_mixed <- function(prep_l, bar = TRUE) {
  list2env(prep_l, envir = environment())
  # a single rolling average gets special treatment.
  if(length(data_points) == 1) {
    data_points[[1]] |>
      dplyr::relocate( period) |>
      dplyr::select(-period_id) |>
      dygraphs::dygraph(main = paste("Posodobljeno:", updated, "   ", transf_txt)) |>
      dygraphs::dyOptions(colors=c(unname(umar_cols()[c(1)])),
                          strokeWidth = 1.5) |>
      dygraphs::dyAxis("x", drawGrid = c(FALSE)) |>
      dygraphs::dyCSS("dygraph.css") |>
      dygraphs::dyRangeSelector() |>
      dygraphs::dyLegend(show = "always", hideOnMouseOut = FALSE) |>
      dygraphs::dyBarSeries("value", label = sub_title[[1]]) |>
      dygraphs::dyLegend(width = 600) |>
      dygraphs::dyAxis("y", label = prep_l$unit) -> chart
  } else {
    if(bar){
      purrr::reduce(data_points, dplyr::left_join, by = c("period_id", "period")) %>%
        dplyr::relocate( period) |>
        dplyr::select(-dplyr::starts_with("raw")) |>
        dplyr::select(-period_id) -> data
      colnames(data)[-1] <- legend_labels
      data_colz <- colnames(data)[-1]
      data |>
        dygraphs::dygraph(main = paste("Posodobljeno:", updated, "   ", transf_txt)) |>
        dygraphs::dyOptions(colors=c(unname(umar_cols()[1:length(data_colz)])),
                            strokeWidth = 1.5) |>
        dygraphs::dyAxis("x", drawGrid = c(FALSE)) |>
        dygraphs::dyCSS("dygraph.css") |>
        dygraphs::dyRangeSelector() |>
        dygraphs::dyLegend(show = "always", hideOnMouseOut = FALSE) |>
        dygraphs::dyLegend(width = 600) |>
        dygraphs::dyAxis("y", label = unit) |>
        dygraphs::dyBarSeries(data_colz[1]) -> chart

      for(i in seq_along(data_colz)[-1]){
        chart <-     chart |>
          dygraphs::dySeries(data_colz[i])
      }


    } else { # stacked bar chart with single line
      purrr::reduce(data_points, dplyr::left_join, by = c("period_id", "period")) %>%
        dplyr::relocate( period) |>
        dplyr::select(-dplyr::starts_with("raw")) |>
        dplyr::select(-period_id) -> data

      data %>%
        dplyr::mutate(sum = rowSums(select(., -(1:2)), na.rm = TRUE)) |>
        select(sum) |>
        range() |>
        pretty(n = 10) |>
        range() -> ylim

      colnames(data)[-1] <- legend_labels
      data_colz <- colnames(data)[-1]
      data |>
        dygraphs::dygraph(main = paste("Posodobljeno:", updated, "   ", transf_txt)) |>
        dygraphs::dyStackedBarGroup(data_colz[-1]) |>
        dyAxis("y", label = unit, valueRange = ylim) |>
        dygraphs::dyOptions(colors=c(unname(umar_cols()[1:length(data_colz)])),
                            strokeWidth = 1.5) |>
        dygraphs::dyAxis("x", drawGrid = c(FALSE)) |>
        dygraphs::dyCSS("dygraph.css") |>
        dygraphs::dyRangeSelector() |>
        dygraphs::dyLegend(show = "always", hideOnMouseOut = FALSE) |>
        dygraphs::dyLegend(width = 600) |>
        dygraphs::dySeries(data_colz[1]) -> chart
    }
  }

  # emphasised gridline
  if(unit %in% c("Indeks", "Index", "indeks", "index", "%")) {
    chart <- chart |>
      dygraphs::dyLimit(100, strokePattern = "solid", color = unname(umar_cols("emph")))
  }
  chart |>
    dygraphs::dyLimit(0, strokePattern = "solid", color = unname(umar_cols("emph")))
}

