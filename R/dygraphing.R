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
      dygraph(main = paste("Posodobljeno:", updated, "   ", transf_txt)) |>
      dyOptions(colors=c(unname(umar_cols()[c(1, 3)])),
                strokeWidth = 1.5) |>
      dyAxis("x", drawGrid = c(FALSE)) |>
      dyCSS("dygraph.css") |>
      dyRangeSelector() |>
      dyLegend(show = "always", hideOnMouseOut = FALSE) |>
      dySeries("raw", label = sub_title[[1]]) |>
      dySeries("value", label = transf_txt) |>
      dyLegend(width = 600) |>
      dyAxis("y", label = prep_l$unit) |>
      dyLimit(0, strokePattern = "solid", color = unname(umar_cols("emph"))) -> chart
  } else {
    purrr::reduce(data_points, dplyr::left_join, by = c("period_id", "period")) %>%
      dplyr::relocate( period) |>
      dplyr::select(-dplyr::starts_with("raw")) |>
      dplyr::select(-period_id) -> data
    data_colz <- colnames(data)[-1]
    data |>
      dygraph(main = paste("Posodobljeno:", updated, "   ", transf_txt)) |>
      dyOptions(colors=c(unname(umar_cols()[1:length(data_colz)])),
                strokeWidth = 1.5) |>
      dyAxis("x", drawGrid = c(FALSE)) |>
      dyCSS("dygraph.css") |>
      dyRangeSelector() |>
      dyLegend(show = "always", hideOnMouseOut = FALSE) |>
      dyLegend(width = 600) |>
      dyAxis("y", label = unit)  -> chart
    # add labels to the legend
    if(length(data_points) == 1){
      legend_labels <- sub_title[[1]]}
    for(i in seq_along(data_colz)){
      chart <- chart |>
        dySeries(data_colz[i], label = legend_labels[i])}
  }
  # emphasised gridline
  if(unit %in% c("Indeks", "Index", "indeks", "index", "%")) {
    chart <- chart |>
      dyLimit(100, strokePattern = "solid", color = unname(umar_cols("emph")))
      }
  chart |>
      dyLimit(0, strokePattern = "solid", color = unname(umar_cols("emph")))
}

