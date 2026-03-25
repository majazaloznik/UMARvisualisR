#' Plot a prepared chart to the current device
#'
#' Takes a chart object from \link[UMARvisualisR]{prep_chart} and renders it
#' to the current graphics device.
#'
#' @param chart an object of class "umar_chart" from \link[UMARvisualisR]{prep_chart}
#'
#' @return invisible chart object (for piping)
#' @export
view_chart <- function(chart) {
  if (!inherits(chart, "umar_chart")) stop("chart must be a 'umar_chart' object from prep_chart().")

  # --- map to internal config format ---
  config <- to_internal_config(chart)
  datapoints <- chart$datapoints

  # --- set params ---
  title_ps <- 10
  legend_ps <- 10
  shapes <- vapply(config$series, \(x) x$type, character(1))
  bar <- any(shapes == "bar")
  line <- any(shapes == "line")

  # --- cut data to x limits ---
  datapoints <- cut_to_x_range(datapoints, config)

  # --- x axis setup ---
  x_axis <- x_axis_lims_tickmarks(datapoints, config)

  # --- y axis ---
  values <- get_data_values(datapoints, config)
  y_axis <- find_pretty_ylim(values)

  # --- left margin ---
  left <- left_axis_label_width(config, y_axis)
  if (is.null(config$y_axis_label)) config$y_axis_label <- left$unit

  # --- top margin ---
  top <- get_top_margin_and_title(config, title_ps = title_ps)

  # --- draw ---
  if (!bar) {
    empty_plot(x_axis$x_lims, y_axis, config$y_axis_label)
    draw_emphasis(chart$config$emphasis, config$y_axis_label, y_axis$ylim)
    draw_lines(datapoints, config)
    x_values <- NULL
  }
  if (bar) {
    x_values <- base_barplot(datapoints, config, y_axis)
    draw_emphasis(chart$config$emphasis, config$y_axis_label, y_axis$ylim)
  }
  if (bar & line) {
    draw_lines(datapoints, config, x_values = x_values)
  }

  # --- x axis labels ---
  x_axis <- x_axis_label_params(datapoints, config, x_axis$tickmarks,
                                x_axis$x_lims, bar, x_values)

  # --- legend ---
  if (length(config$series) > 1) {
    create_legend(config, legend_ps = legend_ps)
  }

  # --- title ---
  par("ps" = title_ps)
  mtext(top[[3]], side = 3, line = top[[2]], adj = 0, padj = 0,
        family = umar_font(), font = 2)

  # --- y axis labels ---
  par("ps" = 9)
  left_axis_labels(config$y_axis_label, left$axis_positions,
                   left$axis_labels, left$y_lab_lines)

  # --- x axis tickmarks ---
  if (bar) {
    axis(1, at = x_axis$tickmarks, col = umar_cols("gridlines"),
         lwd = 0, lwd.ticks = 1, tck = -0.02, labels = FALSE)
  } else {
    axis.Date(1, at = x_axis$tickmarks, col = umar_cols("gridlines"),
              lwd = 0, lwd.ticks = 1, tck = -0.02, labels = FALSE)
  }

  # --- x axis labels ---
  par_mgp(mgp = c(3, -0.2, 0))
  axis(1, x_axis$x_labels, at = x_axis$x_positions,
       col = umar_cols("gridlines"), lwd = 0, tck = 0,
       family = umar_font(), padj = 0.5, gap.axis = 0.25)

  invisible(chart)
}


#' Map umar_chart object to the internal config format
#'
#' Bridges from the user-facing structure to the format expected
#' by the existing rendering functions.
#'
#' @param chart umar_chart object
#' @return config list in internal format
#' @keywords internal
to_internal_config <- function(chart) {
  series <- lapply(chart$series, function(s) {
    list(
      type = s$type,
      colour = s$colour,
      legend_txt_si = s$legend_txt,
      legend_txt_en = s$legend_txt,
      unit = chart$config$y_axis %||% "",
      mio_eur = FALSE
    )
  })

  list(
    title = chart$config$title,
    y_axis_label = chart$config$y_axis,
    xmin = chart$config$xmin,
    xmax = chart$config$xmax,
    stacked = chart$config$stacked,
    legend_columns = chart$config$legend_columns,
    x_sub_annual = FALSE,
    dual_y = FALSE,
    series = series
  )
}

#' Draw emphasis gridlines
#' @param emphasis NULL (auto), FALSE (none), or numeric vector
#' @param y_axis_label character y-axis label (for auto-detection)
#' @param y_lims numeric(2) y-axis limits
#' @keywords internal
draw_emphasis <- function(emphasis, y_axis_label, y_lims) {
  if (isFALSE(emphasis)) return(invisible())
  if (is.null(emphasis)) {
    vals <- numeric(0)
    if (y_lims[1] < 0 && y_lims[2] > 0) vals <- c(vals, 0)
    if (!is.null(y_axis_label) &&
        grepl("indeks|index", y_axis_label, ignore.case = TRUE) &&
        y_lims[1] < 100 && y_lims[2] > 100) vals <- c(vals, 100)
  } else {
    vals <- emphasis[emphasis > y_lims[1] & emphasis < y_lims[2]]
  }
  if (length(vals)) abline(h = vals, col = umar_cols("emph"), lwd = 1)
}


#' Null-coalescing operator
#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else y
