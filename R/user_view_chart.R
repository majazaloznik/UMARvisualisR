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

  op <- par(no.readonly = TRUE)
  op$mgp <- NULL
  on.exit({
    suppressWarnings(par(mgp = c(3, 1, 0)))
    suppressWarnings(par(op))
  }, add = TRUE)

  withCallingHandlers({
    # --- map to internal config format ---
    config <- to_internal_config(chart)
    datapoints <- chart$datapoints

    # --- set params ---
    title_ps <- 10.5
    legend_ps <- 8.5
    shapes <- vapply(config$series, \(x) x$type, character(1))
    bar <- any(shapes == "bar")
    line <- any(shapes == "line")
    area <- any(shapes == "area")
    # --- cut data to x limits ---
    datapoints <- cut_to_x_range(datapoints, config)

    # --- x axis setup ---
    x_axis <- x_axis_lims_tickmarks(datapoints, config)

    # --- y axis ---
    # --- y axis / axes ---
    axis_of <- vapply(config$series,
                      \(s) if (is.null(s$axis)) 1L else as.integer(s$axis), integer(1))
    dual <- any(axis_of == 2L)

    # the values that drive one axis's limits, bars and stacking included
    axis_values <- function(idx) {
      cfg <- config
      cfg$series <- config$series[idx]
      dp <- datapoints[idx]
      vals <- get_data_values(dp, cfg)
      if (config$stacked && any(shapes[idx] == "bar")) {
        bar_dp <- dp[shapes[idx] == "bar"]
        stack <- purrr::reduce(bar_dp, ~dplyr::full_join(.x, .y, by = "date")) |>
          dplyr::arrange(date)
        stack <- as.matrix(stack[, -1]); stack[is.na(stack)] <- 0
        vals <- c(vals, rowSums(pmax(stack, 0)), rowSums(pmin(stack, 0)))
      }
      vals
    }

    if (!dual) {
      if (!is.null(chart$config$ylim)) {
        y_axis <- list(ylim = chart$config$ylim, y_breaks = pretty(chart$config$ylim))
      } else {
        y_axis <- find_pretty_ylim(axis_values(seq_along(config$series)))
      }
      y_axis2 <- NULL
    } else {
      il <- which(axis_of == 1L); ir <- which(axis_of == 2L)
      ref  <- axis_reference(config$y_axis_label)
      ref2 <- axis_reference(config$y2_axis_label)
      values_left  <- axis_values(il)
      values_right <- axis_values(ir)

      scales <- pair_y_scales(values_left, values_right,
                              ylim  = chart$config$ylim,
                              ylim2 = chart$config$ylim2,
                              ref = ref, ref2 = ref2)

      for (a in dual_axis_advice(values_left, values_right,
                                 config$y_axis_label, config$y2_axis_label,
                                 shapes[il], shapes[ir], scales,
                                 ref = ref, ref2 = ref2)) {
        warning(a, call. = FALSE)
      }

      # right-hand series into left-hand coordinates - from here on there is
      # only one coordinate system and the draw functions need no changes
      rescale_to <- function(v, from, to) to[1] + (v - from[1]) * diff(to) / diff(from)
      for (i in ir) {
        datapoints[[i]]$value <- rescale_to(datapoints[[i]]$value,
                                            scales$right$ylim, scales$left$ylim)
      }
      y_axis  <- scales$left
      y_axis2 <- scales$right
    }

    # --- top margin ---
    top <- get_top_margin_and_title(config, title_ps = title_ps)
    bottom <- get_bottom_margin_and_note(config$note)

    # --- left margin ---
    left <- left_axis_label_width(config_for_axis(config, 1L), y_axis,
                                  language = config$language)
    config$y_axis_label <- left$y_axis_label
    if (dual) {
      right <- right_axis_label_width(config_for_axis(config, 2L), y_axis2,
                                      language = config$language)
      if (!bar && y_axis2$ylim[1] > 0) right$axis_labels[1] <- "//"
    }

    # format numeric labels with separators
    if (!bar && y_axis$ylim[1] > 0) {
      left$axis_labels[1] <- "//"
    }
    # --- draw ---
    if (!bar) {
      empty_plot(x_axis$x_lims, y_axis, config$y_axis_label)
      draw_emphasis(chart$config$emphasis, config$y_axis_label, y_axis$ylim)
      draw_forecast(chart$config$forecast, bar = FALSE, x_values = NULL)
      draw_areas(datapoints, config, config$y_axis_label)
      draw_lines(datapoints, config)
    }
    if (bar) {
      x_values <- base_barplot(datapoints, config, y_axis, forecast = chart$config$forecast)
      draw_emphasis(chart$config$emphasis, config$y_axis_label, y_axis$ylim)
      if(area)  stop("Oh no, you cannot combine a bar chart with an area chart.")

    }
    if (bar & line) {
      draw_lines(datapoints, config, x_values = x_values)
      if(area)  stop("Oh no, you cannot combine a bar chart with an area chart.")
    }

    # --- x axis labels ---
    par(ps = legend_ps)  # match axis label size
    x_axis <- x_axis_label_params(datapoints, config, x_axis$tickmarks,
                                  x_axis$x_lims, bar, x_values,
                                  language = config$language,
                                  interval_type = x_axis$interval_type)

    # --- legend ---
    if (n_legend_entries(config) > 0) {
      create_legend(config, legend_ps = legend_ps, language = config$language)
    }

    # --- title ---
    # par("ps" = title_ps)
    mtext(top[[3]], side = 3, line = top[[2]], adj = 0, padj = 0,
          family = umar_font(), font = 2, cex = title_ps/par("ps"))

    # --- y axis labels ---
    par("ps" = legend_ps)
    left_axis_labels(config$y_axis_label, left$axis_positions,
                     left$axis_labels, left$y_lab_lines)

    if (dual) {
      axis(4, at = y_axis$y_breaks,        # left-hand heights ...
           labels = right$axis_labels,     # ... right-hand numbers
           col = umar_cols("gridlines"), lwd = 0, tck = 0, las = 2,
           family = umar_font())
      mtext(right$y_axis_label, side = 4, line = right$y_lab_lines + 0.1,
            family = umar_font())
    }
    # --- x axis tickmarks ---
    if (length(x_axis$tickmarks) > 0 && all(is.finite(x_axis$tickmarks))) {
      if (bar) {
        axis(1, at = x_axis$tickmarks, col = umar_cols("gridlines"),
             lwd = 0, lwd.ticks = 1, tck = -0.015, labels = FALSE)
      } else {
        axis.Date(1, at = x_axis$tickmarks, col = umar_cols("gridlines"),
                  lwd = 0, lwd.ticks = 1, tck = -0.015, labels = FALSE)
      }
    }

    # --- x axis labels ---
    if (length(x_axis$x_positions) > 0 && all(is.finite(x_axis$x_positions))) {
      par_mgp(mgp = c(3, -0.2, 0))
      axis(1, x_axis$x_labels, at = x_axis$x_positions,
           col = umar_cols("gridlines"), lwd = 0, tck = 0,
           family = umar_font(), padj = 0.5, gap.axis = 0.25)
    }
    # --- note ---
    if (length(bottom$wrapped) > 0) {
      par(ps = legend_ps)
      for (i in seq_along(bottom$wrapped)) {
        mtext(bottom$wrapped[i], side = 1,
              line = 0.8 + (i - 1) * 0.8,
              adj = 0, at = par("usr")[1],
              family = umar_font())
      }
    }

    invisible(chart)
  }, warning = function(w) {
    if (grepl("mgp", conditionMessage(w))) {
      invokeRestart("muffleWarning")
    }
  })
}

#' Map umar_chart object to the internal config format
#'
#' Bridges from the user-facing structure to the format expected by the
#' existing rendering functions. Each series carries its axis, and its unit
#' comes from that axis's label, so \link{left_axis_label_width} and
#' \link{right_axis_label_width} see one unit each rather than both.
#'
#' @param chart umar_chart object
#' @return config list in internal format
#' @keywords internal
to_internal_config <- function(chart) {
  axis_of <- vapply(chart$series,
                    \(s) if (is.null(s$axis)) 1L else as.integer(s$axis), integer(1))
  units <- ifelse(axis_of == 2L,
                  chart$config$y_axis2 %||% "",
                  chart$config$y_axis  %||% "")
  series <- lapply(seq_along(chart$series), function(i) {
    s <- chart$series[[i]]
    list(
      type = s$type,
      colour = s$colour,
      linestyle = s$linestyle,
      legend_txt_si = s$legend_txt,
      legend_txt_en = s$legend_txt,
      axis = axis_of[i],
      unit = units[i],
      mio_eur = FALSE
    )
  })

  list(
    title = chart$config$title,
    y_axis_label = chart$config$y_axis,
    y2_axis_label = chart$config$y_axis2,
    xmin = chart$config$xmin,
    xmax = chart$config$xmax,
    stacked = chart$config$stacked,
    legend_columns = chart$config$legend_columns,
    x_sub_annual = FALSE,
    dual_y = any(axis_of == 2L),
    series = series,
    note = chart$config$note,
    language = chart$config$language
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
`%||%` <- function(x, y) if (is.null(x)) y else x
