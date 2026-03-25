#' Save a chart to PDF or PNG
#'
#' Renders the chart and saves to file. Format is determined by the \code{format}
#' parameter or the file extension if one is provided.
#'
#' @param chart an object of class "umar_chart" from \link[UMARvisualisR]{prep_chart}
#' @param filename character path to output file. If no extension is provided,
#'   one is added based on \code{format}.
#' @param format character, "pdf" or "png". Defaults to "pdf". Ignored if
#'   \code{filename} already has a valid extension.
#' @param size integer, chart size: 1 (small), 2 (normal), 3 (large). Defaults to 2.
#'
#' @return invisible chart object
#' @export
save_chart <- function(chart, filename, format = "pdf", size = 2) {
  if (!inherits(chart, "umar_chart")) stop("chart must be a 'umar_chart' object from prep_chart().")
  if (missing(filename)) stop("filename is required. E.g. save_chart(chart, 'my_chart')")
  if (!size %in% c(1, 2, 3)) stop("size must be 1 (small), 2 (normal), or 3 (large).")
  if (!format %in% c("pdf", "png")) stop("format must be 'pdf' or 'png'.")

  ext <- tolower(tools::file_ext(filename))

  if (ext %in% c("pdf", "png")) {
    # extension present — use it
    if (ext != format) {
      warning("Using extension '.", ext, "' from filename (ignoring format = '", format, "').")
    }
    format <- ext
  } else if (ext == "") {
    # no extension — append format
    filename <- paste0(filename, ".", format)
  } else {
    stop("Unsupported file extension '.", ext, "'. Use .pdf or .png.")
  }

  size_name <- c("small", "normal", "large")[size]

  if (format == "pdf") {
    pdf_output(filename, size_name)
  } else {
    png_output(filename, size_name)
  }

  view_chart(chart)
  grDevices::dev.off()

  message("Chart saved to ", filename)
  invisible(chart)
}


#' Generate reproducible code for a chart
#'
#' Returns the full \code{prep_chart()} call with all parameters
#' explicitly specified, so the chart can be reproduced or modified.
#'
#' @param chart an object of class "umar_chart" from \link[UMARvisualisR]{prep_chart}
#'
#' @return character string with the code (also printed to console)
#' @export
get_code <- function(chart) {
  if (!inherits(chart, "umar_chart")) stop("chart must be a 'umar_chart' object from prep_chart().")

  types <- vapply(chart$series, \(s) s$type, character(1))
  colours <- vapply(chart$series, \(s) s$colour, character(1))
  legends <- vapply(chart$series, \(s) s$legend_txt, character(1))

  lines <- c("prep_chart(data,")

  # type
  if (length(unique(types)) == 1) {
    lines <- c(lines, paste0('  type = "', types[1], '",'))
  } else {
    lines <- c(lines, paste0("  type = c(", paste0('"', types, '"', collapse = ", "), "),"))
  }

  # title
  if (!is.null(chart$config$title)) {
    lines <- c(lines, paste0('  title = "', chart$config$title, '",'))
  }

  # y_axis
  if (!is.null(chart$config$y_axis)) {
    lines <- c(lines, paste0('  y_axis = "', chart$config$y_axis, '",'))
  }

  # legend
  lines <- c(lines, paste0("  legend = c(", paste0('"', legends, '"', collapse = ", "), "),"))

  # colours
  coloured_hex <- vapply(colours, \(c) colourise(paste0('"', c, '"'), c), character(1))
  lines <- c(lines, paste0("  colours = c(", paste(coloured_hex, collapse = ", "), "),"))

  # xmin/xmax
  if (!is.null(chart$config$xmin)) {
    lines <- c(lines, paste0('  xmin = "', chart$config$xmin, '",'))
  }
  if (!is.null(chart$config$xmax)) {
    lines <- c(lines, paste0('  xmax = "', chart$config$xmax, '",'))
  }

  # stacked
  if (chart$config$stacked) {
    lines <- c(lines, "  stacked = TRUE,")
  }

  # emphasis
  if (isFALSE(chart$config$emphasis)) {
    lines <- c(lines, "  emphasis = FALSE,")
  } else if (!is.null(chart$config$emphasis)) {
    lines <- c(lines, paste0("  emphasis = c(",
                             paste(chart$config$emphasis, collapse = ", "), "),"))
  }

  # transformations
  if (!is.null(chart$config$rolling)) {
    lines <- c(lines, paste0("  rolling = ", chart$config$rolling, ","))
  }
  if (!is.null(chart$config$growth)) {
    lines <- c(lines, paste0('  growth = "', chart$config$growth, '",'))
  }
  if (!is.null(chart$config$index)) {
    lines <- c(lines, paste0('  index = "', chart$config$index, '",'))
  }

  # legend_columns
  lines <- c(lines, paste0("  legend_columns = ", chart$config$legend_columns, ")"))

  code <- paste(lines, collapse = "\n")

  # add pipe to view
  code <- paste0(code, " |>\n  view_chart()")

  cat(code, "\n")
  invisible(code)
}
