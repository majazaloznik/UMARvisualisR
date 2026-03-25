#' Prepare a chart configuration from a dataframe
#'
#' Takes a dataframe in wide or long format and prepares a chart configuration
#' object that can be passed to \link[UMARvisualisR]{view_chart},
#' \link[UMARvisualisR]{save_chart}, or \link[UMARvisualisR]{get_code}.
#'
#' Wide format: first column is date (Date class), remaining columns are numeric
#' series. Column names become legend labels by default.
#'
#' Long format: exactly three columns — one Date, one numeric (values), one
#' character or factor (series names).
#'
#' @param data dataframe in wide or long format
#' @param type character, either a single value applied to all series or a vector
#'   of per-series values. Valid: "line", "bar". Defaults to "line".
#' @param title character, chart title. Defaults to NULL (no title).
#' @param y_axis character, y-axis label. Defaults to NULL.
#' @param legend character vector of legend labels. Defaults to column names (wide)
#'   or unique group values (long).
#' @param colours integer vector (indices into UMAR palette) or character vector
#'   (hex colours). Defaults to sequential palette colours.
#' @param xmin Date or character coercible to Date. Left x-axis limit.
#' @param xmax Date or character coercible to Date. Right x-axis limit.
#' @param stacked logical, stack bar series. Defaults to FALSE.
#' @param emphasis numeric value(s) for emphasised horizontal gridlines.
#'   NULL (default) auto-detects: 0 if in range, 100 if y_axis contains
#'   "indeks"/"index". FALSE disables. Numeric vector for explicit values.
#' @param legend_columns integer, number of columns in the legend. Defaults to 2.
#' @param rolling number of periods to calculate rolling mean over
#' @param growth "YOY", "MOM" or "QOQ"
#' @param index base_period either a year in YYYY format or quarter in 2023Q2 format
#' or month in 2023M03 format
#'
#' @return An object of class "umar_chart" containing the data and config.
#' @export
prep_chart <- function(data,
                       type = "line",
                       title = NULL,
                       y_axis = NULL,
                       legend = NULL,
                       colours = NULL,
                       xmin = NULL,
                       xmax = NULL,
                       stacked = FALSE,
                       emphasis = NULL,
                       legend_columns = 2,
                       rolling = NULL,
                       growth = NULL,
                       index = NULL) {

  # --- convert period strings to Date if needed ---
  data <- convert_period_column(data)

  # --- detect and validate format ---
  format <- detect_format(data)

  # convert long to wide
  if (format == "long") {
    parsed <- parse_long(data)
  } else {
    parsed <- parse_wide(data)
  }
  # parsed is a list: datapoints (list of dfs with date + value), series_names

  n_series <- length(parsed$series_names)

  # --- validate type ---
  type <- validate_type(type, n_series)

  # --- validate colours ---
  colours <- validate_colours(colours, n_series)

  # --- validate dates ---
  xmin <- validate_date(xmin, "xmin")
  xmax <- validate_date(xmax, "xmax")

  if (!is.null(xmin) && !is.null(xmax) && xmin >= xmax) {
    stop("xmin must be earlier than xmax.")
  }

  # --- validate stacked ---
  if (stacked && !any(type == "bar")) {
    stop("stacked = TRUE requires at least one 'bar' series.")
  }

  # --- validate emphasis ---
  if (!is.null(emphasis) && !isFALSE(emphasis)) {
    if (!is.numeric(emphasis)) stop("emphasis must be NULL, FALSE, or a numeric vector.")
  }

  # --- validate transformations ---
  if (!is.null(rolling)) {
    if (!is.numeric(rolling) || length(rolling) != 1 || rolling < 2)
      stop("rolling must be a single integer >= 2.")
  }
  if (!is.null(growth)) {
    if (!growth %in% c("YOY", "QOQ", "MOM"))
      stop("growth must be 'YOY', 'QOQ', or 'MOM'.")
  }
  if (!is.null(index)) {
    if (!grepl("^\\d{4}(Q\\d|M\\d{2})?$", index))
      stop("index must be a year ('2015'), quarter ('2023Q1'), or month ('2023M06').")
  }
  if (!is.null(growth) && !is.null(index)) {
    stop("growth and index are mutually exclusive.")
  }

  # --- dual y axis: not yet ---
  # reserved for future use

  # --- apply transformations ---
  if (!is.null(rolling)) {
    parsed$datapoints <- lapply(parsed$datapoints, transform_rolling,
                                periods = rolling, align = "r")
  }
  if (!is.null(growth)) {
    parsed$datapoints <- lapply(parsed$datapoints, transform_growth, type = growth)
    if (is.null(y_axis)) y_axis <- "%"
  }
  if (!is.null(index)) {
    results <- lapply(parsed$datapoints, transform_index, base_period = index)
    parsed$datapoints <- lapply(results, `[[`, "df")
    # use the (possibly corrected) base_period from first series
    index <- results[[1]]$base_period
    if (is.null(y_axis)) y_axis <- paste0("Indeks (", index, " = 100)")
  }

  # --- build legend ---
  if (is.null(legend)) {
    legend <- parsed$series_names
  } else {
    if (length(legend) != n_series) {
      stop(paste0("legend must have ", n_series, " elements (one per series)."))
    }
  }

  # --- build config ---
  config <- list(
    title = title,
    y_axis = y_axis,
    xmin = xmin,
    xmax = xmax,
    stacked = stacked,
    emphasis = emphasis,
    legend_columns = legend_columns,
    rolling = rolling,
    growth = growth,
    index = index
  )

  series <- lapply(seq_len(n_series), function(i) {
    list(
      series_name = parsed$series_names[i],
      type = type[i],
      colour = colours[i],
      legend_txt = legend[i]
    )
  })

  structure(
    list(
      datapoints = parsed$datapoints,
      config = config,
      series = series
    ),
    class = "umar_chart"
  )
}


# --- internal helpers ---

#' Detect whether dataframe is wide or long format
#' @param data dataframe
#' @return "wide" or "long"
#' @keywords internal
detect_format <- function(data) {
  if (!is.data.frame(data)) stop("data must be a data.frame.")
  if (ncol(data) < 2) stop("data must have at least 2 columns.")

  date_col <- find_date_column(data)
  if (is.null(date_col)) stop("No Date column found. Ensure one column is of class Date.")

  remaining <- data[, setdiff(names(data), date_col), drop = FALSE]
  n_numeric <- sum(vapply(remaining, is.numeric, logical(1)))
  n_char <- sum(vapply(remaining, function(x) is.character(x) || is.factor(x), logical(1)))

  if (ncol(remaining) == 2 && n_numeric == 1 && n_char == 1) {
    return("long")
  }
  if (n_numeric == ncol(remaining) && n_numeric >= 1) {
    return("wide")
  }
  stop("Cannot detect format. Wide: date + numeric columns. Long: date + one numeric + one character/factor column.")
}

#' Find the first Date-class column
#' @param data dataframe
#' @return column name or NULL
#' @keywords internal
find_date_column <- function(data) {
  date_cols <- names(data)[vapply(data, inherits, logical(1), "Date")]
  if (length(date_cols) == 0) return(NULL)
  if (length(date_cols) > 1) {
    warning("Multiple Date columns found, using '", date_cols[1], "'.")
  }
  date_cols[1]
}

#' Parse wide format into standard internal structure
#' @param data dataframe
#' @return list with datapoints (list of dfs) and series_names
#' @keywords internal
parse_wide <- function(data) {
  date_col <- find_date_column(data)
  dates <- data[[date_col]]
  value_cols <- setdiff(names(data), date_col)

  datapoints <- lapply(value_cols, function(col) {
    data.frame(date = dates, value = data[[col]])
  })

  list(datapoints = datapoints, series_names = value_cols)
}

#' Parse long format into standard internal structure
#' @param data dataframe
#' @return list with datapoints (list of dfs) and series_names
#' @keywords internal
parse_long <- function(data) {
  date_col <- find_date_column(data)
  remaining <- data[, setdiff(names(data), date_col), drop = FALSE]

  value_col <- names(remaining)[vapply(remaining, is.numeric, logical(1))]
  group_col <- names(remaining)[vapply(remaining, function(x) {
    is.character(x) || is.factor(x)
  }, logical(1))]

  groups <- unique(data[[group_col]])

  datapoints <- lapply(groups, function(g) {
    subset <- data[data[[group_col]] == g, ]
    df <- data.frame(date = subset[[date_col]], value = subset[[value_col]])
    df[order(df$date), ]
  })

  list(datapoints = datapoints, series_names = as.character(groups))
}

#' Validate and recycle type argument
#' @keywords internal
validate_type <- function(type, n_series) {
  valid <- c("line", "bar")
  if (!all(type %in% valid)) stop("type must be 'line' or 'bar'.")

  if (length(type) == 1) {
    rep(type, n_series)
  } else if (length(type) == n_series) {
    type
  } else {
    stop(paste0("type must be length 1 or ", n_series, " (one per series)."))
  }
}

#' Validate and fill colours
#' @keywords internal
validate_colours <- function(colours, n_series) {
  palette <- umar_cols()

  if (is.null(colours)) {
    return(unname(palette[seq_len(n_series)]))
  }

  if (length(colours) != n_series) {
    stop(paste0("colours must have ", n_series, " elements (one per series)."))
  }

  vapply(colours, function(c) {
    if (is.numeric(c)) {
      if (c < 1 || c > 8) stop("Colour index must be between 1 and 8.")
      unname(palette[c])
    } else if (is.character(c) && grepl("^#", c)) {
      c
    } else {
      stop("colours must be integers (1-8) or hex strings (e.g. '#A10305').")
    }
  }, character(1))
}

#' Validate date argument
#' @keywords internal
validate_date <- function(d, name) {
  if (is.null(d)) return(NULL)
  if (inherits(d, "Date")) return(d)
  result <- tryCatch(as.Date(d), error = function(e) NULL)
  if (is.null(result) || is.na(result)) {
    stop(paste0(name, " must be a Date or a string coercible to Date (e.g. '2010-01-01')."))
  }
  result
}

#' Print method for umar_chart objects
#' @param x umar_chart object
#' @param ... ignored
#' @export
print.umar_chart <- function(x, ...) {
  n <- length(x$series)
  types <- vapply(x$series, \(s) s$type, character(1))
  cat("UMAR chart configuration\n")
  cat("  Series:", n, "\n")
  cat("  Types: ", paste(types, collapse = ", "), "\n")
  if (!is.null(x$config$title)) cat("  Title: ", x$config$title, "\n")
  if (!is.null(x$config$y_axis))  cat("  Y-axis title:  ", x$config$y_axis, "\n")
  for (i in seq_len(n)) {
    s <- x$series[[i]]
    cat(sprintf("  [%d] %s (%s, %s)\n", i, s$legend_txt, s$type,
                colourise(s$colour, s$colour)))
  }
  invisible(x)
}

#' Colourise text with ANSI true colour
#' @param text character string to colourise
#' @param hex hex colour string
#' @return colourised string or plain text if terminal doesn't support it
#' @keywords internal
colourise <- function(text, hex) {
  tryCatch({
    style <- cli::make_ansi_style(hex)
    style(text)
  }, error = function(e) text)
}


#' Convert period string column to Date
#'
#' Detects character columns with period patterns (2023M01, 2023Q1, 2023)
#' and converts them to Date class in-place.
#'
#' @param data dataframe
#' @return dataframe with period column converted to Date (if found)
#' @keywords internal
convert_period_column <- function(data) {
  # only act if no Date column exists already
  if (!is.null(find_date_column(data))) return(data)

  char_cols <- names(data)[vapply(data, is.character, logical(1))]
  for (col in char_cols) {
    vals <- stats::na.omit(data[[col]])
    if (length(vals) == 0) next
    interval <- get_interval(vals[1])
    if (is.na(interval)) next
    # all values must match the same pattern
    intervals <- vapply(vals, get_interval, character(1))
    if (!all(intervals == interval, na.rm = TRUE)) next
    # convert
    data[[col]] <- switch(interval,
                          M = lubridate::ym(data[[col]], quiet = TRUE),
                          Q = lubridate::yq(data[[col]], quiet = TRUE),
                          A = lubridate::ymd(data[[col]], truncated = 2L, quiet = TRUE)
    )
    return(data)
  }
  data
}
