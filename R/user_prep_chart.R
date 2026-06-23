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
#' character or factor (series names) or multiple character or factor which get
#' concatented into series names unless legend in given.
#'
#' @param data dataframe in wide or long format
#' @param type character, either a single value applied to all series or a vector
#'   of per-series values. Valid: "line", "bar" or "area" (for max 2 series). Defaults to "line".
#' @param title character, chart title. Defaults to NULL (no title).
#' @param y_axis character, y-axis label. Defaults to NULL.
#' @param legend character vector of legend labels. Defaults to column names (wide)
#'   or unique group values (long).
#' @param colours integer vector (indices into UMAR palette) or character vector
#'   (hex colours). Defaults to sequential palette colours.
#' @param dashed integer vector for which series should be drawn dashed
#' @param dotted integer vector for which series should be drawn dotted
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
#' @param ylim numeric(2) manual y-axis limits, e.g. c(90, 120).
#' NULL (default) auto-detects. Ignored for bar charts (must include 0).
#' @param note character string, possibly with \\n breaks
#' @param forecast character or date vector of lenght 2 to determine extend of
#' gray background shading.
#'
#' @return An object of class "umar_chart" containing the data and config.
#' @export
prep_chart <- function(data,
                       type = "line",
                       title = NULL,
                       y_axis = NULL,
                       legend = NULL,
                       colours = NULL,
                       dashed = NULL,
                       dotted = NULL,
                       xmin = NULL,
                       xmax = NULL,
                       stacked = FALSE,
                       emphasis = NULL,
                       legend_columns = 2,
                       rolling = NULL,
                       growth = NULL,
                       index = NULL,
                       ylim = NULL,
                       note = NULL,
                       forecast = NULL) {

  # --- first sanity check ---
  if (!is.data.frame(data)) stop("data must be a data.frame.")

  # --- defactor ---
  data[] <- lapply(data, function(x) if (is.factor(x)) as.character(x) else x)

  # --- convert POSIXct to Date ---
  posix_cols <- names(data)[vapply(data, inherits, logical(1), "POSIXct")]
  for (col in posix_cols) {
    data[[col]] <- as.Date(data[[col]])
  }

  # ---  drop character columns with only one unique value ---
  if (nrow(data) > 1) {
    const_cols <- names(data)[vapply(data, function(x) {
      is.character(x) && length(unique(stats::na.omit(x))) <= 1
    }, logical(1))]
    if (length(const_cols)) {
      message("Dropping constant columns: ", paste(const_cols, collapse = ", "))
      data <- data[, setdiff(names(data), const_cols), drop = FALSE]
    }
  }

  # --- convert period strings to Date if needed ---
  data <- convert_period_column(data)

  # --- drop extra date columns ---
  date_cols <- names(data)[vapply(data, inherits, logical(1), "Date")]
  if (length(date_cols) > 1) {
    warning("Multiple Date columns found. Keeping '", date_cols[1],
            "', dropping: ", paste(date_cols[-1], collapse = ", "))
    data <- data[, setdiff(names(data), date_cols[-1]), drop = FALSE]
  }

  # --- collapse multiple category columns ---
  data <- collapse_categories(data)

  # --- detect and validate format ---
  format <- detect_format(data)

  # --- check duplicates ---
  check_duplicates(data)

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

  # --- validate area ---
  area_indices <- which(type == "area")
  n_area <- length(area_indices)
  if (n_area > 2) {
    stop("Maximum 2 series can be type 'area' (single area or ribbon between two).")
  }

  # --- validate series number
  if (n_series > 8) {
    warning("Maximum 8 series supported. Keeping the first 8.")
    parsed$datapoints <- parsed$datapoints[1:8]
    parsed$series_names <- parsed$series_names[1:8]
    n_series <- 8
  }
  # --- validate colours ---
  area_indices <- which(type == "area")
  # warn if user explicitly passed non-NA colours for area positions
  if (!is.null(colours) && length(area_indices) > 0) {
    explicit <- colours[area_indices]
    explicit_non_na <- explicit[!is.na(explicit)]
    if (length(explicit_non_na)) {
      warning("Colours for area series are always gray. Ignoring user value(s) at position(s): ",
              paste(area_indices[!is.na(explicit)], collapse = ", "),
              ". Use NA to suppress this warning.")
    }
  }
  colours <- validate_colours(colours, n_series, area_indices)

  # --- validate line styles ---
  validate_linestyle <- function(x, name, n_series, type) {
    if (is.null(x)) return(NULL)
    if (!is.numeric(x) || !all(x == round(x)))
      stop(name, " must be integer index/indices.")
    if (any(x < 1 | x > n_series))
      stop(name, " indices must be between 1 and ", n_series, ".")
    non_line <- x[type[x] != "line"]
    if (length(non_line)) {
      warning(name, " ignored for non-line series: ", paste(non_line, collapse = ", "))
      x <- x[type[x] == "line"]
    }
    if (length(x) == 0) NULL else x
  }
  dashed <- validate_linestyle(dashed, "dashed", n_series, type)
  dotted <- validate_linestyle(dotted, "dotted", n_series, type)

  if (length(intersect(dashed, dotted))) {
    stop("A series cannot be both dashed and dotted: ",
         paste(intersect(dashed, dotted), collapse = ", "))
  }

  # --- validate dates ---
  xmin <- validate_date(xmin, "xmin")
  xmax <- validate_date(xmax, "xmax")

  if (!is.null(xmin) && !is.null(xmax) && xmin >= xmax) {
    stop("xmin must be earlier than xmax.")
  }

  # --- validate stacked ---
  if (stacked && !any(type == "bar")) {
    warning("stacked = TRUE ignored: no 'bar' series present.")
    stacked <- FALSE
  }
  # --- validate emphasis ---
  if (!is.null(emphasis) && !isFALSE(emphasis)) {
    if (!is.numeric(emphasis)) stop("emphasis must be NULL, FALSE, or a numeric vector.")
  }

  # in prep_chart() validation
  if (!is.null(forecast)) {
    if (!is.character(forecast) && !inherits(forecast, "Date") || length(forecast) != 2) {
      stop("forecast must be a character or Date vector of length 2.")
    }
    forecast <- as.Date(forecast)
    if (is.na(forecast[1]) || is.na(forecast[2])) stop("forecast dates couldn't be parsed.")
    if (forecast[1] >= forecast[2]) stop("forecast[1] must be earlier than forecast[2].")
  }

  # --- validate y_axis ---
  if (!is.null(y_axis) && (!is.character(y_axis) || length(y_axis) != 1)) {
    stop("y_axis is the axis title and must be a single character string. For manual axis limits use ylim.")
  }
  # --- validate transformations ---
  if (!is.null(rolling)) {
    if (!is.numeric(rolling) || length(rolling) != 1 || rolling < 2)
      stop("rolling must be a single integer >= 2.")
  }
  if (!is.null(growth)) {
    if (!growth %in% c("YOY", "QOQ", "MOM"))
      stop("growth must be 'YOY', 'QOQ', or 'MOM'.")
    if (growth == "QOQ" && determine_interval(parsed$datapoints[[1]]) != "Q")
      stop("QOQ growth requires quarterly data.")
    if (growth == "MOM" && determine_interval(parsed$datapoints[[1]]) != "M")
      stop("MOM growth requires monthly data.")
  }

  if (!is.null(index)) {
    if (!grepl("^\\d{4}(Q\\d|M\\d{2})?$", index))
      stop("index must be a year ('2015'), quarter ('2023Q1'), or month ('2023M06').")
  }
  if (!is.null(growth) && !is.null(index)) {
    stop("growth and index are mutually exclusive.")
  }

  # --- validate ylim ---
  if (!is.null(ylim)) {
    if (!is.numeric(ylim) || length(ylim) != 2) stop("ylim must be numeric(2).")
    if (ylim[1] >= ylim[2]) stop("ylim[1] must be less than ylim[2].")
    if (any(type == "bar") && ylim[1] > 0) {
      warning("ylim[1] forced to 0 for bar charts.")
      ylim[1] <- 0
    }
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

  parsed$datapoints <- center_dates(parsed$datapoints)
  # --- build legend ---
  legend_arg <- legend  # save before defaults fill in
  # if user supplied legend, check length, allow NA at area positions
  if (!is.null(legend_arg)) {
    if (length(legend_arg) != n_series) {
      stop("legend must have ", n_series, " elements.")
    }
    legend <- legend_arg
  } else {
    legend <- parsed$series_names
  }

  # area legend handling
  if (length(area_indices) >= 1) {
    if (length(area_indices) == 2) {
      second <- area_indices[2]
      if (!is.null(legend_arg) && !is.na(legend_arg[second])) {
        warning("Legend for second area series ignored. ",
                "Use NA at position ", second, " to suppress.")
      }
      legend[second] <- NA
    }
    # first area series legend stays as-is (either user-supplied or column name)
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
    index = index,
    ylim = ylim,
    note = note,
    forecast = forecast
  )

  series <- lapply(seq_len(n_series), function(i) {
    style <- if (i %in% dashed) "dashed"
    else if (i %in% dotted) "dotted"
    else "solid"
    list(
      series_name = parsed$series_names[i],
      type = type[i],
      colour = colours[i],
      legend_txt = legend[i],
      linestyle = style
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
  valid <- c("line", "bar", "area")
  if (!all(type %in% valid)) stop("type must be 'line' or 'bar' or 'area'.")

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
validate_colours <- function(colours, n_series, area_indices = integer(0)) {
  palette <- umar_cols()
  if (is.null(colours)) {
    out <- unname(palette[seq_len(n_series)])
  } else {
    if (length(colours) != n_series) {
      stop(paste0("colours must have ", n_series, " elements (one per series)."))
    }
    out <- vapply(seq_along(colours), function(i) {
      c <- colours[[i]]
      if (is.na(c)) {
        if (i %in% area_indices) return(NA_character_)
        stop("Colour at position ", i, " is NA but series is not area type.")
      }
      if (is.numeric(c)) {
        if (c < 1 || c > 9) stop("Colour index must be between 1 and 9.")
        unname(palette[c])
      } else if (is.character(c) && grepl("^#", c)) {
        c
      } else {
        stop("colours must be integers (1-9) or hex strings (e.g. '#A10305').")
      }
    }, character(1))
  }
  # fill area positions with gray
  if (length(area_indices)) {
    out[area_indices] <- unname(umar_cols("siva"))
  }
  out
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
  if (!is.null(find_date_column(data))) return(data)

  # check numeric columns that look like years
  num_cols <- names(data)[vapply(data, is.numeric, logical(1))]
  for (col in num_cols) {
    vals <- stats::na.omit(data[[col]])
    if (length(vals) == 0) next
    if (all(vals >= 1900 & vals <= 2100 & vals == floor(vals))) {
      data[[col]] <- as.Date(paste0(vals, "-01-01"))
      return(data)
    }
  }

  char_cols <- names(data)[vapply(data, is.character, logical(1))]
  for (col in char_cols) {
    vals <- stats::na.omit(data[[col]])
    if (length(vals) == 0) next
    interval <- get_interval(vals[1])
    if (is.na(interval)) next
    intervals <- vapply(vals, get_interval, character(1))
    if (!all(intervals == interval, na.rm = TRUE)) next
    data[[col]] <- switch(interval,
                          M = lubridate::ym(data[[col]], quiet = TRUE),
                          Q = lubridate::yq(data[[col]], quiet = TRUE),
                          A = lubridate::ymd(data[[col]], truncated = 2L, quiet = TRUE)
    )
    return(data)
  }
  data
}


#' Shift dates to mid-period for proper chart alignment
#' @param datapoints list of date+value dataframes
#' @return datapoints with centered dates
#' @keywords internal
center_dates <- function(datapoints) {
  interval <- determine_interval(datapoints[[1]])
  if (is.na(interval) || interval == "A") return(datapoints)
  lapply(datapoints, function(df) {
    if (interval == "M") {
      month_start <- lubridate::floor_date(df$date, "month")
      df$date <- month_start + (lubridate::days_in_month(month_start) - 1) / 2
    } else if (interval == "Q") {
      quarter_start <- lubridate::floor_date(df$date, "quarter")
      df$date <- quarter_start + 45
    }
    df
  })
}

#' Collapse multiple character columns into one
#' @keywords internal
collapse_categories <- function(data) {
  date_col <- find_date_column(data)
  if (is.null(date_col)) {
    # try period columns too
    char_cols <- names(data)[vapply(data, is.character, logical(1))]
    num_cols <- names(data)[vapply(data, is.numeric, logical(1))]
  } else {
    remaining <- data[, setdiff(names(data), date_col), drop = FALSE]
    char_cols <- names(remaining)[vapply(remaining, is.character, logical(1))]
    num_cols <- names(remaining)[vapply(remaining, is.numeric, logical(1))]
  }
  if (length(char_cols) > 1 && length(num_cols) == 1) {
    data$category <- do.call(paste, c(data[char_cols], sep = " - "))
    data <- data[, c(if (!is.null(date_col)) date_col,
                     setdiff(names(data), c(date_col, char_cols, "category")),
                     "category"), drop = FALSE]
    # keep: date, numeric, combined category
    data <- data[, c(if (!is.null(date_col)) date_col, num_cols, "category")]
    message("Multiple category columns collapsed into one: ",
            paste(char_cols, collapse = ", "))
  }
  data
}


#' Check for duplicate date-value pairs per series
#' @keywords internal
check_duplicates <- function(data) {
  fmt <- detect_format(data)
  if (fmt == "long") {
    date_col <- find_date_column(data)
    remaining <- data[, setdiff(names(data), date_col), drop = FALSE]
    group_col <- names(remaining)[vapply(remaining, function(x) {
      is.character(x) || is.factor(x)
    }, logical(1))]
    dupes <- duplicated(data[, c(date_col, group_col)])
    if (any(dupes)) {
      first_dupe <- data[which(dupes)[1], ]
      stop("Duplicate rows found. First duplicate: ",
           first_dupe[[date_col]], " / ", first_dupe[[group_col]],
           ". Ensure each series has one value per period.")
    }
  } else {
    date_col <- find_date_column(data)
    if (any(duplicated(data[[date_col]]))) {
      stop("Duplicate dates found. Ensure each date appears only once in wide format.")
    }
  }
}
