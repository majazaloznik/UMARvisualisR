#' One series in the shape prep_chart's transforms expect
#'
#' Routes through the same parse step \link{prep_chart} uses
#' (\code{convert_period_column} then \code{parse_wide}) so the frame handed
#' to \code{transform_*} is identical to what they see there.
#'
#' @param period_id character or Date vector
#' @param value numeric vector
#' @return a single-series datapoints frame (Date column + value column)
#' @keywords internal
series_frame <- function(period_id, value) {
  parse_wide(convert_period_column(data.frame(period_id = period_id, value = value)))$datapoints[[1]]
}

#' Values back out of a datapoints frame
#' @keywords internal
series_values <- function(df) {
  df[[which(!vapply(df, inherits, logical(1), "Date"))[1]]]
}

#' Apply one row's transforms, in prep_chart's order
#'
#' rolling -> growth -> index, each via the same internal function
#' \link{prep_chart} calls, with its QOQ/MOM frequency check. Row count must
#' be preserved.
#'
#' @param period_id the table's period column
#' @param value numeric vector for this row
#' @param rolling,growth,index the row's transform values (NA when unused)
#' @return list(value = transformed numeric vector, base_period = resolved
#'   index base or NA)
#' @keywords internal
apply_row_transforms <- function(period_id, value, rolling, growth, index) {
  n <- length(value)
  base_period <- NA_character_
  step <- function(v, f) {
    out <- f(series_frame(period_id, v))
    if (nrow(out) != n) stop("transform changed the row count (", n, " -> ", nrow(out), ")")
    series_values(out)
  }

  if (!is.na(growth)) {
    # same data-dependent check prep_chart makes before transforming
    int <- determine_interval(series_frame(period_id, value))
    if (growth == "QOQ" && (is.na(int) || int != "Q")) stop("QOQ growth requires quarterly data")
    if (growth == "MOM" && (is.na(int) || int != "M")) stop("MOM growth requires monthly data")
    value <- step(value, \(df) transform_growth(df, type = growth))
  }

  if (!is.na(index)) {
    res <- transform_index(series_frame(period_id, value), base_period = index)
    if (nrow(res$df) != n) stop("transform changed the row count (", n, " -> ", nrow(res$df), ")")
    value <- series_values(res$df)
    base_period <- as.character(res$base_period)
  }

  if (!is.na(rolling)) value <- step(value, \(df) transform_rolling(df, periods = as.numeric(rolling), align = "r"))

  list(value = value, base_period = base_period)
}

#' Default y-axis label implied by the plotted series' transforms
#'
#' Same rule as \link{prep_chart}: \code{"\%"} when every series has a growth
#' transform; \code{"Index (<base> = 100)"} / \code{"Indeks (...)"} when every
#' series is indexed to the same base; otherwise \code{NULL}. Pass the
#' \emph{plotted} rows' columns.
#'
#' @param growth,index character vectors, one element per plotted series
#' @param language "si" or "en"
#' @return character scalar or NULL
#' @export
default_y_axis <- function(growth, index, language = "si") {
  if (length(growth) > 0 && all(!is.na(growth))) return("%")
  bases <- unique(index[!is.na(index)])
  if (length(index) > 0 && all(!is.na(index)) && length(bases) == 1) {
    return(if (language == "en") paste0("Index (", bases, " = 100)")
           else paste0("Indeks (", bases, " = 100)"))
  }
  NULL
}

#' Build the wide data table for one chart from its spec rows
#'
#' Blank aliases are filled via \link{fill_default_aliases}
#' first. Then, in position order, each row is fetched (via \code{fetch_fn}) or
#' evaluated (its \code{formula}, against the table so far), and that row's
#' \code{rolling}/\code{growth}/\code{index} are applied immediately using the
#' same functions \link{prep_chart} uses. So a formula sees earlier rows'
#' \emph{final} values, the same series raw and smoothed are two different
#' columns, and the table is what gets plotted — pass \code{NULL} for
#' \code{rolling}/\code{growth}/\code{index} to \link{prep_chart} afterwards.
#'
#' Returns every alias, plotted or not, columns in position order. Subset to
#' \code{plot == TRUE} rows before \link{prep_chart}; keep the full table for
#' the data export so computed series are auditable against their inputs.
#'
#' @param series_rows chart_series rows for exactly one chart_id
#' @param con Database connection object, passed to \code{fetch_fn}
#' @param fetch_fn \code{function(codes, con, date_valid, schema)} returning a
#'   dataframe with \code{period_id} plus one column per \code{names(codes)}.
#'   In production pass \code{UMARaccessR::get_series_table}; kept as an
#'   argument so UMARvisualisR stays DB-agnostic and tests can pass a stub.
#' @param date_valid passed to \code{fetch_fn}
#' @param schema passed to \code{fetch_fn}
#'
#' @return A dataframe with \code{period_id} and one column per alias, in
#'   position order.
#' @export
#' @importFrom rlang .data
build_chart_data <- function(series_rows, con, fetch_fn, date_valid = NULL,
                             schema = "platform") {

  chart_id <- unique(series_rows$chart_id)
  if (length(chart_id) != 1) {
    stop("series_rows must belong to exactly one chart_id, got: ",
         paste(chart_id, collapse = ", "), call. = FALSE)
  }
  if (any(series_rows$source_type == "adhoc")) {
    stop("chart ", chart_id, ": source_type 'adhoc' is not implemented yet", call. = FALSE)
  }

  series_rows <- fill_default_aliases(series_rows) |>
    dplyr::arrange(.data$position)
  db_rows <- dplyr::filter(series_rows, .data$source_type == "db")

  if (nrow(db_rows) == 0) {
    stop("chart ", chart_id, " has no 'db' series rows", call. = FALSE)
  }

  raw <- fetch_fn(rlang::set_names(db_rows$series_code, db_rows$alias),
                  con, date_valid = date_valid, schema = schema)

  # db rows first (they depend on nothing), then computed rows in position
  # order — the availability rule validate_chart_specs enforces
  order_idx <- c(which(series_rows$source_type == "db"),
                 which(series_rows$source_type == "computed"))

  wide <- purrr::reduce(order_idx, \(acc, i) {
    row <- series_rows[i, ]
    tryCatch({
      value <- if (row$source_type == "db") {
        if (!row$alias %in% names(raw)) {
          stop("fetch_fn returned no column '", row$alias, "' (columns: ",
               paste(names(raw), collapse = ", "), ")")
        }
        raw[[row$alias]]
      } else {
        v <- rlang::eval_tidy(rlang::parse_expr(row$formula), data = acc)
        if (length(v) == 1L) v <- rep_len(v, nrow(acc))
        v
      }
      value <- apply_row_transforms(acc$period_id, value,
                                    rolling = row$rolling, growth = row$growth,
                                    index = row$index)$value
      acc[[row$alias]] <- value
      acc
    }, error = function(e) {
      what <- if (row$source_type == "db") row$series_code else row$formula
      stop("chart ", chart_id, ", series '", row$alias, "' (", what, "): ",
           conditionMessage(e), call. = FALSE)
    })
  }, .init = raw[, "period_id", drop = FALSE])

  dplyr::select(wide, "period_id", dplyr::all_of(series_rows$alias))
}
