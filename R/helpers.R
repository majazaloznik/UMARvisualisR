#' Blank a series after another series' last observation
#'
#' Two series plotted together but published on different schedules leave the
#' faster one apparently running ahead of the story. This blanks \code{x} for
#' every period after the last one where \code{reference} has a value, so both
#' lines end together.
#'
#' Meant to be called from a \code{formula} cell in chart_series.csv, with the
#' reference series present as its own row with \code{plot = FALSE} — it then
#' stays in the table \link{build_chart_data} returns, so the data export shows
#' what caused the truncation. Positional, like every formula in a chart spec:
#' it assumes the table is in period order.
#'
#' @param x numeric vector to truncate
#' @param reference numeric vector whose coverage sets the cutoff
#' @return x, with everything after reference's last observation set to NA
#' @family spec helpers
#' @export
truncate_to <- function(x, reference) {
  if (length(x) != length(reference)) {
    stop("truncate_to(): x and reference must be the same length.")
  }
  last <- which(!is.na(reference))
  if (!length(last)) return(rep(NA_real_, length(x)))
  x[seq_along(x) > max(last)] <- NA
  x
}

#' Aggregate a monthly chart table to quarters
#'
#' Collapses a monthly wide table to quarters. Not a formula helper: it changes
#' the row count, so it must run on the whole table at fetch time rather than in
#' a \code{formula} cell. Chart-level by nature, since one period column means
#' every series shares a frequency.
#'
#' A quarter with fewer than \code{min_obs} observations comes back NA rather
#' than as a partial aggregate — most important with \code{sum}, where two
#' months of a three-month quarter reads as a collapse rather than as missing
#' data. The resulting NA quarters are trimmed by \link{chart_args_from_spec}
#' if no plotted series has data there.
#'
#' @param data wide table with a period column and one column per series
#' @param value_cols columns to aggregate
#' @param period_col name of the period column, monthly (YYYYMnn)
#' @param aggr_func aggregation function, e.g. mean or sum
#' @param min_obs months required before a quarter is reported; 3 (the default)
#'   allows only complete quarters
#' @return the table with one row per quarter, period column in YYYYQn form
#' @export
aggregate_to_quarters <- function(data, value_cols, period_col = "period_id",
                                  aggr_func = mean, min_obs = 3L) {
  periods    <- as.character(data[[period_col]])
  is_month   <- grepl("^\\d{4}M\\d{2}$", periods)
  is_quarter <- grepl("^\\d{4}Q[1-4]$",  periods)
  if (!all(is_month | is_quarter)) {
    stop("aggregate_to_quarters(): ", period_col,
         " must be monthly (YYYYMnn) or quarterly (YYYYQn); found '",
         periods[!(is_month | is_quarter)][1], "'.", call. = FALSE)
  }

  quarter <- ifelse(is_month,
                    paste0(substr(periods, 1, 4), "Q",
                           ceiling(as.integer(substr(periods, 6, 7)) / 3)),
                    periods)
  out_periods <- sort(unique(quarter))
  idx <- split(seq_along(quarter), factor(quarter, levels = out_periods))

  # a series that only ever reports on quarterly periods is already quarterly,
  # so one observation is a complete quarter for it
  agg_one <- function(cn) {
    x <- data[[cn]]
    need <- if (any(!is.na(x) & is_month)) min_obs else 1L
    vapply(idx, function(i) {
      v <- x[i]
      if (sum(!is.na(v)) < need) NA_real_ else as.numeric(aggr_func(v, na.rm = TRUE))
    }, numeric(1), USE.NAMES = FALSE)
  }

  res <- data.frame(stats::setNames(list(out_periods), period_col),
                    stringsAsFactors = FALSE, check.names = FALSE)
  for (cn in value_cols) res[[cn]] <- agg_one(cn)
  res
}

#' Wrap a fetch function so a whole chart comes back quarterly
#' @inheritParams aggregate_to_quarters
#' @param fetch_fn the underlying fetch function, e.g. UMARaccessR::get_series_table
#' @return a function with fetch_fn's signature
#' @export
quarterly_fetch <- function(fetch_fn, aggr_func = mean, min_obs = 3L) {
  function(codes, con, date_valid = NULL, schema = "platform") {
    raw <- fetch_fn(codes, con, date_valid = date_valid, schema = schema)
    aggregate_to_quarters(raw, value_cols = names(codes),
                          aggr_func = aggr_func, min_obs = min_obs)
  }
}

#' The fetch function a chart's spec asks for
#'
#' Most charts fetch straight from the database. A chart with
#' \code{aggregate = "Q"} in charts.csv gets its monthly data collapsed to
#' quarters first, via \link{quarterly_fetch}. Chart-level by necessity: the
#' wide table has one period column, so every series on a chart shares a
#' frequency.
#'
#' @param chart one row of the charts table
#' @param base_fetch the underlying fetch function
#' @param min_obs passed to \link{aggregate_to_quarters}
#' @return a function with base_fetch's signature
#' @export
fetch_fn_for_chart <- function(chart, base_fetch, min_obs = 3L) {
  agg <- if ("aggregate" %in% names(chart)) nz(chart$aggregate) else NULL
  if (is.null(agg)) return(base_fetch)
  if (!identical(toupper(as.character(agg)), "Q")) {
    stop("chart ", chart$chart_id, ": aggregate = '", agg,
         "' (only 'Q' is supported)", call. = FALSE)
  }
  fun_name <- if ("aggregate_fun" %in% names(chart)) nz(chart$aggregate_fun) else NULL
  fun_name <- if (is.null(fun_name)) "mean" else tolower(as.character(fun_name))
  fun <- switch(fun_name, mean = mean, sum = sum,
                stop("chart ", chart$chart_id, ": aggregate_fun = '", fun_name,
                     "' (valid: mean, sum)", call. = FALSE))
  quarterly_fetch(base_fetch, aggr_func = fun, min_obs = min_obs)
}
