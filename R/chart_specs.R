#' Variable names referenced by a formula string
#'
#' @param formula character scalar holding an R expression
#' @return character vector of referenced names (empty for a constant), or
#'   \code{NA_character_} if the string does not parse as a single expression
#' @keywords internal
formula_vars <- function(formula) {
  tryCatch(all.vars(str2lang(formula)), error = function(e) NA_character_)
}

#' Aliases a formula at a given position may reference
#'
#' \code{period_id}, every non-computed alias in the chart regardless of
#' position, and every computed alias at an earlier position. Mirrors the
#' evaluation order in \link{build_chart_data}: fetched series are all
#' available before any formula runs; computed series are evaluated
#' sequentially by position.
#'
#' @param chart_series full chart_series table
#' @param chart_id chart to look in
#' @param position position of the formula row
#' @return character vector of aliases
#' @keywords internal
available_aliases <- function(chart_series, chart_id, position) {
  rows <- chart_series[chart_series$chart_id == chart_id, ]
  fetched <- rows$alias[rows$source_type != "computed"]
  earlier_computed <- rows$alias[rows$source_type == "computed" & rows$position < position]
  c("period_id", fetched, earlier_computed)
}

#' Default alias derived from a series code or a formula, plus transforms
#'
#' For a series code: lowercased, runs of non-alphanumerics collapsed to
#' \code{_}, trimmed, prefixed with \code{x_} if it would start with a digit.
#' For a formula (\code{formula = TRUE}) the operators are spelled out first
#' so they survive that step: \code{+ - * / ^} become
#' \code{_plus_ _minus_ _times_ _div_ _pow_}, a leading \code{-} becomes
#' \code{neg_}. Transforms are appended as suffixes in application order:
#' \code{_roll<n>}, \code{_<growth>}, \code{_idx<base>}.
#'
#' Deterministic, so it survives position edits; two rows collide only if
#' they show the same thing — same code (or formula) with the same transforms.
#'
#' @param x character vector of series codes, or formulas if \code{formula = TRUE}
#' @param growth,index,rolling the row's transform columns (NA when unused)
#' @param formula logical, is \code{x} a formula rather than a code
#' @return character vector of aliases matching \code{^[a-z][a-z0-9_]*$}
#' @export
default_alias <- function(x, growth = NA, index = NA, rolling = NA, formula = FALSE) {
  base <- trimws(x)
  if (isTRUE(formula)) {
    base <- sub("^-", "neg_", base)
    base <- gsub("+", "_plus_", base, fixed = TRUE)
    base <- gsub("-", "_minus_", base, fixed = TRUE)
    base <- gsub("*", "_times_", base, fixed = TRUE)
    base <- gsub("/", "_div_", base, fixed = TRUE)
    base <- gsub("^", "_pow_", base, fixed = TRUE)
  }
  base <- gsub("[^a-z0-9]+", "_", tolower(base))
  base <- gsub("^_+|_+$", "", base)
  base <- ifelse(grepl("^[0-9]", base), paste0("x_", base), base)
  paste0(
    base,
    ifelse(is.na(rolling), "", paste0("_roll", rolling)),
    ifelse(is.na(growth),  "", paste0("_", tolower(growth))),
    ifelse(is.na(index),   "", paste0("_idx", tolower(index)))
  )
}

#' Fill blank aliases from the series code (db rows) or formula (computed rows)
#'
#' Idempotent. Rows with neither (a db row with no code, a computed row with
#' no formula) are left blank for \link{validate_chart_specs} to report.
#' Applied internally by the validator and \link{build_chart_data}; the
#' driver gets it via \link{read_chart_specs}.
#'
#' @param chart_series chart_series table
#' @return chart_series with \code{alias} filled where derivable
#' @export
fill_default_aliases <- function(chart_series) {
  needed <- c("alias", "source_type", "series_code", "formula", "growth", "index", "rolling")
  missing <- setdiff(needed, names(chart_series))
  if (length(missing) > 0) {
    stop("chart_series is missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  blank <- function(x) is.na(x) | x == ""
  computed <- chart_series$source_type %in% "computed"
  base <- ifelse(computed, chart_series$formula, chart_series$series_code)
  derivable <- blank(chart_series$alias) & !blank(base)
  fill <- function(rows) {
    if (!any(rows)) return(invisible())
    chart_series$alias[rows] <<- default_alias(
      base[rows], formula = all(computed[rows]),
      growth = chart_series$growth[rows], index = chart_series$index[rows],
      rolling = chart_series$rolling[rows])
  }
  fill(derivable & computed)
  fill(derivable & !computed)
  chart_series
}

#' Validate the chart spec tables for structural/referential problems
#'
#' Returns a tibble of problems (zero rows = clean). Pure and testable on its
#' own; \link{check_chart_specs} wraps it with a fail-loud stop().
#'
#' Titles are optional but must be given in both languages or neither.
#' Blank aliases on 'db' rows are filled via \link{fill_default_aliases}.
#' Transform columns follow the same rules \link{prep_chart} enforces:
#' \code{rolling} an integer >= 2, \code{growth} one of YOY/QOQ/MOM,
#' \code{index} a year/quarter/month, growth and index mutually exclusive,
#' at most 8 plotted series and at most 2 of type "area".
#'
#' Required columns - charts: \code{chart_id, title_sl, title_en};
#' chart_series: \code{chart_id, position, alias, source_type, series_code,
#' formula, plot, legend_sl, legend_en, type, growth, index, rolling}.
#' Other columns are ignored.
#'
#' @param charts tibble read from charts.csv
#' @param chart_series tibble read from chart_series.csv
#'
#' @return A tibble with columns \code{check}, \code{chart_id}, \code{detail},
#'   one row per problem.
#' @export
#' @importFrom rlang .data
validate_chart_specs <- function(charts, chart_series) {

  required_charts <- c("chart_id", "title_sl", "title_en")
  required_series <- c("chart_id", "position", "alias", "source_type", "series_code",
                       "formula", "plot", "legend_sl", "legend_en", "type",
                       "growth", "index", "rolling")
  missing_cols <- c(
    if (length(m <- setdiff(required_charts, names(charts))))
      paste0("charts: ", paste(m, collapse = ", ")),
    if (length(m <- setdiff(required_series, names(chart_series))))
      paste0("chart_series: ", paste(m, collapse = ", "))
  )
  if (length(missing_cols) > 0) {
    stop("Missing required columns - ", paste(missing_cols, collapse = "; "), call. = FALSE)
  }

  valid_source_types <- c("db", "computed", "adhoc")
  valid_types <- c("line", "bar", "area")
  valid_growth <- c("YOY", "QOQ", "MOM")
  index_pattern <- "^\\d{4}(Q\\d|M\\d{2})?$"
  blank <- function(x) is.na(x) | x == ""

  chart_series <- fill_default_aliases(chart_series) |>
    dplyr::mutate(plot_lgl = as.logical(.data$plot))
  plotted <- dplyr::filter(chart_series, .data$plot_lgl %in% TRUE)

  # --- charts.csv --------------------------------------------------------------

  dup_chart_id <- charts |>
    dplyr::count(.data$chart_id) |>
    dplyr::filter(.data$n > 1) |>
    dplyr::transmute(check = "duplicate chart_id in charts",
                     chart_id = .data$chart_id,
                     detail = paste0("appears ", .data$n, " times"))

  # paired columns: both blank or both filled (only checked when both exist)
  pair_check <- function(a, b, check) {
    if (!all(c(a, b) %in% names(charts))) return(NULL)
    charts |>
      dplyr::filter(blank(.data[[a]]) != blank(.data[[b]])) |>
      dplyr::transmute(check = check,
                       chart_id = .data$chart_id,
                       detail = paste0(a, " and ", b, " must be both blank or both filled"))
  }
  unpaired <- dplyr::bind_rows(
    pair_check("title_sl", "title_en", "title given in one language only"),
    pair_check("y_axis_sl", "y_axis_en", "y_axis given in one language only"),
    pair_check("note_sl", "note_en", "note given in one language only"),
    pair_check("ylim_min", "ylim_max", "ylim given on one side only"),
    pair_check("forecast_start", "forecast_end", "forecast given on one side only")
  )

  # dates: blank, ISO, or period form - locale forms would parse silently wrong
  date_pattern <- "^(\\d{4}-\\d{2}-\\d{2}|\\d{4}M\\d{2}|\\d{4}Q[1-4])$"
  date_check <- function(col) {
    if (!col %in% names(charts)) return(NULL)
    x <- charts[[col]]
    if (inherits(x, "Date")) return(NULL)
    charts |>
      dplyr::filter(!blank(.data[[col]]), !grepl(date_pattern, trimws(as.character(.data[[col]])))) |>
      dplyr::transmute(check = "invalid date",
                       chart_id = .data$chart_id,
                       detail = paste0(col, " = '", .data[[col]],
                                       "' must be YYYY-MM-DD, YYYYMnn or YYYYQn"))
  }
  bad_dates <- dplyr::bind_rows(date_check("xmin"), date_check("xmax"),
                                date_check("forecast_start"), date_check("forecast_end"))

  headers_without_series <- charts |>
    dplyr::anti_join(chart_series, by = "chart_id") |>
    dplyr::transmute(check = "chart has no series rows",
                     chart_id = .data$chart_id,
                     detail = "no rows in chart_series for this chart_id")

  # --- chart_series.csv: keys --------------------------------------------------

  orphan_series <- chart_series |>
    dplyr::anti_join(charts, by = "chart_id") |>
    dplyr::distinct(.data$chart_id) |>
    dplyr::transmute(check = "chart_series row has no matching chart header",
                     chart_id = .data$chart_id,
                     detail = "no row in charts with this chart_id")

  dup_position <- chart_series |>
    dplyr::count(.data$chart_id, .data$position) |>
    dplyr::filter(.data$n > 1) |>
    dplyr::transmute(check = "duplicate position within chart_id",
                     chart_id = .data$chart_id,
                     detail = paste0("position ", .data$position, " appears ", .data$n, " times"))

  dup_alias <- chart_series |>
    dplyr::count(.data$chart_id, .data$alias) |>
    dplyr::filter(.data$n > 1) |>
    dplyr::transmute(check = "duplicate alias within chart_id",
                     chart_id = .data$chart_id,
                     detail = paste0("alias '", .data$alias, "' appears ", .data$n,
                                     " times (same series with the same transforms? give one an explicit alias)"))

  bad_alias <- chart_series |>
    dplyr::filter(is.na(.data$alias) |
                    !grepl("^[a-z][a-z0-9_]*$", .data$alias) |
                    .data$alias == "period_id") |>
    dplyr::transmute(check = "missing or invalid alias",
                     chart_id = .data$chart_id,
                     detail = paste0("position ", .data$position, ": '", .data$alias,
                                     "' must match ^[a-z][a-z0-9_]*$ and not be 'period_id'",
                                     " (blank is only allowed on 'db' rows, where it is derived from the code)"))

  # --- chart_series.csv: source ------------------------------------------------

  bad_source_type <- chart_series |>
    dplyr::filter(!.data$source_type %in% valid_source_types) |>
    dplyr::transmute(check = "invalid source_type",
                     chart_id = .data$chart_id,
                     detail = paste0("position ", .data$position, ": '", .data$source_type,
                                     "' (valid: ", paste(valid_source_types, collapse = ", "), ")"))

  missing_code_for_db <- chart_series |>
    dplyr::filter(.data$source_type %in% "db", blank(.data$series_code)) |>
    dplyr::transmute(check = "source_type 'db' with no series_code",
                     chart_id = .data$chart_id,
                     detail = paste0("position ", .data$position))

  computed <- dplyr::filter(chart_series, .data$source_type %in% "computed")

  missing_formula <- computed |>
    dplyr::filter(blank(.data$formula)) |>
    dplyr::transmute(check = "source_type 'computed' with no formula",
                     chart_id = .data$chart_id,
                     detail = paste0("position ", .data$position, " (alias '", .data$alias, "')"))

  with_formula <- computed |>
    dplyr::filter(!blank(.data$formula)) |>
    dplyr::mutate(
      vars = purrr::map(.data$formula, formula_vars),
      unparseable = purrr::map_lgl(.data$vars, \(v) length(v) == 1L && is.na(v))
    )

  bad_formula <- with_formula |>
    dplyr::filter(.data$unparseable) |>
    dplyr::transmute(check = "formula does not parse",
                     chart_id = .data$chart_id,
                     detail = paste0("alias '", .data$alias, "': ", .data$formula))

  unresolved_refs <- with_formula |>
    dplyr::filter(!.data$unparseable) |>
    dplyr::mutate(
      available = purrr::map2(.data$chart_id, .data$position,
                              \(cid, pos) available_aliases(chart_series, cid, pos)),
      missing = purrr::map2(.data$vars, .data$available, setdiff)
    ) |>
    dplyr::filter(lengths(.data$missing) > 0) |>
    dplyr::transmute(check = "formula references unknown alias",
                     chart_id = .data$chart_id,
                     detail = paste0("alias '", .data$alias, "' references: ",
                                     purrr::map_chr(.data$missing, paste, collapse = ", "),
                                     " (must be a db/adhoc alias, or a computed alias at an earlier position)"))

  # --- chart_series.csv: transforms (same rules as prep_chart) -----------------

  bad_rolling <- chart_series |>
    dplyr::filter(!is.na(.data$rolling),
                  is.na(suppressWarnings(as.numeric(.data$rolling))) |
                    as.numeric(.data$rolling) < 2 |
                    as.numeric(.data$rolling) != round(as.numeric(.data$rolling))) |>
    dplyr::transmute(check = "invalid rolling",
                     chart_id = .data$chart_id,
                     detail = paste0("alias '", .data$alias, "': '", .data$rolling,
                                     "' must be NA or an integer >= 2"))

  bad_growth <- chart_series |>
    dplyr::filter(!is.na(.data$growth), !.data$growth %in% valid_growth) |>
    dplyr::transmute(check = "invalid growth",
                     chart_id = .data$chart_id,
                     detail = paste0("alias '", .data$alias, "': '", .data$growth,
                                     "' must be NA or one of ", paste(valid_growth, collapse = "/")))

  bad_index <- chart_series |>
    dplyr::filter(!is.na(.data$index), !grepl(index_pattern, .data$index)) |>
    dplyr::transmute(check = "invalid index",
                     chart_id = .data$chart_id,
                     detail = paste0("alias '", .data$alias, "': '", .data$index,
                                     "' must be NA, a year ('2015'), quarter ('2023Q1') or month ('2023M06')"))

  growth_index_clash <- chart_series |>
    dplyr::filter(!is.na(.data$growth), !is.na(.data$index)) |>
    dplyr::transmute(check = "growth and index on the same series",
                     chart_id = .data$chart_id,
                     detail = paste0("alias '", .data$alias, "': they are mutually exclusive"))

  # --- chart_series.csv: plotting ---------------------------------------------

  bad_plot <- chart_series |>
    dplyr::filter(is.na(.data$plot_lgl)) |>
    dplyr::transmute(check = "plot must be TRUE or FALSE",
                     chart_id = .data$chart_id,
                     detail = paste0("position ", .data$position, ": '", .data$plot, "'"))

  missing_legend <- plotted |>
    dplyr::arrange(.data$chart_id, .data$position) |>
    dplyr::group_by(.data$chart_id) |>
    dplyr::mutate(area_rank = cumsum(.data$type %in% "area")) |>
    dplyr::ungroup() |>
    dplyr::filter(!(.data$type %in% "area" & .data$area_rank == 2)) |>   # prep_chart NAs it anyway
    dplyr::filter(blank(.data$legend_sl) | blank(.data$legend_en)) |>
    dplyr::transmute(check = "missing legend_sl/legend_en on plotted series",
                     chart_id = .data$chart_id,
                     detail = paste0("alias '", .data$alias, "'"))

  bad_type <- plotted |>
    dplyr::filter(!.data$type %in% valid_types) |>
    dplyr::transmute(check = "missing or invalid type on plotted series",
                     chart_id = .data$chart_id,
                     detail = paste0("alias '", .data$alias, "': '", .data$type,
                                     "' (valid: ", paste(valid_types, collapse = ", "), ")"))

  too_many_plotted <- plotted |>
    dplyr::count(.data$chart_id) |>
    dplyr::filter(.data$n > 8) |>
    dplyr::transmute(check = "more than 8 plotted series",
                     chart_id = .data$chart_id,
                     detail = paste0(.data$n, " plotted; prep_chart supports at most 8"))

  too_many_area <- plotted |>
    dplyr::filter(.data$type %in% "area") |>
    dplyr::count(.data$chart_id) |>
    dplyr::filter(.data$n > 2) |>
    dplyr::transmute(check = "more than 2 area series",
                     chart_id = .data$chart_id,
                     detail = paste0(.data$n, " area series; prep_chart supports at most 2"))

  dplyr::bind_rows(
    dup_chart_id, unpaired, bad_dates, headers_without_series,
    orphan_series, dup_position, dup_alias, bad_alias,
    bad_source_type, missing_code_for_db, missing_formula, bad_formula, unresolved_refs,
    bad_rolling, bad_growth, bad_index, growth_index_clash,
    bad_plot, missing_legend, bad_type, too_many_plotted, too_many_area
  )
}

#' Fail loud if the chart spec tables have structural problems
#'
#' @inheritParams validate_chart_specs
#' @return Invisibly, the (empty) problems tibble, if it doesn't stop first.
#' @export
check_chart_specs <- function(charts, chart_series) {
  problems <- validate_chart_specs(charts, chart_series)
  if (nrow(problems) > 0) {
    stop(
      "Chart spec validation failed:\n",
      paste(sprintf("- [%s] chart_id=%s: %s", problems$check, problems$chart_id, problems$detail),
            collapse = "\n"),
      call. = FALSE
    )
  }
  invisible(problems)
}

#' NULL for blank, otherwise the value
#' @keywords internal
nz <- function(x) {
  if (length(x) == 0 || is.na(x) || identical(trimws(as.character(x)), "")) NULL else x
}

#' A column if present, else a default recycled to the row count
#' @keywords internal
col_or <- function(df, col, default) {
  if (col %in% names(df)) df[[col]] else rep(default, nrow(df))
}

#' Parse a spec date: ISO (YYYY-MM-DD) or period (YYYYMnn / YYYYQn), else stop
#'
#' Deliberately refuses locale forms like \code{01/01/2022}: \code{as.Date()}
#' would silently read that as year 1. Period form maps to the first day of
#' the period. \code{Date} input passes through.
#'
#' @param x scalar: character, Date, or blank/NA
#' @param what column name, for the error message
#' @return a Date, or NULL if blank
#' @export
parse_spec_date <- function(x, what = "date") {
  if (is.null(nz(x))) return(NULL)
  if (inherits(x, "Date")) return(x)
  x <- trimws(as.character(x))
  if (grepl("^\\d{4}-\\d{2}-\\d{2}$", x)) return(as.Date(x))
  if (grepl("^\\d{4}M\\d{2}$", x)) return(as.Date(sprintf("%s-%s-01", substr(x, 1, 4), substr(x, 6, 7))))
  if (grepl("^\\d{4}Q[1-4]$", x)) {
    month <- (as.integer(substr(x, 6, 6)) - 1) * 3 + 1
    return(as.Date(sprintf("%s-%02d-01", substr(x, 1, 4), month)))
  }
  stop(what, " = '", x, "' is not an ISO date (YYYY-MM-DD) or a period (YYYYMnn, YYYYQn)",
       call. = FALSE)
}

#' Translate one chart's spec into prep_chart() arguments
#'
#' Pure mapping from the two spec tables plus the table from
#' \link{build_chart_data} to a named list you can \code{do.call(prep_chart, ...)}
#' — or inspect first. Rules applied here, all mirroring \link{prep_chart}'s own:
#' \itemize{
#'   \item only \code{plot == TRUE} rows are plotted, in position order;
#'   \item \code{rolling}/\code{growth}/\code{index} are NULL (already applied
#'         by \link{build_chart_data});
#'   \item blank \code{y_axis} falls back to \link{default_y_axis};
#'   \item \code{dashed}/\code{dotted} logical columns become index vectors;
#'   \item the second \code{area} series gets \code{NA} legend;
#'   \item a literal \code{\\n} in \code{title}, \code{y_axis} or \code{note}
#'         becomes a line break;
#'   \item \code{legend_columns_<lang>} is used if present, else
#'         \code{legend_columns}, else 2;
#'   \item \code{colour} all-numeric strings become palette indices;
#'   \item \code{xmin}/\code{xmax}/\code{forecast_*} go through
#'         \link{parse_spec_date}: ISO or period form only.
#' }
#'
#' @param chart one row of the charts table
#' @param series_rows this chart's rows of the chart_series table (aliases filled)
#' @param wide the table from \link{build_chart_data}
#' @param language "si" or "en" — picks the \code{_sl}/\code{_en} columns
#' @return named list of \link{prep_chart} arguments
#' @export
#' @importFrom rlang .data
chart_args_from_spec <- function(chart, series_rows, wide, language = "si") {
  if (!language %in% c("si", "en")) stop("language must be 'si' or 'en'.")
  if (nrow(chart) != 1) stop("chart must be exactly one row.")
  suffix <- if (language == "en") "_en" else "_sl"
  lang_col <- function(df, stem) {
    col <- paste0(stem, suffix)
    if (col %in% names(df)) nz(df[[col]]) else NULL
  }
  # a literal \n typed in the CSV becomes a line break (CSV cells can't hold
  # one); stray leading/trailing ones are dropped so they can't add empty lines
  breaks <- function(x) {
    if (is.null(x)) return(NULL)
    gsub("^\n+|\n+$", "", gsub("\\\\n", "\n", x))
  }

  plotted <- series_rows |>
    dplyr::filter(as.logical(.data$plot) %in% TRUE) |>
    dplyr::arrange(.data$position)
  if (nrow(plotted) == 0) stop("chart ", chart$chart_id, " has no plotted series.")

  data <- dplyr::select(wide, "period_id", dplyr::all_of(plotted$alias))

  legend <- plotted[[paste0("legend", suffix)]]
  area_idx <- which(plotted$type == "area")
  if (length(area_idx) == 2) legend[area_idx[2]] <- NA

  colours <- col_or(plotted, "colour", NA_character_)
  if (all(is.na(colours))) {
    colours <- NULL
  } else if (all(grepl("^\\d+$", colours[!is.na(colours)]))) {
    colours <- as.integer(colours)
  }

  as_idx <- function(x) { i <- which(as.logical(x) %in% TRUE); if (length(i)) i else NULL }

  y_axis <- breaks(lang_col(chart, "y_axis"))
  if (is.null(y_axis)) {
    y_axis <- default_y_axis(col_or(plotted, "growth", NA_character_),
                             col_or(plotted, "index", NA_character_), language)
  }

  emphasis <- nz(chart$emphasis)
  if (!is.null(emphasis)) {
    emphasis <- if (toupper(as.character(emphasis)) == "FALSE") FALSE else as.numeric(emphasis)
  }

  ylim <- if (is.null(nz(chart$ylim_min)) || is.null(nz(chart$ylim_max))) NULL
  else as.numeric(c(chart$ylim_min, chart$ylim_max))

  forecast <- if (is.null(nz(chart$forecast_start)) || is.null(nz(chart$forecast_end))) NULL
  else c(parse_spec_date(chart$forecast_start, "forecast_start"),
         parse_spec_date(chart$forecast_end, "forecast_end"))

  # legend_columns_<lang>, else legend_columns, else prep_chart's default
  legend_columns <- lang_col(chart, "legend_columns")
  if (is.null(legend_columns) && "legend_columns" %in% names(chart)) legend_columns <- nz(chart$legend_columns)
  legend_columns <- if (is.null(legend_columns)) 2L else as.integer(legend_columns)

  list(
    data = data,
    type = plotted$type,
    title = breaks(lang_col(chart, "title")),
    y_axis = y_axis,
    legend = legend,
    colours = colours,
    dashed = as_idx(col_or(plotted, "dashed", FALSE)),
    dotted = as_idx(col_or(plotted, "dotted", FALSE)),
    xmin = parse_spec_date(chart$xmin, "xmin"),
    xmax = parse_spec_date(chart$xmax, "xmax"),
    stacked = isTRUE(as.logical(nz(chart$stacked))),
    emphasis = emphasis,
    legend_columns = legend_columns,
    rolling = NULL,
    growth = NULL,
    index = NULL,
    ylim = ylim,
    note = breaks(lang_col(chart, "note")),
    forecast = forecast,
    language = language
  )
}

#' Build a umar_chart from one chart's spec
#'
#' \link{chart_args_from_spec} followed by \link{prep_chart}.
#'
#' @inheritParams chart_args_from_spec
#' @return an object of class "umar_chart"
#' @export
prep_chart_from_spec <- function(chart, series_rows, wide, language = "si") {
  do.call(prep_chart, chart_args_from_spec(chart, series_rows, wide, language))
}


#' Read one spec CSV with the pipeline's conventions
#'
#' Delimiter is sniffed from the header line (\code{;} is Excel-in-a-
#' European-locale output; \code{,} decimals in numeric columns are handled
#' by \link{coerce_spec_types}). Every column is read as character (so
#' \code{readr} never guesses a type) and the file must be valid UTF-8 —
#' plain "CSV" from Excel on Windows is the system codepage and fails here
#' with instructions, rather than later inside a regex.
#'
#' @param path file path
#' @param encoding expected file encoding; keep UTF-8 unless you have a reason
#' @return tibble, all character columns
#' @keywords internal
read_spec_csv <- function(path, encoding = "UTF-8") {
  if (!file.exists(path)) stop("spec file not found: ", path, call. = FALSE)
  header <- readLines(path, n = 1, warn = FALSE, encoding = encoding)
  semicolon <- grepl(";", header, fixed = TRUE) && !grepl(",", header, fixed = TRUE)
  df <- readr::read_delim(path, delim = if (semicolon) ";" else ",",
                          col_types = readr::cols(.default = readr::col_character()),
                          locale = readr::locale(encoding = encoding),
                          na = c("", "NA"), trim_ws = TRUE,
                          show_col_types = FALSE, progress = FALSE)
  bad <- vapply(df, \(x) !all(validUTF8(x[!is.na(x)])), logical(1))
  if (any(bad)) {
    stop(basename(path), " is not valid ", encoding, " (columns: ",
         paste(names(df)[bad], collapse = ", "), "). ",
         "From Excel use Save As -> 'CSV UTF-8 (Comma delimited)', not plain CSV.",
         call. = FALSE)
  }
  names(df) <- trimws(names(df))
  df
}

#' Coerce known spec columns from character, leaving the rest alone
#' @keywords internal
coerce_spec_types <- function(df, logical_cols = character(), numeric_cols = character(),
                              integer_cols = character()) {
  to_lgl <- function(x) {
    x <- toupper(trimws(x))
    dplyr::case_when(x %in% c("TRUE", "T", "1") ~ TRUE,
                     x %in% c("FALSE", "F", "0") ~ FALSE,
                     TRUE ~ NA)
  }
  for (col in intersect(logical_cols, names(df))) df[[col]] <- to_lgl(df[[col]])
  # "," as decimal mark can only occur in a ;-delimited file, so swapping is safe
  to_num <- function(x) suppressWarnings(as.numeric(sub(",", ".", trimws(x), fixed = TRUE)))
  for (col in intersect(numeric_cols, names(df))) df[[col]] <- to_num(df[[col]])
  for (col in intersect(integer_cols, names(df))) df[[col]] <- as.integer(to_num(df[[col]]))
  df
}

#' Read, type, fill and validate the two chart spec CSVs
#'
#' The single entry point the driver should use. Reads both files via
#' \link{read_spec_csv} (UTF-8 enforced, delimiter sniffed, everything as
#' character), coerces the columns with known types (\code{plot},
#' \code{dashed}, \code{dotted}, \code{stacked} to logical; \code{position},
#' \code{rolling}, \code{ylim_*} to numeric; \code{legend_columns} to
#' integer, likewise \code{legend_columns_sl}/\code{_en} — dates stay
#' character for \link{parse_spec_date}), fills default
#' aliases and, unless \code{validate = FALSE}, runs \link{check_chart_specs}.
#'
#' @param charts_path path to charts.csv
#' @param series_path path to chart_series.csv
#' @param encoding file encoding, default UTF-8
#' @param validate run \link{check_chart_specs} (fail loud) before returning
#' @return list with \code{charts} and \code{chart_series} tibbles
#' @export
read_chart_specs <- function(charts_path, series_path, encoding = "UTF-8", validate = TRUE) {
  charts <- read_spec_csv(charts_path, encoding) |>
    coerce_spec_types(logical_cols = "stacked",
                      numeric_cols = c("ylim_min", "ylim_max"),
                      integer_cols = c("legend_columns", "legend_columns_sl", "legend_columns_en"))
  chart_series <- read_spec_csv(series_path, encoding) |>
    coerce_spec_types(logical_cols = c("plot", "dashed", "dotted"),
                      numeric_cols = c("position", "rolling")) |>
    fill_default_aliases()
  if (validate) check_chart_specs(charts, chart_series)
  list(charts = charts, chart_series = chart_series)
}
