# Stub fetcher: eight quarters, one column per names(codes), values 1:8 scaled
# by the column's index so each series is distinguishable.
periods <- c("2022Q1", "2022Q2", "2022Q3", "2022Q4", "2023Q1", "2023Q2", "2023Q3", "2023Q4")
stub_fetch <- function(codes, con, date_valid = NULL, schema = "platform") {
  cols <- purrr::imap(codes, \(code, nm) (1:8) * match(nm, names(codes)))
  tibble::tibble(period_id = periods, !!!cols)
}

# Expected value of a transform, computed the way build_chart_data does it —
# through the package's own internals — so the tests check wiring, not maths.
direct <- function(value, f) {
  f(parse_wide(convert_period_column(data.frame(period_id = periods, value = value)))$datapoints[[1]])$value
}

gdp_rows <- function() {
  tibble::tibble(
    chart_id    = "gdp",
    position    = c(1,        2,        3,           4,          5),
    alias       = c("p52",    "p53",    "inv",       "p7",       "imp"),
    source_type = c("db",     "db",     "computed",  "db",       "computed"),
    series_code = c("S--P52", "S--P53", NA,          "S--P7",    NA),
    formula     = c(NA,       NA,       "p52 + p53", NA,         "-p7"),
    plot        = c(FALSE,    FALSE,    TRUE,        FALSE,      TRUE),
    growth      = NA_character_,
    index       = NA_character_,
    rolling     = NA_real_
  )
}

test_that("db rows are fetched under their alias and computed rows evaluated", {
  wide <- build_chart_data(gdp_rows(), con = NULL, fetch_fn = stub_fetch)

  testthat::expect_named(wide, c("period_id", "p52", "p53", "inv", "p7", "imp"))
  testthat::expect_equal(wide$inv, wide$p52 + wide$p53)
  testthat::expect_equal(wide$imp, -wide$p7)
})

test_that("blank alias on a db row becomes the derived alias", {
  rows <- gdp_rows()
  rows$alias[rows$alias == "p7"] <- NA
  rows$formula[rows$alias %in% "imp"] <- "-s_p7"
  wide <- build_chart_data(rows, con = NULL, fetch_fn = stub_fetch)

  testthat::expect_named(wide, c("period_id", "p52", "p53", "inv", "s_p7", "imp"))
  testthat::expect_equal(wide$imp, -wide$s_p7)
})

test_that("columns come back in position order regardless of input row order", {
  rows <- gdp_rows()[c(5, 3, 1, 4, 2), ]
  wide <- build_chart_data(rows, con = NULL, fetch_fn = stub_fetch)

  testthat::expect_named(wide, c("period_id", "p52", "p53", "inv", "p7", "imp"))
})

test_that("computed rows may reference earlier computed rows and later db rows", {
  rows <- dplyr::bind_rows(gdp_rows(), tibble::tibble(
    chart_id = "gdp", position = 6, alias = "share", source_type = "computed",
    series_code = NA, formula = "inv / gdp", plot = TRUE
  ), tibble::tibble(
    chart_id = "gdp", position = 7, alias = "gdp", source_type = "db",
    series_code = "S--B1GQ", formula = NA, plot = TRUE
  ))
  wide <- build_chart_data(rows, con = NULL, fetch_fn = stub_fetch)

  testthat::expect_equal(wide$share, wide$inv / wide$gdp)
})

test_that("a constant formula is recycled to the table length", {
  rows <- dplyr::bind_rows(gdp_rows(), tibble::tibble(
    chart_id = "gdp", position = 6, alias = "zero", source_type = "computed",
    series_code = NA, formula = "0", plot = TRUE
  ))
  wide <- build_chart_data(rows, con = NULL, fetch_fn = stub_fetch)

  testthat::expect_equal(wide$zero, rep(0, 8))
})

# --- transforms ---------------------------------------------------------------

test_that("a db row's rolling transform is applied via the package's own transform_rolling", {
  rows <- gdp_rows()
  rows$rolling[rows$alias == "p52"] <- 3
  wide <- build_chart_data(rows, con = NULL, fetch_fn = stub_fetch)

  raw <- stub_fetch(c(p52 = "S--P52"), NULL)$p52
  testthat::expect_equal(wide$p52, direct(raw, \(df) transform_rolling(df, periods = 3, align = "r")))
})

test_that("formulas see transformed values, not raw ones", {
  rows <- gdp_rows()
  rows$growth[rows$alias == "p52"] <- "YOY"
  rows$growth[rows$alias == "p53"] <- "YOY"
  wide <- build_chart_data(rows, con = NULL, fetch_fn = stub_fetch)

  # inv = p52 + p53 where both are already YoY growth rates
  testthat::expect_equal(wide$inv, wide$p52 + wide$p53)
  testthat::expect_false(isTRUE(all.equal(wide$p52, stub_fetch(c(p52 = "S--P52"), NULL)$p52)))
})

test_that("a computed row's own transform is applied after its formula", {
  rows <- gdp_rows()
  rows$growth[rows$alias == "inv"] <- "YOY"
  wide <- build_chart_data(rows, con = NULL, fetch_fn = stub_fetch)

  level <- wide$p52 + wide$p53
  testthat::expect_equal(wide$inv, direct(level, \(df) transform_growth(df, type = "YOY")))
})

test_that("transforms run in prep_chart's order: rolling, then growth", {
  rows <- gdp_rows()
  rows$rolling[rows$alias == "p52"] <- 2
  rows$growth[rows$alias == "p52"] <- "QOQ"
  wide <- build_chart_data(rows, con = NULL, fetch_fn = stub_fetch)

  raw <- stub_fetch(c(p52 = "S--P52"), NULL)$p52
  expected <-     direct(raw, \(df) transform_growth(df, type = "QOQ")) |>
    direct(\(df) transform_rolling(df, periods = 2, align = "r"))

  testthat::expect_equal(wide$p52, expected)
})

test_that("index transform is applied", {
  rows <- gdp_rows()
  rows$index[rows$alias == "p52"] <- "2022"
  wide <- build_chart_data(rows, con = NULL, fetch_fn = stub_fetch)

  raw <- stub_fetch(c(p52 = "S--P52"), NULL)$p52
  testthat::expect_equal(wide$p52, transform_index(
    parse_wide(convert_period_column(data.frame(period_id = periods, value = raw)))$datapoints[[1]],
    base_period = "2022")$df$value)
})

test_that("same series raw and smoothed, both blank aliases, become two distinct columns", {
  rows <- tibble::tibble(
    chart_id = "x", position = 1:2, alias = NA_character_, source_type = "db",
    series_code = "S--X", formula = NA, plot = TRUE,
    growth = NA_character_, index = NA_character_, rolling = c(NA, 3)
  )
  wide <- build_chart_data(rows, con = NULL, fetch_fn = stub_fetch)

  testthat::expect_named(wide, c("period_id", "s_x", "s_x_roll3"))
  testthat::expect_false(isTRUE(all.equal(wide$s_x, wide$s_x_roll3)))
})

test_that("default_y_axis mirrors prep_chart's rule", {
  testthat::expect_equal(default_y_axis(c("YOY", "YOY"), c(NA, NA)), "%")
  testthat::expect_null(default_y_axis(c("YOY", NA), c(NA, NA)))
  testthat::expect_equal(default_y_axis(c(NA, NA), c("2015", "2015")), "Indeks (2015 = 100)")
  testthat::expect_equal(default_y_axis(c(NA, NA), c("2015", "2015"), "en"), "Index (2015 = 100)")
  testthat::expect_null(default_y_axis(c(NA, NA), c("2015", "2020")))
  testthat::expect_null(default_y_axis(c(NA, NA), c(NA, NA)))
})

# --- failure modes -------------------------------------------------------------

test_that("plotted subset is a plain select on the result", {
  rows <- gdp_rows()
  wide <- build_chart_data(rows, con = NULL, fetch_fn = stub_fetch)
  plotted <- rows$alias[rows$plot]

  testthat::expect_named(dplyr::select(wide, "period_id", dplyr::all_of(plotted)),
                         c("period_id", "inv", "imp"))
})

test_that("a formula that fails at evaluation names the chart, alias and formula", {
  rows <- gdp_rows()
  rows$formula[rows$alias == "inv"] <- "p52 + nope"

  testthat::expect_error(build_chart_data(rows, con = NULL, fetch_fn = stub_fetch),
                         regexp = "chart gdp, series 'inv' \\(p52 \\+ nope\\)")
})

test_that("a transform that fails names the chart, alias and code", {
  rows <- gdp_rows()
  rows$growth[rows$alias == "p52"] <- "MOM"   # monthly growth on quarterly data

  testthat::expect_error(build_chart_data(rows, con = NULL, fetch_fn = stub_fetch),
                         regexp = "chart gdp, series 'p52' \\(S--P52\\)")
})

test_that("rows from more than one chart_id stop", {
  rows <- gdp_rows()
  rows$chart_id[1] <- "other"

  testthat::expect_error(build_chart_data(rows, con = NULL, fetch_fn = stub_fetch),
                         regexp = "exactly one chart_id")
})

test_that("adhoc rows stop until implemented", {
  rows <- gdp_rows()
  rows$source_type[1] <- "adhoc"

  testthat::expect_error(build_chart_data(rows, con = NULL, fetch_fn = stub_fetch),
                         regexp = "adhoc")
})

test_that("a chart with no db rows stops", {
  rows <- gdp_rows()[3, ]

  testthat::expect_error(build_chart_data(rows, con = NULL, fetch_fn = stub_fetch),
                         regexp = "no 'db' series rows")
})

test_that("fetch_fn receives codes named by alias", {
  seen <- NULL
  spy <- function(codes, con, date_valid = NULL, schema = "platform") {
    seen <<- codes
    stub_fetch(codes, con, date_valid, schema)
  }
  build_chart_data(gdp_rows(), con = NULL, fetch_fn = spy)

  testthat::expect_equal(seen, c(p52 = "S--P52", p53 = "S--P53", p7 = "S--P7"))
})

test_that("a fetch_fn that misnames columns fails loudly, listing what it returned", {
  bad_fetch <- function(codes, con, date_valid = NULL, schema = "platform") {
    out <- stub_fetch(codes, con, date_valid, schema)
    names(out)[-1] <- paste0("nm.", seq_along(codes))
    out
  }
  testthat::expect_error(build_chart_data(gdp_rows(), con = NULL, fetch_fn = bad_fetch),
                         regexp = "no column 'p52' \\(columns: period_id, nm.1, nm.2, nm.3\\)")
})
