# A clean two-chart spec exercising the rules that should pass:
# - a plot = FALSE db row with no legend/type
# - a computed row referencing a db alias at a LATER position
# - a computed row referencing an earlier computed alias
clean_spec <- function() {
  charts <- tibble::tibble(
    chart_id = c("gdp", "wages"),
    title_sl = c("BDP", "Place"),
    title_en = c("GDP", "Wages")
  )
  chart_series <- tibble::tibble(
    chart_id    = c("gdp",    "gdp",   "gdp",       "gdp",        "wages"),
    position    = c(1,        2,       3,           4,            1),
    alias       = c("p52",    "p53",   "inv",       "inv_neg",    "w"),
    source_type = c("db",     "db",    "computed",  "computed",   "db"),
    series_code = c("S--P52", "S--P53", NA,         NA,           "S--W"),
    formula     = c(NA,       NA,      "p52 + p53", "-inv + gdp", NA),
    plot        = c(FALSE,    FALSE,   TRUE,        TRUE,         TRUE),
    legend_sl   = c(NA,       NA,      "Zaloge",    "Neg",        "Place"),
    legend_en   = c(NA,       NA,      "Inventories", "Neg",      "Wages"),
    type        = c(NA,       NA,      "bar",       "bar",        "line"),
    growth      = NA_character_,
    index       = NA_character_,
    rolling     = NA_real_
  )
  # inv_neg references gdp, a db alias at position 5 (later) — must be allowed
  chart_series <- dplyr::bind_rows(chart_series, tibble::tibble(
    chart_id = "gdp", position = 5, alias = "gdp", source_type = "db",
    series_code = "S--B1GQ", formula = NA, plot = TRUE,
    legend_sl = "BDP", legend_en = "GDP", type = "line",
    growth = NA_character_, index = NA_character_, rolling = NA_real_
  ))
  list(charts = charts, chart_series = chart_series)
}

expect_only_problem <- function(spec, check) {
  problems <- validate_chart_specs(spec$charts, spec$chart_series)
  testthat::expect_equal(problems$check, check)
}

test_that("clean spec yields zero problems", {
  spec <- clean_spec()
  testthat::expect_equal(nrow(validate_chart_specs(spec$charts, spec$chart_series)), 0)
})

test_that("missing required column stops", {
  spec <- clean_spec()
  spec$chart_series$alias <- NULL
  testthat::expect_error(validate_chart_specs(spec$charts, spec$chart_series),
                         regexp = "chart_series: alias")
})

test_that("duplicate chart_id in charts", {
  spec <- clean_spec()
  spec$charts <- dplyr::bind_rows(spec$charts, spec$charts[1, ])
  expect_only_problem(spec, "duplicate chart_id in charts")
})

test_that("titles are optional, but must be paired", {
  spec <- clean_spec()
  spec$charts$title_sl[2] <- NA
  spec$charts$title_en[2] <- ""
  testthat::expect_equal(nrow(validate_chart_specs(spec$charts, spec$chart_series)), 0)

  spec <- clean_spec()
  spec$charts$title_en[2] <- ""
  expect_only_problem(spec, "title given in one language only")
})

test_that("default_alias is a stable, valid identifier derived from the code", {
  testthat::expect_equal(default_alias("SURS--0300230S--P52--GO4--N--Q"),
                         "surs_0300230s_p52_go4_n_q")
  testthat::expect_equal(default_alias("0300230S--P52"), "x_0300230s_p52")
  testthat::expect_equal(default_alias("--A--"), "a")
  testthat::expect_equal(default_alias("S--X", rolling = 3), "s_x_roll3")
  testthat::expect_equal(default_alias("S--X", growth = "YOY"), "s_x_yoy")
  testthat::expect_equal(default_alias("S--X", index = "2023Q1"), "s_x_idx2023q1")
  testthat::expect_equal(default_alias("S--X", growth = "QOQ", rolling = 2), "s_x_roll2_qoq")
  testthat::expect_equal(default_alias(c("S--X", "S--Y"), rolling = c(3, NA)), c("s_x_roll3", "s_y"))
})

test_that("default_alias spells out operators for formulas", {
  f <- function(x, ...) default_alias(x, ..., formula = TRUE)
  testthat::expect_equal(f("a + b"), "a_plus_b")
  testthat::expect_equal(f("-p7"), "neg_p7")
  testthat::expect_equal(f("inv / gdp"), "inv_div_gdp")
  testthat::expect_equal(f("a * 2"), "a_times_2")
  testthat::expect_equal(f("(a + b) / c"), "a_plus_b_div_c")
  testthat::expect_equal(f("a - b"), "a_minus_b")
  testthat::expect_equal(f("a^2"), "a_pow_2")
  testthat::expect_equal(f("0"), "x_0")
  testthat::expect_equal(f("a + b", growth = "YOY"), "a_plus_b_yoy")
  # a formula and a plain series don't collide by accident
  testthat::expect_false(f("-p7") == default_alias("p7"))
  # codes are NOT operator-spelled: '--' is a separator, not minus
  testthat::expect_equal(default_alias("S--X"), "s_x")
})

test_that("blank alias on a computed row is derived from its formula", {
  spec <- clean_spec()
  spec$chart_series$alias[spec$chart_series$alias == "inv_neg"] <- NA
  testthat::expect_equal(nrow(validate_chart_specs(spec$charts, spec$chart_series)), 0)

  filled <- fill_default_aliases(spec$chart_series)
  testthat::expect_equal(filled$alias[filled$formula %in% "-inv + gdp"], "neg_inv_plus_gdp")
})

test_that("a later computed row can reference a derived computed alias", {
  spec <- clean_spec()
  spec$chart_series$alias[spec$chart_series$alias == "inv"] <- NA          # becomes p52_plus_p53
  spec$chart_series$formula[spec$chart_series$alias %in% "inv_neg"] <- "-p52_plus_p53 + gdp"
  testthat::expect_equal(nrow(validate_chart_specs(spec$charts, spec$chart_series)), 0)
})

test_that("blank alias on a db row is derived from the code and is referenceable", {
  spec <- clean_spec()
  spec$chart_series$alias[spec$chart_series$alias == "p52"] <- NA
  spec$chart_series$formula[spec$chart_series$alias %in% "inv"] <- "s_p52 + p53"
  testthat::expect_equal(nrow(validate_chart_specs(spec$charts, spec$chart_series)), 0)

  filled <- fill_default_aliases(spec$chart_series)
  testthat::expect_equal(filled$alias[filled$series_code %in% "S--P52"], "s_p52")
})

test_that("blank alias on a computed row with no formula is still an error", {
  spec <- clean_spec()
  spec$chart_series$alias[spec$chart_series$alias == "inv_neg"] <- ""
  spec$chart_series$formula[spec$chart_series$formula %in% "-inv + gdp"] <- NA
  problems <- validate_chart_specs(spec$charts, spec$chart_series)
  testthat::expect_setequal(problems$check,
                            c("missing or invalid alias", "source_type 'computed' with no formula"))
})

test_that("same code twice with blank aliases collides only if the transforms match", {
  spec <- clean_spec()
  spec$chart_series$series_code[spec$chart_series$alias == "p53"] <- "S--P52"
  spec$chart_series$alias[spec$chart_series$alias %in% c("p52", "p53")] <- NA
  spec$chart_series$formula[spec$chart_series$alias %in% "inv"] <- "s_p52"
  problems <- validate_chart_specs(spec$charts, spec$chart_series)
  testthat::expect_true("duplicate alias within chart_id" %in% problems$check)

  # raw + smoothed: distinct derived aliases, no collision
  spec$chart_series$rolling[spec$chart_series$series_code %in% "S--P52" & spec$chart_series$position == 2] <- 3
  spec$chart_series$formula[spec$chart_series$alias %in% "inv"] <- "s_p52_roll3 - s_p52"
  testthat::expect_equal(nrow(validate_chart_specs(spec$charts, spec$chart_series)), 0)
})

test_that("chart header with no series rows", {
  spec <- clean_spec()
  spec$charts <- dplyr::bind_rows(spec$charts,
                                  tibble::tibble(chart_id = "empty", title_sl = "x", title_en = "x"))
  expect_only_problem(spec, "chart has no series rows")
})

test_that("series row with no chart header", {
  spec <- clean_spec()
  spec$chart_series$chart_id[spec$chart_series$alias == "w"] <- "ghost"
  problems <- validate_chart_specs(spec$charts, spec$chart_series)
  testthat::expect_setequal(problems$check,
                            c("chart_series row has no matching chart header", "chart has no series rows"))
})

test_that("duplicate position within a chart", {
  spec <- clean_spec()
  spec$chart_series$position[spec$chart_series$alias == "p53"] <- 1
  expect_only_problem(spec, "duplicate position within chart_id")
})

test_that("duplicate alias within a chart", {
  spec <- clean_spec()
  spec$chart_series$alias[spec$chart_series$alias == "p53"] <- "p52"
  problems <- validate_chart_specs(spec$charts, spec$chart_series)
  testthat::expect_true("duplicate alias within chart_id" %in% problems$check)
})

test_that("same alias in different charts is fine", {
  spec <- clean_spec()
  spec$chart_series$alias[spec$chart_series$alias == "w"] <- "gdp"
  testthat::expect_equal(nrow(validate_chart_specs(spec$charts, spec$chart_series)), 0)
})

test_that("invalid alias syntax and reserved name", {
  spec <- clean_spec()
  spec$chart_series$alias[spec$chart_series$alias == "w"] <- "Wages-1"
  expect_only_problem(spec, "missing or invalid alias")

  spec <- clean_spec()
  spec$chart_series$alias[spec$chart_series$alias == "w"] <- "period_id"
  expect_only_problem(spec, "missing or invalid alias")
})

test_that("invalid source_type", {
  spec <- clean_spec()
  spec$chart_series$source_type[spec$chart_series$alias == "w"] <- "excel"
  expect_only_problem(spec, "invalid source_type")
})

test_that("db row without series_code", {
  spec <- clean_spec()
  spec$chart_series$series_code[spec$chart_series$alias == "w"] <- ""
  expect_only_problem(spec, "source_type 'db' with no series_code")
})

test_that("computed row without formula", {
  spec <- clean_spec()
  spec$chart_series$formula[spec$chart_series$alias == "inv"] <- NA
  expect_only_problem(spec, "source_type 'computed' with no formula")
})

test_that("formula that does not parse", {
  spec <- clean_spec()
  spec$chart_series$formula[spec$chart_series$alias == "inv"] <- "p52 +"
  expect_only_problem(spec, "formula does not parse")
})

test_that("formula referencing an unknown alias", {
  spec <- clean_spec()
  spec$chart_series$formula[spec$chart_series$alias == "inv"] <- "p52 + p54"
  problems <- validate_chart_specs(spec$charts, spec$chart_series)
  testthat::expect_equal(problems$check, "formula references unknown alias")
  testthat::expect_match(problems$detail, "p54")
})

test_that("formula referencing a LATER computed alias is rejected", {
  spec <- clean_spec()
  spec$chart_series$formula[spec$chart_series$alias == "inv"] <- "inv_neg * 2"
  problems <- validate_chart_specs(spec$charts, spec$chart_series)
  testthat::expect_equal(problems$check, "formula references unknown alias")
  testthat::expect_match(problems$detail, "inv_neg")
})

test_that("formula referencing an alias from another chart is rejected", {
  spec <- clean_spec()
  spec$chart_series$formula[spec$chart_series$alias == "inv"] <- "p52 + w"
  expect_only_problem(spec, "formula references unknown alias")
})

test_that("constant formula is fine", {
  spec <- clean_spec()
  spec$chart_series$formula[spec$chart_series$alias == "inv"] <- "0"
  testthat::expect_equal(nrow(validate_chart_specs(spec$charts, spec$chart_series)), 0)
})

test_that("plot must be TRUE or FALSE", {
  spec <- clean_spec()
  spec$chart_series$plot[spec$chart_series$alias == "w"] <- NA
  expect_only_problem(spec, "plot must be TRUE or FALSE")
})

test_that("plotted row without legend", {
  spec <- clean_spec()
  spec$chart_series$legend_en[spec$chart_series$alias == "w"] <- ""
  expect_only_problem(spec, "missing legend_sl/legend_en on plotted series")
})

test_that("plotted row with missing or invalid type", {
  spec <- clean_spec()
  spec$chart_series$type[spec$chart_series$alias == "w"] <- NA
  expect_only_problem(spec, "missing or invalid type on plotted series")

  spec <- clean_spec()
  spec$chart_series$type[spec$chart_series$alias == "w"] <- "scatter"
  expect_only_problem(spec, "missing or invalid type on plotted series")
})

test_that("y_axis/note/ylim/forecast pairing is checked only when the columns exist", {
  spec <- clean_spec()
  spec$charts$ylim_min <- c(90, NA); spec$charts$ylim_max <- c(NA, NA)
  expect_only_problem(spec, "ylim given on one side only")

  spec <- clean_spec()
  spec$charts$note_sl <- c("x", NA); spec$charts$note_en <- c("x", "y")
  expect_only_problem(spec, "note given in one language only")
})

test_that("chart dates must be ISO or period form", {
  spec <- clean_spec()
  spec$charts$xmin <- c("01/01/2022", NA)
  expect_only_problem(spec, "invalid date")

  spec$charts$xmin <- c("2022M01", "2022-01-01")
  spec$charts$forecast_start <- c("2025Q1", NA); spec$charts$forecast_end <- c("2026Q4", NA)
  testthat::expect_equal(nrow(validate_chart_specs(spec$charts, spec$chart_series)), 0)
})

test_that("second area series may have a blank legend", {
  spec <- clean_spec()
  gdp_plotted <- spec$chart_series$chart_id == "gdp" & spec$chart_series$plot
  spec$chart_series$type[gdp_plotted] <- c("area", "area", "line")
  second_area <- which(gdp_plotted)[2]
  spec$chart_series$legend_sl[second_area] <- NA
  spec$chart_series$legend_en[second_area] <- NA
  testthat::expect_equal(nrow(validate_chart_specs(spec$charts, spec$chart_series)), 0)

  # but not the first
  spec$chart_series$legend_sl[which(gdp_plotted)[1]] <- NA
  expect_only_problem(spec, "missing legend_sl/legend_en on plotted series")
})

test_that("invalid rolling", {
  spec <- clean_spec()
  spec$chart_series$rolling[spec$chart_series$alias == "w"] <- 1
  expect_only_problem(spec, "invalid rolling")
  spec$chart_series$rolling[spec$chart_series$alias == "w"] <- 2.5
  expect_only_problem(spec, "invalid rolling")
})

test_that("invalid growth", {
  spec <- clean_spec()
  spec$chart_series$growth[spec$chart_series$alias == "w"] <- "yoy"
  expect_only_problem(spec, "invalid growth")
})

test_that("invalid index", {
  spec <- clean_spec()
  spec$chart_series$index[spec$chart_series$alias == "w"] <- "2023-01"
  expect_only_problem(spec, "invalid index")
})

test_that("growth and index on the same series", {
  spec <- clean_spec()
  spec$chart_series$growth[spec$chart_series$alias == "w"] <- "YOY"
  spec$chart_series$index[spec$chart_series$alias == "w"] <- "2015"
  expect_only_problem(spec, "growth and index on the same series")
})

test_that("valid transforms pass", {
  spec <- clean_spec()
  spec$chart_series$growth[spec$chart_series$alias == "w"] <- "YOY"
  spec$chart_series$rolling[spec$chart_series$alias == "w"] <- 3
  spec$chart_series$index[spec$chart_series$alias == "gdp"] <- "2023Q1"
  testthat::expect_equal(nrow(validate_chart_specs(spec$charts, spec$chart_series)), 0)
})

test_that("more than 8 plotted series", {
  spec <- clean_spec()
  extra <- tibble::tibble(
    chart_id = "wages", position = 2:9, alias = paste0("w", 2:9), source_type = "db",
    series_code = paste0("S--W", 2:9), formula = NA, plot = TRUE,
    legend_sl = "x", legend_en = "x", type = "line",
    growth = NA_character_, index = NA_character_, rolling = NA_real_
  )
  spec$chart_series <- dplyr::bind_rows(spec$chart_series, extra)
  expect_only_problem(spec, "more than 8 plotted series")
})

test_that("more than 2 area series", {
  spec <- clean_spec()
  spec$chart_series$type[spec$chart_series$chart_id == "gdp" & spec$chart_series$plot] <- "area"
  expect_only_problem(spec, "more than 2 area series")
})

test_that("check_chart_specs stops loudly listing every problem", {
  spec <- clean_spec()
  spec$charts$title_en[1] <- ""
  spec$chart_series$type[spec$chart_series$alias == "w"] <- "scatter"
  testthat::expect_error(check_chart_specs(spec$charts, spec$chart_series),
                         regexp = "one language only.*\n.*invalid type")
})

spec_chart <- function(...) {
  base <- tibble::tibble(
    chart_id = "gdp", publication = "GT",
    title_sl = "BDP", title_en = "GDP",
    y_axis_sl = NA, y_axis_en = NA,
    note_sl = "Vir: SURS\\nOpomba", note_en = "Source: SURS",
    stacked = TRUE, xmin = "2020-01-01", xmax = NA,
    ylim_min = NA, ylim_max = NA, emphasis = NA,
    forecast_start = NA, forecast_end = NA, legend_columns = NA
  )
  overrides <- list(...)
  for (nm in names(overrides)) base[[nm]] <- overrides[[nm]]
  base
}

spec_rows <- function() {
  tibble::tibble(
    chart_id    = "gdp",
    position    = c(1,     2,     3,       4),
    alias       = c("p52", "inv", "gdp",   "imp"),
    source_type = c("db",  "computed", "db", "computed"),
    series_code = c("S--P52", NA, "S--B1GQ", NA),
    formula     = c(NA, "p52 * 2", NA, "-gdp"),
    plot        = c(FALSE, TRUE, TRUE, TRUE),
    legend_sl   = c(NA, "Zaloge", "BDP", "Uvoz"),
    legend_en   = c(NA, "Inventories", "GDP", "Imports"),
    type        = c(NA, "bar", "line", "bar"),
    growth      = c(NA, NA, "YOY", NA),
    index       = NA_character_,
    rolling     = NA_real_,
    dashed      = c(FALSE, FALSE, TRUE, FALSE),
    dotted      = FALSE,
    colour      = NA_character_
  )
}

spec_wide <- function() {
  tibble::tibble(period_id = c("2023Q1", "2023Q2"), p52 = c(1, 2), inv = c(2, 4),
                 gdp = c(10, 20), imp = c(-10, -20))
}

test_that("plotted series in position order, language columns picked, transforms nulled", {
  args <- chart_args_from_spec(spec_chart(), spec_rows(), spec_wide(), "si")

  testthat::expect_named(args$data, c("period_id", "inv", "gdp", "imp"))
  testthat::expect_equal(args$type, c("bar", "line", "bar"))
  testthat::expect_equal(args$legend, c("Zaloge", "BDP", "Uvoz"))
  testthat::expect_equal(args$title, "BDP")
  testthat::expect_null(args$rolling); testthat::expect_null(args$growth); testthat::expect_null(args$index)
  testthat::expect_equal(args$language, "si")

  en <- chart_args_from_spec(spec_chart(), spec_rows(), spec_wide(), "en")
  testthat::expect_equal(en$legend, c("Inventories", "GDP", "Imports"))
  testthat::expect_equal(en$title, "GDP")
})

test_that("dashed/dotted logicals become index vectors over plotted rows", {
  args <- chart_args_from_spec(spec_chart(), spec_rows(), spec_wide())

  testthat::expect_equal(args$dashed, 2L)   # gdp is 2nd plotted row
  testthat::expect_null(args$dotted)
})

test_that("chart-level scalars map through, blanks become NULL", {
  args <- chart_args_from_spec(spec_chart(), spec_rows(), spec_wide())

  testthat::expect_true(args$stacked)
  testthat::expect_equal(args$xmin, as.Date("2020-01-01"))
  testthat::expect_null(args$xmax)
  testthat::expect_null(args$ylim)
  testthat::expect_null(args$forecast)
  testthat::expect_null(args$emphasis)
  testthat::expect_equal(args$legend_columns, 2)
})

test_that("paired chart fields only pass when both sides are given", {
  args <- chart_args_from_spec(spec_chart(ylim_min = 90, ylim_max = 120,
                                          forecast_start = "2025-01-01", forecast_end = "2026-12-31"),
                               spec_rows(), spec_wide())
  testthat::expect_equal(args$ylim, c(90, 120))
  testthat::expect_equal(args$forecast, as.Date(c("2025-01-01", "2026-12-31")))
})

test_that("spec dates accept ISO or period form, pass Date through, refuse locale forms", {
  testthat::expect_equal(parse_spec_date("2022-03-15"), as.Date("2022-03-15"))
  testthat::expect_equal(parse_spec_date("2022M03"), as.Date("2022-03-01"))
  testthat::expect_equal(parse_spec_date("2022Q3"), as.Date("2022-07-01"))
  testthat::expect_equal(parse_spec_date(as.Date("2022-03-15")), as.Date("2022-03-15"))
  testthat::expect_null(parse_spec_date(NA))
  testthat::expect_null(parse_spec_date(""))
  testthat::expect_error(parse_spec_date("01/01/2022", "xmin"), "xmin = '01/01/2022'")
  testthat::expect_error(parse_spec_date("1.1.2022"), "not an ISO date")

  args <- chart_args_from_spec(spec_chart(xmin = "2022Q1", forecast_start = "2025M01", forecast_end = "2026M12"),
                               spec_rows(), spec_wide())
  testthat::expect_equal(args$xmin, as.Date("2022-01-01"))
  testthat::expect_equal(args$forecast, as.Date(c("2025-01-01", "2026-12-01")))
  testthat::expect_error(chart_args_from_spec(spec_chart(xmin = "01/01/2022"), spec_rows(), spec_wide()), "xmin")
})

test_that("emphasis: FALSE string disables, numeric string parses", {
  testthat::expect_false(chart_args_from_spec(spec_chart(emphasis = "FALSE"), spec_rows(), spec_wide())$emphasis)
  testthat::expect_equal(chart_args_from_spec(spec_chart(emphasis = "100"), spec_rows(), spec_wide())$emphasis, 100)
})

test_that("literal backslash-n becomes a newline in note, y_axis and title", {
  args <- chart_args_from_spec(spec_chart(y_axis_sl = "Prispevki k rasti,\\nv o. t.",
                                          title_sl = "BDP\\nrealno"),
                               spec_rows(), spec_wide(), "si")
  testthat::expect_equal(args$note, "Vir: SURS\nOpomba")
  testthat::expect_equal(args$y_axis, "Prispevki k rasti,\nv o. t.")
  testthat::expect_equal(args$title, "BDP\nrealno")

  # a stray trailing/leading \n in the cell must not produce an empty line
  args <- chart_args_from_spec(spec_chart(y_axis_sl = "v o. t.\\n", title_sl = "\\nBDP"),
                               spec_rows(), spec_wide(), "si")
  testthat::expect_equal(args$y_axis, "v o. t.")
  testthat::expect_equal(args$title, "BDP")
})

test_that("legend_columns: per-language column wins, then plain, then 2", {
  testthat::expect_equal(chart_args_from_spec(spec_chart(), spec_rows(), spec_wide())$legend_columns, 2L)
  testthat::expect_equal(chart_args_from_spec(spec_chart(legend_columns = 3), spec_rows(), spec_wide())$legend_columns, 3L)

  ch <- spec_chart(legend_columns = 3, legend_columns_sl = 1, legend_columns_en = 2)
  testthat::expect_equal(chart_args_from_spec(ch, spec_rows(), spec_wide(), "si")$legend_columns, 1L)
  testthat::expect_equal(chart_args_from_spec(ch, spec_rows(), spec_wide(), "en")$legend_columns, 2L)

  ch <- spec_chart(legend_columns = 3, legend_columns_sl = NA, legend_columns_en = 2)
  testthat::expect_equal(chart_args_from_spec(ch, spec_rows(), spec_wide(), "si")$legend_columns, 3L)
})

test_that("blank y_axis falls back to default_y_axis over plotted rows", {
  rows <- spec_rows()
  rows$growth[rows$plot] <- "YOY"
  testthat::expect_equal(chart_args_from_spec(spec_chart(), rows, spec_wide())$y_axis, "%")

  # explicit wins
  testthat::expect_equal(chart_args_from_spec(spec_chart(y_axis_sl = "mio EUR"), rows, spec_wide())$y_axis, "mio EUR")

  # not all plotted rows have growth -> NULL
  testthat::expect_null(chart_args_from_spec(spec_chart(), spec_rows(), spec_wide())$y_axis)
})

test_that("second area series gets NA legend", {
  rows <- spec_rows()
  rows$type[rows$plot] <- c("area", "area", "line")
  args <- chart_args_from_spec(spec_chart(), rows, spec_wide())
  testthat::expect_equal(args$legend, c("Zaloge", NA, "Uvoz"))
})

test_that("colour: all-numeric strings become palette indices, hex stays character, all blank is NULL", {
  rows <- spec_rows()
  testthat::expect_null(chart_args_from_spec(spec_chart(), rows, spec_wide())$colours)

  rows$colour[rows$plot] <- c("1", "3", NA)
  testthat::expect_equal(chart_args_from_spec(spec_chart(), rows, spec_wide())$colours, c(1L, 3L, NA))

  rows$colour[rows$plot] <- c("#ff0000", NA, "#00ff00")
  testthat::expect_equal(chart_args_from_spec(spec_chart(), rows, spec_wide())$colours, c("#ff0000", NA, "#00ff00"))
})

test_that("missing optional series columns are tolerated", {
  rows <- spec_rows()
  rows$dashed <- NULL; rows$dotted <- NULL; rows$colour <- NULL
  args <- chart_args_from_spec(spec_chart(), rows, spec_wide())
  testthat::expect_null(args$dashed); testthat::expect_null(args$dotted); testthat::expect_null(args$colours)
})

test_that("no plotted rows or bad language stop", {
  rows <- spec_rows(); rows$plot <- FALSE
  testthat::expect_error(chart_args_from_spec(spec_chart(), rows, spec_wide()), "no plotted series")
  testthat::expect_error(chart_args_from_spec(spec_chart(), spec_rows(), spec_wide(), "sl"), "language")
})


charts_lines <- c(
  "chart_id,publication,title_sl,title_en,stacked,xmin,ylim_min,ylim_max,legend_columns",
  "gdp,GT,Bruto domači proizvod,GDP,TRUE,2022M01,90.5,120,1"
)
series_lines <- c(
  "chart_id,position,alias,source_type,series_code,formula,plot,legend_sl,legend_en,type,growth,index,rolling,dashed,dotted",
  "gdp,1,,db,S--B1GQ,,TRUE,BDP širše,GDP,line,YOY,,,1,0",
  "gdp,2,x,computed,,s_b1gq_yoy * 2,FALSE,,,,,,3,,"
)

write_utf8 <- function(lines, path) writeLines(enc2utf8(lines), path, useBytes = TRUE)

test_that("read_chart_specs reads UTF-8 comma CSVs, types columns, fills aliases, validates", {
  cp <- withr::local_tempfile(fileext = ".csv"); sp <- withr::local_tempfile(fileext = ".csv")
  write_utf8(charts_lines, cp); write_utf8(series_lines, sp)

  spec <- read_chart_specs(cp, sp)

  testthat::expect_equal(spec$charts$title_sl, "Bruto domači proizvod")
  testthat::expect_true(spec$charts$stacked)
  testthat::expect_equal(spec$charts$ylim_min, 90.5)
  testthat::expect_equal(spec$charts$legend_columns, 1L)
  testthat::expect_type(spec$charts$xmin, "character")      # left for parse_spec_date
  testthat::expect_equal(spec$chart_series$plot, c(TRUE, FALSE))
  testthat::expect_equal(spec$chart_series$dashed, c(TRUE, NA))
  testthat::expect_equal(spec$chart_series$position, c(1, 2))
  testthat::expect_equal(spec$chart_series$rolling, c(NA, 3))
  testthat::expect_equal(spec$chart_series$alias, c("s_b1gq_yoy", "x"))
  testthat::expect_equal(spec$chart_series$legend_sl[1], "BDP širše")
})

test_that("semicolon-delimited with comma decimals (Excel, sl-SI) reads the same", {
  cp <- withr::local_tempfile(fileext = ".csv"); sp <- withr::local_tempfile(fileext = ".csv")
  write_utf8(gsub(",", ";", charts_lines) |> sub(pattern = "90\\.5", replacement = "90,5"), cp)
  write_utf8(gsub(",", ";", series_lines), sp)

  spec <- read_chart_specs(cp, sp)

  testthat::expect_equal(spec$charts$ylim_min, 90.5)
  testthat::expect_equal(spec$chart_series$alias, c("s_b1gq_yoy", "x"))
})

test_that("a file that is not UTF-8 fails with instructions, naming the columns", {
  cp <- withr::local_tempfile(fileext = ".csv"); sp <- withr::local_tempfile(fileext = ".csv")
  # 0xE8 is 'c-caron' in Windows-1250 and invalid as a lone UTF-8 byte
  writeBin(c(charToRaw("chart_id,title_sl,title_en\ngdp,doma"), as.raw(0xE8),
             charToRaw("i,GDP\n")), cp)
  write_utf8(series_lines, sp)

  testthat::expect_error(read_chart_specs(cp, sp),
                         regexp = "not valid UTF-8 \\(columns: title_sl\\).*CSV UTF-8")
})

test_that("validation failures surface from the reader unless validate = FALSE", {
  cp <- withr::local_tempfile(fileext = ".csv"); sp <- withr::local_tempfile(fileext = ".csv")
  write_utf8(charts_lines, cp)
  write_utf8(sub("YOY,,,1,0", "yoy,,,1,0", series_lines), sp)

  testthat::expect_error(read_chart_specs(cp, sp), "invalid growth")
  testthat::expect_s3_class(read_chart_specs(cp, sp, validate = FALSE)$charts, "data.frame")
})

test_that("missing file is a clear error", {
  testthat::expect_error(read_chart_specs("nope.csv", "nope2.csv"), "spec file not found: nope.csv")
})
