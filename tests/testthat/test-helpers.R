# === truncate_to ===

test_that("truncate_to blanks everything after the reference's last value", {
  x <- 1:8 * 10
  g <- c(1:6, NA, NA)
  expect_equal(truncate_to(x, g), c(10, 20, 30, 40, 50, 60, NA, NA))
})

test_that("truncate_to leaves x alone when the reference runs at least as far", {
  x <- c(1:6, NA, NA)
  g <- 1:8
  expect_equal(truncate_to(x, g), x)
})

test_that("only the reference's LAST value matters, not gaps before it", {
  x <- 1:6 * 10
  g <- c(1, NA, NA, 4, NA, NA)         # interior gaps, last value at 4
  expect_equal(truncate_to(x, g), c(10, 20, 30, 40, NA, NA))
})

test_that("x's own missing values before the cutoff survive", {
  x <- c(1, NA, 3, 4, 5)
  g <- c(1, 2, 3, NA, NA)
  expect_equal(truncate_to(x, g), c(1, NA, 3, NA, NA))
})

test_that("an all-NA reference blanks everything, same length out", {
  out <- truncate_to(1:5, rep(NA_real_, 5))
  expect_length(out, 5)
  expect_true(all(is.na(out)))
})

test_that("truncate_to refuses mismatched lengths", {
  expect_error(truncate_to(1:5, 1:4), "same length")
})

# === reachable from a spec formula ===

test_that("the validator sees only the aliases, not the function name", {
  expect_equal(sort(formula_vars("truncate_to(x, g)")), c("g", "x"))
})

test_that("truncate_to works as a formula in build_chart_data", {
  periods <- c("2022Q1", "2022Q2", "2022Q3", "2022Q4",
               "2023Q1", "2023Q2", "2023Q3", "2023Q4")
  stub <- function(codes, con, date_valid = NULL, schema = "platform") {
    tibble::tibble(period_id = periods,
                   g = c(1:6, NA, NA),        # reference stops two periods early
                   x = 1:8 * 10)
  }
  rows <- tibble::tibble(
    chart_id    = "t",
    position    = 1:3,
    alias       = c("g", "x", "trunc"),
    source_type = c("db", "db", "computed"),
    series_code = c("S--G", "S--X", NA),
    formula     = c(NA, NA, "truncate_to(x, g)"),
    plot        = c(FALSE, FALSE, TRUE),
    growth      = NA_character_,
    index       = NA_character_,
    rolling     = NA_real_
  )
  wide <- build_chart_data(rows, con = NULL, fetch_fn = stub)

  expect_equal(wide$trunc, c(10, 20, 30, 40, 50, 60, NA, NA))
  expect_true("g" %in% names(wide))   # reference kept for the data export
})


test_that("monthly values collapse to quarters", {
  d <- data.frame(period_id = sprintf("2023M%02d", 1:6), a = 1:6, b = c(2, 4, 6, 8, 10, 12))
  out <- aggregate_to_quarters(d, c("a", "b"))
  expect_equal(out$period_id, c("2023Q1", "2023Q2"))
  expect_equal(out$a, c(2, 5))
  expect_equal(out$b, c(4, 10))
})

test_that("sum aggregates as well as mean", {
  d <- data.frame(period_id = sprintf("2023M%02d", 1:3), a = 1:3)
  expect_equal(aggregate_to_quarters(d, "a", aggr_func = sum)$a, 6)
})

test_that("an incomplete quarter is NA, not a partial aggregate", {
  d <- data.frame(period_id = sprintf("2023M%02d", 1:5), a = 1:5)   # Q2 has 2 months
  out <- aggregate_to_quarters(d, "a")
  expect_equal(out$a, c(2, NA))
  expect_equal(aggregate_to_quarters(d, "a", min_obs = 1)$a, c(2, 4.5))
})

test_that("a missing month counts the same as an absent row", {
  gap <- data.frame(period_id = sprintf("2023M%02d", 1:3), a = c(1, NA, 3))
  short <- data.frame(period_id = sprintf("2023M%02d", c(1, 3)), a = c(1, 3))
  expect_true(is.na(aggregate_to_quarters(gap, "a")$a))
  expect_true(is.na(aggregate_to_quarters(short, "a")$a))
})

test_that("an all-NA quarter is NA rather than NaN", {
  d <- data.frame(period_id = sprintf("2023M%02d", 1:3), a = NA_real_)
  expect_true(is.na(aggregate_to_quarters(d, "a", min_obs = 1)$a))
})

test_that("non-monthly, non-quarterly periods are refused", {
  d <- data.frame(period_id = c("2023", "2024"), a = 1:2)          # annual
  expect_error(aggregate_to_quarters(d, "a"), "must be monthly")
})

test_that("an already-quarterly series passes through beside monthly ones", {
  d <- data.frame(
    period_id = c(sprintf("2023M%02d", 1:6), "2023Q1", "2023Q2"),
    m = c(1:6, NA, NA),        # monthly series
    q = c(rep(NA, 6), 10, 20)  # quarterly series
  )
  out <- aggregate_to_quarters(d, c("m", "q"))
  expect_equal(out$period_id, c("2023Q1", "2023Q2"))
  expect_equal(out$m, c(2, 5))      # collapsed
  expect_equal(out$q, c(10, 20))    # untouched, not blanked by min_obs
})
