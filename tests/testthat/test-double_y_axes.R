# === nice_step ===

test_that("nice_step climbs the 1/2/2.5/5/10 ladder", {
  expect_equal(nice_step(1), 1)
  expect_equal(nice_step(1.5), 2)
  expect_equal(nice_step(2), 2)          # exact hits stay put
  expect_equal(nice_step(2.1), 2.5)
  expect_equal(nice_step(3), 5)
  expect_equal(nice_step(6), 10)
  expect_equal(nice_step(0.15), 0.2)     # scales down
  expect_equal(nice_step(15), 20)        # and up
  expect_equal(nice_step(250), 250)
})

test_that("nice_step refuses input it cannot scale", {
  expect_error(nice_step(0), "positive")
  expect_error(nice_step(-1), "positive")
  expect_error(nice_step(NA_real_), "positive")
  expect_error(nice_step(Inf), "positive")
})

# === axis_on_grid, floating range ===

test_that("a floating axis covers the data in exactly n intervals", {
  a <- axis_on_grid(0, 95, 5)
  expect_equal(a$ylim, c(0, 100))
  expect_equal(a$y_breaks, seq(0, 100, by = 20))
  expect_equal(a$step, 20)
  expect_length(a$y_breaks, 6)
})

test_that("an all-positive axis is never padded below zero", {
  a <- axis_on_grid(0, 3, 5)             # needs 2 intervals of padding
  expect_equal(a$ylim, c(0, 5))
  expect_true(a$ylim[1] >= 0)

  b <- axis_on_grid(2, 9, 5)
  expect_true(b$ylim[1] >= 0)
})

test_that("a floating axis puts zero on a gridline whenever zero is in range", {
  for (lo in c(-1, -8, -30)) {
    for (hi in c(3, 12, 47, 180)) {
      a <- axis_on_grid(lo, hi, 5)
      expect_true(min(abs(a$y_breaks)) < 1e-9 * max(abs(a$ylim)))
    }
  }
})

test_that("a flat series still gets a usable axis", {
  a <- axis_on_grid(7, 7, 4)
  expect_length(a$y_breaks, 5)
  expect_true(a$ylim[1] < 7 && a$ylim[2] > 7)

  b <- axis_on_grid(0, 0, 4)             # flat at zero, no relative padding to take
  expect_length(b$y_breaks, 5)
  expect_true(b$ylim[1] < 0 && b$ylim[2] > 0)
})

# === axis_on_grid, anchored to zero ===

test_that("an anchored axis puts zero on the j-th break and still covers the data", {
  a <- axis_on_grid(-5, 15, 4, j = 1)
  expect_equal(a$ylim, c(-5, 15))
  expect_equal(a$y_breaks, seq(-5, 15, by = 5))
  expect_equal(a$y_breaks[2], 0)

  b <- axis_on_grid(-2, 8, 5, j = 1)
  expect_equal(b$y_breaks[2], 0)
  expect_true(b$ylim[1] <= -2 && b$ylim[2] >= 8)
})

test_that("an anchored axis that cannot cover the data returns NULL", {
  expect_null(axis_on_grid(-5, 10, 4, j = 0))    # no room below zero
  expect_null(axis_on_grid(-5, 10, 4, j = 4))    # no room above
  expect_null(axis_on_grid(-5, 10, 4, j = 9))    # j out of range
})

# === pair_y_scales ===

covers <- function(a, lo, hi) a$ylim[1] <= lo + 1e-9 && a$ylim[2] >= hi - 1e-9
zero_height <- function(a) (0 - a$ylim[1]) / diff(a$ylim)

test_that("derived scales share their break count and cover both sets of data", {
  out <- pair_y_scales(c(-5, 12), c(-2, 3))
  expect_length(out$problems, 0)
  expect_equal(length(out$left$y_breaks), length(out$right$y_breaks))
  expect_true(covers(out$left, -5, 12))
  expect_true(covers(out$right, -2, 3))
})

test_that("zero lands at the same height when both sides straddle it", {
  out <- pair_y_scales(c(-5, 12), c(-2, 3))
  expect_equal(zero_height(out$left), zero_height(out$right), tolerance = 1e-8)
  expect_true(min(abs(out$left$y_breaks))  < 1e-9 * max(abs(out$left$ylim)))
  expect_true(min(abs(out$right$y_breaks)) < 1e-9 * max(abs(out$right$ylim)))
})

test_that("both axes are anchored on their reference even when one is far from it", {
  out <- pair_y_scales(c(-5, 12), c(95, 140))
  expect_length(out$problems, 0)
  expect_equal(length(out$left$y_breaks), length(out$right$y_breaks))
  expect_true(covers(out$left, -5, 12))
  expect_true(covers(out$right, 95, 140))
  expect_equal(zero_height(out$left), zero_height(out$right), tolerance = 1e-8)
  # the price: the right axis is dragged down to reach a zero its data never sees
  expect_true(out$right$ylim[1] < 0)
})

test_that("a reference of 100 keeps an index axis tight", {
  out <- pair_y_scales(c(-5, 12), c(95, 140), ref2 = 100)
  expect_length(out$problems, 0)
  expect_equal(length(out$left$y_breaks), length(out$right$y_breaks))
  expect_true(covers(out$right, 95, 140))
  expect_true(out$right$ylim[1] > 0)          # no longer dragged to zero
  expect_equal((0   - out$left$ylim[1])  / diff(out$left$ylim),
               (100 - out$right$ylim[1]) / diff(out$right$ylim), tolerance = 1e-8)
})
test_that("a fixed left scale is respected and the right one is derived to match", {
  out <- pair_y_scales(values_left = NULL, values_right = c(-2, 6),
                       ylim = c(-5, 15))
  expect_equal(out$left$ylim, c(-5, 15))
  expect_equal(out$right$ylim, c(-2, 6))
  expect_equal(length(out$left$y_breaks), length(out$right$y_breaks))
  expect_equal(zero_height(out$left), zero_height(out$right), tolerance = 1e-8)
  expect_length(out$problems, 0)
})

test_that("a fixed right scale works the same way round", {
  out <- pair_y_scales(values_left = c(-2, 6), values_right = NULL,
                       ylim2 = c(-5, 15))
  expect_equal(out$right$ylim, c(-5, 15))
  expect_equal(length(out$left$y_breaks), length(out$right$y_breaks))
  expect_length(out$problems, 0)
})

test_that("both fixed get a break count that lands on round numbers", {
  out <- pair_y_scales(NULL, NULL, ylim = c(-5, 15), ylim2 = c(-2, 6))
  expect_equal(out$left$ylim, c(-5, 15))
  expect_equal(out$right$ylim, c(-2, 6))
  expect_equal(out$left$y_breaks,  seq(-5, 15, by = 5))
  expect_equal(out$right$y_breaks, seq(-2, 6, by = 2))
  expect_length(out$problems, 0)
})

test_that("both fixed with no round break count report rather than throw", {
  out <- pair_y_scales(NULL, NULL, ylim = c(-3, 10), ylim2 = c(-1, 12))
  expect_equal(out$left$ylim, c(-3, 10))
  expect_equal(length(out$left$y_breaks), length(out$right$y_breaks))
  expect_true(length(out$problems) > 0)
})

test_that("the returned sides have find_pretty_ylim's shape", {
  out <- pair_y_scales(c(-5, 12), c(-2, 3))
  expect_true(all(c("ylim", "y_breaks") %in% names(out$left)))
  expect_true(all(c("ylim", "y_breaks") %in% names(out$right)))
  expect_length(out$left$ylim, 2)
})

# === check_axis_pair ===

scale_of <- function(lims, n) list(ylim = lims, y_breaks = seq(lims[1], lims[2], length.out = n + 1))

test_that("a compatible pair has no problems", {
  expect_length(check_axis_pair(scale_of(c(-5, 15), 4), scale_of(c(-2, 6), 4)), 0)
  # neither side reaches zero, so the scales need not relate at all
  expect_length(check_axis_pair(scale_of(c(0, 100), 5), scale_of(c(90, 140), 5)), 0)
})

test_that("mismatched break counts are caught", {
  problems <- check_axis_pair(scale_of(c(0, 100), 5), scale_of(c(0, 60), 3))
  expect_length(problems, 1)
  expect_match(problems, "breaks")
})

test_that("a zero at a different height on each axis is caught", {
  problems <- check_axis_pair(scale_of(c(-5, 15), 4), scale_of(c(-4, 4), 4))
  expect_true(any(grepl("different height", problems)))
})

test_that("a shared zero height that misses the gridlines is still flagged", {
  problems <- check_axis_pair(scale_of(c(-3, 17), 4), scale_of(c(-1.5, 8.5), 4))
  expect_false(any(grepl("different height", problems)))   # heights do match
  expect_true(any(grepl("gridline", problems)))
})


with_device <- function(code) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  plot.new()
  force(code)
}

cfg <- function(label = "Desezonirana stopnja, v %", unit = "%", mio = FALSE) {
  list(y_axis_label = label,
       series = list(list(unit = unit, mio_eur = mio),
                     list(unit = unit, mio_eur = mio)))
}

# === right_axis_label_width ===

test_that("right_axis_label_width returns the same shape as its left twin", {
  with_device({
    y <- list(ylim = c(-2, 6), y_breaks = seq(-2, 6, by = 2))
    out <- right_axis_label_width(cfg(), y)
    expect_named(out, c("unit", "axis_labels", "axis_positions",
                        "y_lab_lines", "y_axis_label"))
    expect_equal(out$axis_positions, y$y_breaks)
    expect_length(out$axis_labels, length(y$y_breaks))
  })
})

test_that("right_axis_label_width sets only the right margin", {
  with_device({
    y <- list(ylim = c(-2, 6), y_breaks = seq(-2, 6, by = 2))
    before <- par("mar")
    out <- right_axis_label_width(cfg(), y)
    after <- par("mar")
    expect_equal(after[1:3], before[1:3])
    n_lines <- length(strsplit(out$y_axis_label, "\n", fixed = TRUE)[[1]])
    expect_equal(after[4], out$y_lab_lines + n_lines + 0.35)
  })
})

test_that("edge_pad widens the margin without moving the labels", {
  with_device({
    y <- list(ylim = c(-2, 6), y_breaks = seq(-2, 6, by = 2))
    a <- right_axis_label_width(cfg("v %"), y, edge_pad = 0)
    m_a <- par("mar")[4]
    b <- right_axis_label_width(cfg("v %"), y, edge_pad = 0.5)
    m_b <- par("mar")[4]
    expect_equal(m_b - m_a, 0.5)
    expect_equal(a$y_lab_lines, b$y_lab_lines)
    expect_equal(a$axis_labels, b$axis_labels)
  })
})

test_that("right axis labels follow the language", {
  with_device({
    y <- list(ylim = c(0, 3000), y_breaks = seq(0, 3000, by = 1000))
    si <- right_axis_label_width(cfg("v %"), y, language = "si")
    en <- right_axis_label_width(cfg("v %"), y, language = "en")
    expect_true(any(grepl(".", si$axis_labels, fixed = TRUE)))
    expect_true(any(grepl(",", en$axis_labels, fixed = TRUE)))
  })
})

test_that("EUR with mio_eur divides the right labels by a million", {
  with_device({
    y <- list(ylim = c(0, 3e6), y_breaks = seq(0, 3e6, by = 1e6))
    out <- right_axis_label_width(
      list(y_axis_label = NULL, series = list(list(unit = "EUR", mio_eur = TRUE))), y)
    expect_equal(out$unit, "Mio EUR")
    expect_equal(out$y_axis_label, "Mio EUR")        # title falls back to the unit
    expect_true(any(trimws(out$axis_labels) == "3"))
  })
})


# === dual_axis_advice ===

sc <- function(l, r, problems = character(0)) {
  mk <- function(v) list(ylim = v, y_breaks = seq(v[1], v[2], length.out = 5))
  list(left = mk(l), right = mk(r), problems = problems)
}

test_that("a chart where one axis would have done is flagged, by its label", {
  out <- dual_axis_advice(c(0, 10), c(2, 12), "v %", "v %",
                          "line", "line", sc(c(0, 10), c(2, 12)))
  expect_length(out, 1)
  expect_match(out, "single axis")
  expect_match(out, "v %", fixed = TRUE)
})

test_that("series at genuinely different magnitudes are left alone", {
  out <- dual_axis_advice(c(95, 140), c(-2, 6), "Indeks", "v %",
                          "line", "line", sc(c(90, 140), c(-2, 6)),
                          ref = 100, ref2 = 0)
  expect_length(out, 0)
})

test_that("bars on both axes are flagged", {
  out <- dual_axis_advice(c(0, 100), c(0, 5), "Mio EUR", "v %",
                          "bar", c("bar", "line"), sc(c(0, 100), c(0, 5)))
  expect_true(any(grepl("bars on both axes", out)))
})

test_that("a reference crossed only on the right says no line is drawn", {
  out <- dual_axis_advice(c(6, 8), c(-2, 6), "v %", "Indeks",
                          "line", "line", sc(c(4, 9), c(-2, 6)))
  expect_true(any(grepl("no line is drawn", out)))
})

test_that("a reference crossed only on the left says the line is arbitrary", {
  out <- dual_axis_advice(c(-2, 6), c(96, 99), "v %", "Indeks",
                          "line", "line", sc(c(-2, 6), c(95, 100)),
                          ref = 0, ref2 = 100)
  expect_true(any(grepl("arbitrary height", out)))
})

test_that("scale problems are passed through", {
  out <- dual_axis_advice(c(0, 10), c(0, 5), "a", "b", "line", "line",
                          sc(c(0, 10), c(0, 5),
                             problems = "different number of breaks (5 left, 4 right)"))
  expect_true(any(grepl("not aligned", out)))
})

test_that("min_share tunes how eagerly a single axis is suggested", {
  args <- list(values_left = c(0, 100), values_right = c(40, 60),
               label_left = "a", label_right = "b",
               types_left = "line", types_right = "line",
               scales = sc(c(0, 100), c(40, 60)))
  expect_length(do.call(dual_axis_advice, args), 0)
  expect_true(length(do.call(dual_axis_advice, c(args, list(min_share = 0.15)))) > 0)
})

# === pair_y_scales: nothing found ===

test_that("pair_y_scales fails loudly when no scale can be found", {
  expect_error(pair_y_scales(c(-5, 12), c(-2, 3), n_range = integer(0)),
               "no compatible pair")
  expect_error(pair_y_scales(NULL, c(-2, 3), ylim = c(-5, 15), n_range = integer(0)),
               "compatible with the fixed limits")
})
