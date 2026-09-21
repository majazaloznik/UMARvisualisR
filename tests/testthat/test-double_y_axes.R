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
