prep_l <- readRDS(test_path("testdata", "prep_l.rds"))

test_that("multiplication works", {
  p <- function() univariate_line_chart(prep_l)
  vdiffr::expect_doppelganger("univariate line", p)
  roll_prep_l <- add_rolling_average(prep_l)
  p <- function() univariate_line_chart(roll_prep_l)
  vdiffr::expect_doppelganger("univariate rolling", p)
  yoy_prep_l <- add_yoy_change(prep_l)
  p <- function() univariate_line_chart(yoy_prep_l)
  vdiffr::expect_doppelganger("univariate yoy", p)
  prep_l[[1]] <- prep_l[[1]][1:100,]
  p <- function() univariate_line_chart(prep_l)
  vdiffr::expect_doppelganger("na chart", p)
  p <- function() na_chart(prep_l)
  vdiffr::expect_doppelganger("na chart2", p)

})
