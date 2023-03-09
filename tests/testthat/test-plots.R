prep_l <- readRDS(test_path("testdata", "prep_l.rds"))
prep_lm <- readRDS(test_path("testdata", "prep_lm.rds"))
prep_slm <- readRDS(test_path("testdata", "prep_slm.rds"))
prep_a <- readRDS(test_path("testdata", "prep_a.rds"))

test_that("plots look right", {
  p <- function() univariate_line_chart(prep_l)
  vdiffr::expect_doppelganger("univariate line", p)
  roll_prep_l <- do_transformations(c(prep_l, rolling_average_periods = 3,
                       rolling_average_alignment = "c", year_on_year = FALSE,interval = "M"))
  p <- function() univariate_line_chart(roll_prep_l)
  vdiffr::expect_doppelganger("univariate rolling", p)
  yoy_prep_l <- do_transformations(c(prep_l, rolling_average_periods = NA,
                                                    rolling_average_alignment = NA,
                                                    year_on_year = TRUE,interval = "M"))
  p <- function() univariate_line_chart(yoy_prep_l)
  vdiffr::expect_doppelganger("univariate yoy", p)
  roll_yoy_prep_l <- do_transformations(c(prep_l, rolling_average_periods = 3,
                                          rolling_average_alignment = "c",
                                          year_on_year = TRUE,interval = "M"))
  p <- function() univariate_line_chart(roll_yoy_prep_l)
  vdiffr::expect_doppelganger("univariate yoy rolling", p)
  prep_l$data_points[[1]] <- prep_l$data_points[[1]][1:100,]
  p <- function() univariate_line_chart(prep_l)
  vdiffr::expect_doppelganger("na chart", p)
  p <- function() na_chart(prep_l)
  vdiffr::expect_doppelganger("na chart2", p)
  p <- function() multivariate_line_chart(prep_lm)
  vdiffr::expect_doppelganger("multi 8", p)
  p <- function() multivariate_line_chart(prep_slm)
  vdiffr::expect_doppelganger("single with multi", p)
  p <- function() multivariate_line_chart(prep_a)
  vdiffr::expect_doppelganger("single with annual", p)
})
