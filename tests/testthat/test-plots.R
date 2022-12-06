prep_l <- readRDS(test_path("testdata", "prep_l.rds"))

test_that("multiplication works", {
  p <- function() univariate_line_chart(prep_l)
  vdiffr::expect_doppelganger("univariate line", p)
  prep_l[[1]] <- prep_l[[1]][1:100,]
  p <- function() univariate_line_chart(prep_l)
  vdiffr::expect_doppelganger("na chart", p)
  p <- function() na_chart(prep_l)
  vdiffr::expect_doppelganger("na chart2", p)
})
