prep_l <- readRDS(test_path("testdata", "prep_l.rds"))

test_that("multiplication works", {
  p <- function() univariate_line_chart(prep_l)
  vdiffr::expect_doppelganger("univariate line", p)
  })
