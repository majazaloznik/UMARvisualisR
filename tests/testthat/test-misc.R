test_that("range finding works ", {
  expect_true(in_range(1, range(1, 2)))
  expect_error(in_range(1, 2))
  expect_error(in_range(1, c(2,1)))
  expect_false(in_range_strict(1, range(1, 2)))
  expect_error(in_range_strict(1, 2))
  expect_error(in_range_strict(1, c(2,1)))
  expect_equal(first_up("in"), "In")
  expect_equal(first_up("iN"), "IN")
  expect_equal(find_pretty_ylim(c(1, 3, 10)), c(0, 12))
  expect_equal(0, nrow(apply_xlims(data.frame(period = as.Date("2010/01/01"),
                                              period_id = as.Date("2010/01/01"),
                                              value = 1))))
  expect_equal(1, nrow(apply_xlims(data.frame(period = as.Date("2011/02/01"),
                                              period_id = as.Date("2011/02/01"),
                                              value = 1))))
  df <- data.frame(period = seq(as.Date("2015/1/1"), by="month", length.out = 12),
                   period_id = seq(as.Date("2015/1/1"), by="month", length.out = 12),
                   value = c(NA, NA, 1, NA, NA, 3:7, NA, NA),
                   raw = c(NA, NA, 1, NA, NA, 3:7, 8, NA))
  expect_equal(nrow(remove_head_tail_NAs(df)), 9)
  expect_equal(nrow(apply_xlims(df, xmax = as.Date("2015/08/01"))), 6)
  x <- read_csv_guess_encoding(test_path("testdata", "test_report_input.csv"))
  expect_equal(dim(x), c(31,13))
  expect_false(all_equal(df$raw))
})





