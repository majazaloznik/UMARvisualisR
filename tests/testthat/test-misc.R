test_that("range finding works ", {
  expect_true(in_range(1, range(1, 2)))
  expect_error(in_range(1, 2))
  expect_error(in_range(1, c(2,1)))

  expect_false(in_range_strict(1, range(1, 2)))
  expect_error(in_range_strict(1, 2))
  expect_error(in_range_strict(1, c(2,1)))
})
