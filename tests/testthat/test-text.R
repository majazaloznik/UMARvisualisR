test_that("string wrapping", {
input <- "testing 123 testing 234"
  out <- wrap_string(input, 12)
  expect_equal(nchar(input) + 1, nchar(encodeString(out[[1]])))
  expect_equal(out[[2]], 2)
  input <- "12 34 56 78"
  out <- wrap_string(input, 2)
  expect_equal(out[[2]], 3)
})
