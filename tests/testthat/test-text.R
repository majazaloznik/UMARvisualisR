test_that("string wrapping", {
input <- "testing 123 testing 234"
  out <- wrap_string(input, 12)
  expect_equal(nchar(input) + 1, nchar(encodeString(out)))
})
