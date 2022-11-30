test_that("colour palette works", {
  expect_equal(umar_cols("rdeca"), c(rdeca="#A10305"))
  expect_equal(length(umar_cols()), 9)
})
