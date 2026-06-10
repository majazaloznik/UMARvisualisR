test_that("umar_palette default is 'default'", {
  expect_equal(umar_palette(), "default")
})

test_that("umar_palette switches to janez", {
  on.exit(umar_palette("default"))
  umar_palette("janez")
  expect_equal(umar_palette(), "janez")
})

test_that("umar_palette rejects invalid values", {
  expect_error(umar_palette("rainbow"), "must be 'default' or 'Janez'")
})

test_that("umar_cols returns different colours per palette", {
  on.exit(umar_palette("default"))
  default_cols <- umar_cols()
  umar_palette("janez")
  cb_cols <- umar_cols()
  expect_false(identical(unname(default_cols[1]), unname(cb_cols[1])))
})

test_that("black is at index 9 in default palette", {
  expect_equal(as.character(umar_cols(9)), "#000000")
})

test_that("black is at index 8 in colourblind palette", {
  on.exit(umar_palette("default"))
  umar_palette("janez")
  expect_equal(as.character(umar_cols(8)), "#000000")
})

test_that("prep_chart respects colourblind palette", {
  on.exit(umar_palette("default"))
  df <- data.frame(date = as.Date("2020-01-01") + 0:4, A = 1:5, B = 6:10)
  umar_palette("janez")
  chart <- prep_chart(df)
  expect_equal(chart$series[[1]]$colour, as.character(umar_cols(1)))
})

test_that("colours = 9 (black) works in default palette", {
  df <- data.frame(date = as.Date("2020-01-01") + 0:4, value = 1:5)
  chart <- prep_chart(df, colours = 9)
  expect_equal(unname(chart$series[[1]]$colour), "#000000")
})
