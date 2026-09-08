# === get_code ===

test_that("get_code returns valid string", {
  df <- data.frame(date = as.Date("2020-01-01") + 0:4, A = 1:5, B = 6:10)
  chart <- prep_chart(df, title = "Test", y_axis = "%", colours = c(1, 3))
  code <- get_code(chart)
  expect_type(code, "character")
  expect_true(grepl("prep_chart", code))
  expect_true(grepl("view_chart", code))
  expect_true(grepl("title", code))
  expect_true(grepl("y_axis", code))
})

test_that("get_code includes transformations", {
  df <- data.frame(
    date = seq(as.Date("2020-01-01"), by = "month", length.out = 24),
    value = 100 + cumsum(rnorm(24))
  )
  chart <- prep_chart(df, rolling = 3, growth = "YOY")
  code <- get_code(chart)
  expect_true(grepl("rolling = 3", code))
  expect_true(grepl('growth = "YOY"', code))
})

test_that("get_code includes emphasis", {
  df <- data.frame(date = as.Date("2020-01-01") + 0:4, value = 1:5)
  chart <- prep_chart(df, emphasis = c(0, 100))
  code <- get_code(chart)
  expect_true(grepl("emphasis", code))
})

# === save_chart ===

test_that("save_chart rejects invalid extension", {
  df <- data.frame(date = as.Date("2020-01-01") + 0:4, value = 1:5)
  chart <- prep_chart(df)
  expect_error(save_chart(chart, "test.jpg"), "Unsupported")
})

test_that("save_chart rejects invalid size", {
  df <- data.frame(date = as.Date("2020-01-01") + 0:4, value = 1:5)
  chart <- prep_chart(df)
  expect_error(save_chart(chart, "test.pdf", size = 5), "size must be")
})

test_that("get_code round-trips per-series transforms", {
  df <- data.frame(date = seq(as.Date("2015-01-01"), by = "quarter", length.out = 20),
                   A = 1:20, B = 21:40)
  chart <- prep_chart(df, rolling = c(4, NA), growth = c(NA, "YOY"))
  code <- get_code(chart)
  expect_true(grepl("rolling = c(4, NA)", code, fixed = TRUE))
  expect_true(grepl('growth = c(NA, "YOY")', code, fixed = TRUE))
})
