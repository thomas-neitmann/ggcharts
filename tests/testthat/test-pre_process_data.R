context("Pre Process Data")

df <- dplyr::tribble(
  ~cat, ~val,
  "A", 3,
  "B", 1,
  "C", 7
)

test_that("Input does not change when all args are NULL/FALSE", {
  expect_equal(pre_process_data(df, cat, val, sort = FALSE), df)
})

test_that("x is converted to factor with levels in order of y when sort = TRUE", {
  expect_equal(
    pre_process_data(df, cat, val),
    dplyr::tibble(
      cat = factor(LETTERS[1:3], levels = c("B", "A", "C")),
      val = c(3, 1, 7)
    )
  )
})

test_that("limit works", {
  expect_equal(
    pre_process_data(df, cat, val, limit = 1),
    dplyr::tibble(cat = factor("C"), val = 7)
  )
})

test_that("theshold works", {
  expect_equal(
    pre_process_data(df, cat, val, threshold = 2),
    dplyr::tibble(cat = factor(c("A", "C")), val = c(3, 7))
  )
})
