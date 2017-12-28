context("test-day02.R")

test_that("range checksum", {
  f_diff <- function(x) max(x) - min(x)

  "5 1 9 5\n7 5 3\n2 4 6 8" %>%
    spreadsheet_checksum(max_min_pair, f_diff) %>%
    expect_equal(18)
})

test_that("evenly divisible checksum", {
  f_divide <- function(x) max(x) / min(x)

  "5 9 2 8\n9 4 7 3\n3 8 6 5" %>%
    spreadsheet_checksum(evenly_divisible_pair, f_divide) %>%
    expect_equal(9)
})
