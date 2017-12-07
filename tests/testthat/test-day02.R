context("test-day02.R")

test_that("range checksum", {
  string <-
    "5 1 9 5
    7 5 3
    2 4 6 8"

  checksum_range(string) %>%
    expect_equal(18)
})

test_that("evenly divisible checksum", {
  string <- "
  5 9 2 8
  9 4 7 3
  3 8 6 5
  "

  checksum_evenly_divisible(string) %>%
    expect_equal(9)
})
