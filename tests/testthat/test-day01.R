context("test-day01.R")

test_that("Sum of digits matching next in circular list", {
  sum_of_digits_matching_next(1122) %>%
    expect_equal(3)

  sum_of_digits_matching_next(1111) %>%
    expect_equal(4)

  sum_of_digits_matching_next(1234) %>%
    expect_equal(0)

  sum_of_digits_matching_next(91212129) %>%
    expect_equal(9)
})

test_that("Sum of digits matching halfway around the circular list", {
  sum_of_digits_matching_halfway_around(1212) %>%
    expect_equal(6)

  sum_of_digits_matching_halfway_around(1221) %>%
    expect_equal(0)

  sum_of_digits_matching_halfway_around(123425) %>%
    expect_equal(4)

  sum_of_digits_matching_halfway_around(123123) %>%
    expect_equal(12)

  sum_of_digits_matching_halfway_around(12131415) %>%
    expect_equal(4)
})

