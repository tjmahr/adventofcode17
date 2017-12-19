context("test-day15.R")

test_that("generators work", {
  a <- create_generator(seed = 65, factor = 16807, divisor = 2147483647)
  b <- create_generator(seed = 8921, factor = 48271, divisor = 2147483647)

  gen_a <- c(1092455, 1181022009, 245556042, 1744312007, 1352636452)
  gen_b <- c(430625591, 1233683848, 1431495498, 137874439, 285222916)

  expect_equal(a(5), gen_a)
  expect_equal(b(5), gen_b)
})

test_that("judging generators", {
  a <- create_generator(seed = 65, factor = 16807, divisor = 2147483647)
  b <- create_generator(seed = 8921, factor = 48271, divisor = 2147483647)

  matches <- judge_generators(a, b, 5)
  expect_equal(matches, 1)
})

test_that("filtered generators work", {
  a <- create_generator(seed = 65, factor = 16807, divisor = 2147483647,
                        criterion = is_divisible_by_4)
  b <- create_generator(seed = 8921, factor = 48271, divisor = 2147483647,
                        criterion = is_divisible_by_8)

  gen_a <- c(1352636452, 1992081072, 530830436, 1980017072, 740335192)
  gen_b <- c(1233683848, 862516352, 1159784568, 1616057672, 412269392)

  expect_equal(a(5), gen_a)
  expect_equal(b(5), gen_b)
})

test_that("judging filtered generators", {
  a <- create_generator(seed = 65, factor = 16807, divisor = 2147483647,
                        criterion = is_divisible_by_4)
  b <- create_generator(seed = 8921, factor = 48271, divisor = 2147483647,
                        criterion = is_divisible_by_8)

  # No match until 1056
  expect_equal(judge_generators(a, b, 1055), 0)
  expect_equal(judge_generators(a, b, 1), 1)
})
