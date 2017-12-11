context("test-day10.R")

test_that("Knot hashing", {
  args <- list(
    items = 0:4,
    step = 0,
    current_position = 1,
    lengths = c(3, 4, 1, 5))

  a <- do_call_twist_a_knot(args)
  b <- do_call_twist_a_knot(a)
  c <- do_call_twist_a_knot(b)
  d <- do_call_twist_a_knot(c)

  expect_equal(a$items, c(2, 1, 0, 3, 4))
  expect_equal(b$items, c(4, 3, 0, 1, 2))
  expect_equal(c$items, c(4, 3, 0, 1, 2))
  expect_equal(d$items, c(3, 4, 2, 1, 0))

  expect_equal(a$step, 1)
  expect_equal(b$step, 2)
  expect_equal(c$step, 3)
  expect_equal(d$step, 4)

  expect_equal(a$pos_change, 3)
  expect_equal(b$pos_change, 5)
  expect_equal(c$pos_change, 3)
  expect_equal(d$pos_change, 8)

  all_at_once <- twist_knot(args)
  expect_equal(all_at_once$items, c(3, 4, 2, 1, 0))

  create_day10_lengths("") %>%
    expect_equal(c(17, 31, 73, 47, 23))

  create_day10_lengths("1,2,3") %>%
    expect_equal(c(49, 44, 50, 44, 51, 17, 31, 73, 47, 23))

  c(65, 27, 9, 1, 4, 3, 40, 50, 91, 7, 6, 0, 2, 5, 68, 22) %>%
    reduce_bitwise_xor() %>%
    expect_equal(64)

  knot_hash("") %>%
    expect_equal("a2582a3a0e66e6e86e3812dcb672a272")

  knot_hash("AoC 2017") %>%
    expect_equal("33efeb34ea91902bb2f59c9920caa6cd")

  knot_hash("1,2,3") %>%
    expect_equal("3efbe78a8d82f29979031a4aa0b16a9d")

  knot_hash("1,2,4") %>%
    expect_equal("63960835bcdc130f0b66d7ff4f6a5a8e")
})
