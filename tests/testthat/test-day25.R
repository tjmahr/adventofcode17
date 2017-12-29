context("test-day25.R")

test_that("turing machine emulation", {
  # testthat::skip("This doesn't work during package checks for some reason")

  demo_rules <- list(
    create_tm_rule("A", "0", "1", "R", "B"),
    create_tm_rule("A", "1", "0", "L", "B"),
    create_tm_rule("B", "0", "1", "L", "A"),
    create_tm_rule("B", "1", "1", "R", "A")
  )

  m <- turing_machine(demo_rules, "A")
  expect_equal(m$format_tape(), "... 0 0 0 0 0 [0 (A)] 0 0 0 0 0 ...")

  m$step()
  expect_equal(m$format_tape(), "... 0 0 0 0 1 [0 (B)] 0 0 0 0 0 ...")

  m$step()
  expect_equal(m$format_tape(), "... 0 0 0 0 0 [1 (A)] 1 0 0 0 0 ...")

  m$step()
  expect_equal(m$format_tape(), "... 0 0 0 0 0 [0 (B)] 0 1 0 0 0 ...")

  m$step()
  expect_equal(m$format_tape(), "... 0 0 0 0 0 [0 (A)] 1 0 1 0 0 ...")

  m$step()
  expect_equal(m$format_tape(), "... 0 0 0 0 1 [1 (B)] 0 1 0 0 0 ...")

  m$step()
  expect_equal(m$format_tape(), "... 0 0 0 1 1 [0 (A)] 1 0 0 0 0 ...")

  expect_equal(m$checksum(), 3)
})
