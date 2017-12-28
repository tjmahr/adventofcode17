context("test-day05.R")

test_that("following processing instructions", {
  x <- "
  0
  3
  0
  1
  -3
  "

  find_time_to_escape_trampolines(x) %>% expect_equal(5)
  find_time_to_escape_twistolines(x) %>% expect_equal(10)
})
