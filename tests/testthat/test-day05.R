context("test-day05.R")

test_that("following processing instructions", {
  x <- "
  0
  3
  0
  1
  -3
  "

  follow_day05a_instructions(x) %>% expect_equal(5)
  follow_day05b_instructions(x) %>% expect_equal(10)

})
