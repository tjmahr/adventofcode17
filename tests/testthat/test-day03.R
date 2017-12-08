context("test-day03.R")

test_that("spiral distance", {
  spiral_distance(1) %>% expect_equal(0)
  spiral_distance(12) %>% expect_equal(3)
  spiral_distance(23) %>% expect_equal(2)
  spiral_distance(1024) %>% expect_equal(31)
})
