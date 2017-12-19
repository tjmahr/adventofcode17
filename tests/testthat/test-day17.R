context("test-day17.R")

test_that("spinlock", {
  spinlock(3, 0) %>% expect_equal(c(0))
  spinlock(3, 1) %>% expect_equal(c(0, 1))
  spinlock(3, 2) %>% expect_equal(c(0, 2, 1))
  spinlock(3, 3) %>% expect_equal(c(0, 2, 3, 1))
  spinlock(3, 4) %>% expect_equal(c(0, 2, 4, 3, 1))
  spinlock(3, 5) %>% expect_equal(c(0, 5, 2, 4, 3, 1))
  spinlock(3, 6) %>% expect_equal(c(0, 5, 2, 4, 3, 6, 1))
  spinlock(3, 7) %>% expect_equal(c(0, 5, 7, 2, 4, 3, 6, 1))
  spinlock(3, 8) %>% expect_equal(c(0, 5, 7, 2, 4, 3, 8, 6, 1))
  spinlock(3, 9) %>% expect_equal(c(0, 9, 5, 7, 2, 4, 3, 8, 6, 1))

  spinlock(3, 2017) %>%
    get_spinlock_value_after_last_insertion() %>%
    expect_equal(638)
})
