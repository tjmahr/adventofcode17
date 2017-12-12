context("test-day06.R")

test_that("reallocation", {
  c(0, 2, 7, 0) %>%
    reallocate() %>% expect_equal(c(2, 4, 1, 2)) %>%
    reallocate() %>% expect_equal(c(3, 1, 2, 3)) %>%
    reallocate() %>% expect_equal(c(0, 2, 3, 4)) %>%
    reallocate() %>% expect_equal(c(1, 3, 4, 1)) %>%
    reallocate() %>% expect_equal(c(2, 4, 1, 2))

  c(0, 2, 7, 0) %>%
    analyze_reallocations() %>%
    getElement("cycles_until_repeat") %>%
    expect_equal(5)

  c(0, 2, 7, 0) %>%
    analyze_reallocations() %>%
    getElement("loop_length") %>%
    expect_equal(4)
})
