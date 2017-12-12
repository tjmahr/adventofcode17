context("test-day12.R")

test_that("breadth first search", {
  pipes <-
   "0 <-> 2
    1 <-> 1
    2 <-> 0, 3, 4
    3 <-> 2, 4
    4 <-> 2, 3, 6
    5 <-> 6
    6 <-> 4, 5" %>%
    read_text_lines()

  pipes %>%
    search_pipes_from_zero() %>%
    expect_equal(c("0", "2", "3", "4", "5", "6"))

  pipes %>%
    count_pipe_groups() %>%
    expect_equal(2)
})
