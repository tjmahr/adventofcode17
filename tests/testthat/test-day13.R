context("test-day13.R")

test_that("multiplication works", {
  locate_scanner(3, 1) %>% expect_equal(1)
  locate_scanner(3, 2) %>% expect_equal(2)
  locate_scanner(3, 3) %>% expect_equal(3)
  locate_scanner(3, 4) %>% expect_equal(2)
  locate_scanner(3, 5) %>% expect_equal(1)
  locate_scanner(3, 6) %>% expect_equal(2)
  locate_scanner(3, 7) %>% expect_equal(3)
  locate_scanner(3, 8) %>% expect_equal(2)

  locate_scanner(2, 1) %>% expect_equal(1)
  locate_scanner(2, 2) %>% expect_equal(2)
  locate_scanner(2, 3) %>% expect_equal(1)
  locate_scanner(2, 4) %>% expect_equal(2)
  locate_scanner(2, 5) %>% expect_equal(1)
  locate_scanner(2, 6) %>% expect_equal(2)

  locate_scanner(1, 1) %>% expect_equal(1)
  locate_scanner(2, 2) %>% expect_equal(2)
  locate_scanner(2, 3) %>% expect_equal(1)
  locate_scanner(2, 4) %>% expect_equal(2)
  locate_scanner(2, 5) %>% expect_equal(1)
  locate_scanner(2, 6) %>% expect_equal(2)

  test <- "
  0: 3
  1: 2
  4: 4
  6: 4"

  update_lists <- function(x) c(x, list(location = locate_scanner(x$range, x$depth + 1)))
  l <- test %>%
    read_text_lines() %>%
    lapply(parse_scanner_line) %>%
    lapply(update_lists)

  str(l)
  locate_scanner(l[[1]][["range"]], l[[1]][["depth"]] + 1)
  %>%
    lapply(update_lists)

})
