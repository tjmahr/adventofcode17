context("test-day13.R")

test_that("locating a firewall scanner", {
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

  "0: 3
  1: 2
  4: 4
  6: 4" %>%
    read_text_lines() %>%
    calculate_firewall_severity() %>%
    expect_equal(24)

  "0: 3
  1: 2
  4: 4
  6: 4" %>%
    read_text_lines() %>%
    determine_firewall_delay() %>%
    expect_equal(10)
})
