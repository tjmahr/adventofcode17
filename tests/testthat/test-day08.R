context("test-day08.R")

test_that("register instructions", {
  lines <- "
  b inc 5 if a > 1
  a inc 1 if b < 5
  c dec -10 if a >= 1
  c inc -20 if c == 10
  "
  lines %>%
    run_register_instructions() %>%
    getElement("max_final") %>%
    expect_equal(1)

  lines %>%
    run_register_instructions() %>%
    getElement("max_ever") %>%
    expect_equal(10)
})
