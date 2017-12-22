context("test-day21.R")

test_that("multiplication works", {
  rules <- c(
    "../.# => ##./#../...",
    ".#./..#/### => #..#/..../..../#..#")

  rules <- rules %>%
    lapply(expand_rule) %>%
    unlist()

  seed <- ".#./..#/###" %>%
    string_to_matrix()

  step1 <- enhance_matrix(seed, rules)

  step1 %>%
    matrix_to_string() %>%
    expect_equal("#..#/..../..../#..#")

  step2 <- enhance_matrix(step1, rules)
  step2 %>%
    matrix_to_string() %>%
    expect_equal("##.##./#..#../....../##.##./#..#../......")
})
