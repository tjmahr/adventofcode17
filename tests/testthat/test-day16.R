context("test-day16.R")

test_that("dancing", {
  programs <- "abcde"

  moves1 <- "s1"
  moves2 <- "s1,x3/4"
  moves3 <- "s1,x3/4,pe/b"

  programs %>%
    dance(moves1) %>%
    paste0(collapse = "") %>%
    expect_equal("eabcd")

  programs %>%
    dance(moves2) %>%
    paste0(collapse = "") %>%
    expect_equal("eabdc")

  programs %>%
    dance(moves3) %>%
    paste0(collapse = "") %>%
    expect_equal("baedc")
})
