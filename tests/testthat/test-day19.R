context("test-day19.R")

test_that("following a path", {
  # The vector below has this grid line by line:
  #     |
  #     |  +--+
  #     A  |  C
  # F---|----E|--+
  #     |  |  |  D
  #     +B-+  +--+
  grid <- c("    |", "    |  +--+", "    A  |  C", "F---|----E|--+",
            "    |  |  |  D", "    +B-+  +--+")

  grid %>%
    create_board() %>%
    collect_board_letters() %>%
    paste0(collapse = "") %>%
    expect_equal("ABCDEF")

  grid %>%
    create_board() %>%
    walk_board_from_start() %>%
    length() %>%
    expect_equal(38)
})
