context("test-day11.R")

test_that("hexagon distances", {


  hexagon_distance_sum <- function(hex_dist) {
    hex_dist$n_x_steps + hex_dist$n_y_steps
  }

  c("ne", "ne", "ne") %>%
    hexagon_distance() %>%
    hexagon_distance_sum() %>%
    expect_equal(3)

  c("ne", "ne", "sw", "sw") %>%
    hexagon_distance() %>%
    hexagon_distance_sum() %>%
    expect_equal(0)

  c("ne", "ne", "s", "s") %>%
    hexagon_distance() %>%
    hexagon_distance_sum() %>%
    expect_equal(2)

  c("se", "sw", "se", "sw", "s") %>%
    hexagon_distance() %>%
    hexagon_distance_sum() %>%
    expect_equal(3)
})
