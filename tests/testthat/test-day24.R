context("test-day24.R")

test_that("finding bridges", {
  pieces <- c("0/2", "2/2", "2/3",  "3/4", "3/5",  "0/1", "10/1", "9/10")

  possible_bridges <- c("0/1--10/1--9/10", "0/2--2/3--3/4", "0/2--2/3--3/5",
                        "0/2--2/2--2/3--3/4", "0/2--2/2--2/3--3/5")

  bridges <- connect_free_pieces("", 0, pieces)

  expect_equal(sort(bridges), sort(possible_bridges))

  bridges %>%
    find_strongest_bridge() %>%
    expect_equal("0/1--10/1--9/10") %>%
    compute_bridge_strength() %>%
    expect_equal(31)
})
