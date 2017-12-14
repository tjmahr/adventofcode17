context("test-day14.R")

test_that("getting bits of a knot hash", {

  "a0c2017..." %>%
    convert_knot_hash_to_bits() %>%
    substr(1, 32) %>%
    expect_equal("10100000110000100000000101110000")

  grid_samples <- "
   ##.#.#..
   .#.#.#.#
   ....#.#.
   #.#.##.#
   .##.#...
   ##..#..#
   .#...#..
   ##.#.##." %>%
    read_text_lines()

  test_grid <- "flqrgnkx" %>%
    generate_grid_hashes(0:7) %>%
    substr(1, 8) %>%
    lapply(binary_to_grid) %>%
    unlist() %>%
    expect_equal(grid_samples)

})
