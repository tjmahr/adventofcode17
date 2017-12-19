library(adventofcode17)
input <- "ljoxqyyw"
bits <- generate_grid_hashes("flqrgnkx")
bits <- generate_grid_hashes("ljoxqyyw")

ones <- str_sum_ones(bits)
stopifnot(ones == 8108)


n_regions <- count_grid_regions(bits)
stopifnot(n_regions == 1074)
