library(adventofcode17)
bits <- generate_grid_hashes("ljoxqyyw")

p1 <- str_sum_ones(bits)
p2 <- count_grid_regions(bits)

stopifnot(p1 == 8316)
stopifnot(p2 == 1074)
