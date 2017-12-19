library(adventofcode17)
x <- readLines("./inst/input02.txt")

p1 <- checksum_range(x)
p2 <- checksum_evenly_divisible(x)


stopifnot(p1 == 45972)
stopifnot(p2 == 326)
