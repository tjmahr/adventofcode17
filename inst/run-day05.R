library(adventofcode17)
x <- readLines("./inst/input05.txt")

p1 <- follow_day05a_instructions(x)
p2 <- follow_day05b_instructions(x)

stopifnot(p1 == 342669)
stopifnot(p2 == 25136209)
