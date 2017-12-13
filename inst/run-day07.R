library(adventofcode17)
lines <- readLines("./inst/input07.txt")

program <- find_root_program(lines)
stopifnot(program$name == "dgoocsw")

imbalance <- find_program_imbalance(lines)
stopifnot(imbalance == 1275)
