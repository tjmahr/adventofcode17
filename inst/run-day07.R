library(adventofcode17)
lines <- readLines("./inst/input07.txt")

program <- find_root_program(lines)
stopifnot(program$name == "dgoocsw")
