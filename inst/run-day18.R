library(adventofcode17)
commands <- readLines("./inst/input18.txt")

machine <- create_duet(commands)

while (length(machine$.messages) == 0) {
  machine$.eval_next()
}

stopifnot(machine$.messages == 1187)
