library(adventofcode17)
pipes <- readLines("./inst/input12.txt")

neighborhood <- search_pipes_from_zero(pipes)
groups <- count_pipe_groups(pipes)

stopifnot(length(neighborhood) == 306)
stopifnot(groups == 200)
