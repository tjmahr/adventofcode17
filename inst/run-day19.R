library(adventofcode17)

grid_lines <- readLines("./inst/input19.txt")
board <- create_board(grid_lines)
path <- collect_board_letters(board)
path_word <- paste0(path, collapse = "")
stopifnot(path_word == aoc17_solutions$day19a)

path <- walk_board_from_start(board)
stopifnot(length(path) == aoc17_solutions$day19b)
