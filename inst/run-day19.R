library(adventofcode17)

grid_lines <- readLines("./inst/input19.txt")
board <- create_board(grid_lines)
path <- collect_board_letters(board)
path_word <-  paste0(path, collapse = "")

stopifnot(path_word == "EOCZQMURF")

path <- walk_board_from_start(board)
stopifnot(length(path) == 16312)
