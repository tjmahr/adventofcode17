
grid <-
"
    |
    |  +--+
    A  |  C
F---|----E|--+
    |  |  |  D
    +B-+  +--+
"


chars <- grid %>%
  strsplit("\n") %>%
  unlist() %>%
  keep_if(function(x) x != "") %>%
  strsplit("")

chars <- readLines("inst/input19.txt") %>% strsplit("")
rows <- length(chars)
lapply(chars, length) %>% unlist
board <- matrix(unlist(chars), nrow = rows, byrow = TRUE)

move_d <- function(cell) c(cell[1] + 1, cell[2])
move_u <- function(cell) c(cell[1] - 1, cell[2])
move_l <- function(cell) c(cell[1], cell[2] - 1)
move_r <- function(cell) c(cell[1], cell[2] + 1)

solve_turn <- function(board, cell, move) {
  possible_moves <- if (all(move(cell) == move_d(cell))) {
    list(move_l, move_r)
  } else if (all(move(cell) == move_u(cell))) {
    list(move_l, move_r)
  } else {
    list(move_u, move_d)
  }

  # Try both functions
  m1 <- possible_moves[[1]](cell)
  m2 <- possible_moves[[2]](cell)

  # Keep the second if the first hits a space or goes offboard
  if (offboard(board, m1) | look_up_cell(board, m1) == " ") {
    f <- possible_moves[[2]]
  } else {
    f <- possible_moves[[1]]
  }
  f
}

go <- function(board, move, start) {
  cell <- move(start)
  current_run <- c(list(start), list(current_cell))
  while (!offboard(board, move(cell))) {
    cell <- move(cell)
    current_run <- c(current_run, list(cell))
    # message(look_up_cell(board, cell))

    if (plus_cell(board, cell)) {
      move <- solve_turn(board, cell, move)
    }
  }
  current_run
}

plus_cell <- function(board, cell) {
  look_up_cell(board, cell) == "+"
}

offboard <- function(board, cell) {
  any(cell[1] <= 0, nrow(board) < cell[1],
      cell[2] <= 0, ncol(board) < cell[2])
}

# Get a cell's value
look_up_cell <- function(board, cell) {
  board[cell[1], cell[2]]
}

start <- c(1, which(board[1, ] == "|"))
moves <- go(board, move_d, start)
letters <- moves %>%
  lapply(function(x) look_up_cell(board, x)) %>%
  keep_if(function(x) x %in% LETTERS) %>%
  unlist()
