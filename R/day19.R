#' Day 19: Title
#'
#' [Title](http://adventofcode.com/2017/day/19)
#'
#' @details
#'
#' **Part One**
#'
#'
#'
#' **Part Two**
#'
#'
#' @rdname day19
#' @export
collect_board_letters <- function(board) {
  # Get path, convert path positions into grid symbols, keep just letters
  board %>%
    walk_board_from_start() %>%
    lapply(function(x) look_up_cell(board, x)) %>%
    keep_if(function(x) x %in% LETTERS) %>%
    unlist()
}

#' @rdname day19
#' @export
walk_board_from_start <- function(board) {
  start <- find_board_start(board)
  # We enter from top, so initial movement function is move_d()
  walk_board(board, move_d, start)
}

# Start on the "|" in the first row
find_board_start <- function(board) {
  c(1, which(board[1, ] == "|"))
}

# Follow the path until we go off the path
#
# This is probably the only interesting programming feature of my solution. I
# thought about having a "bearing" value that recorded which direction we are
# moving through the grid, having a move(cell, bearing) function that updated
# our location, and having the bearing variable update when we hit a plus sign.
# Instead, I just wrote four different move functions, and turning changes
# which move function is being used. No big philosophical motivations for this
# decision. It just seemed cleaner in my head to let the movement functions
# implicitly store the current direction of the path.
walk_board <- function(board, move, start) {
  # Keep track of moves in a list
  cell <- move(start)
  path <- c(list(start), list(cell))

  # Walk until we go offpath
  while (!offpath(board, move(cell))) {
    cell <- move(cell)
    path <- c(path, list(cell))

    # Change direction on "+" symbols
    if (plus_cell(board, cell)) {
      move <- solve_turn(board, cell, move)
    }
  }

  path
}

move_d <- function(cell) c(cell[1] + 1, cell[2])
move_u <- function(cell) c(cell[1] - 1, cell[2])
move_l <- function(cell) c(cell[1], cell[2] - 1)
move_r <- function(cell) c(cell[1], cell[2] + 1)

offpath <- function(board, cell) {
  any(cell[1] <= 0, nrow(board) < cell[1],
      cell[2] <= 0, ncol(board) < cell[2],
      look_up_cell(board, cell) == " ")
}

plus_cell <- function(board, cell) {
  look_up_cell(board, cell) == "+"
}

look_up_cell <- function(board, cell) {
  board[cell[1], cell[2]]
}

# Figure out which direction to go when we hit a plus
solve_turn <- function(board, cell, move) {
  # If we were going up/down, try left/right. Otherwise, try up/down.
  possible_moves <- if (identical(move, move_d)) {
    list(move_l, move_r)
  } else if (identical(move, move_u)) {
    list(move_l, move_r)
  } else {
    list(move_u, move_d)
  }

  # Try both moves
  m1 <- possible_moves[[1]](cell)
  m2 <- possible_moves[[2]](cell)

  # Keep the second move if first goes offpath
  if (offpath(board, m1)) {
    possible_moves[[2]]
  } else {
    possible_moves[[1]]
  }
}


#' @rdname day19
#' @export
create_board <- function(strings) {
  # Create a matrix from a vector of strings
  chars <- strings %>% pad_right() %>% strsplit("")
  matrix(unlist(chars), nrow = length(chars), byrow = TRUE)
}

# Sometimes RStudio or other programs delete trailing spaces from lines. Pad
# strings in a vector so they have the same number of characters.
pad_right <- function(strings) {
  padding <- max(nchar(strings)) - nchar(strings)
  pad_one <- function(string, padding) {
    tail <- paste0(rep(" ", padding), collapse = "")
    paste0(string, tail, collapse = "")
  }

  unlist(Map(pad_one, strings, padding), use.names = FALSE)
}




