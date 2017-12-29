#' Day 19: A Series of Tubes
#'
#' [A Series of Tubes](http://adventofcode.com/2017/day/19)
#'
#' @name day19
#' @rdname day19
#' @details
#'
#' **Part One**
#'
#' Somehow, a network packet got lost
#' and ended up here. It's trying to follow a routing diagram (your puzzle
#' input), but it's confused about where to go.
#'
#' Its starting point is just off the top of the diagram. Lines (drawn with
#' `|`, `-`, and `+`) show the path it needs to take, starting by going
#' down onto the only line connected to the top of the diagram. It needs to
#' follow this path until it reaches the end (located somewhere within the
#' diagram) and stop there.
#'
#' Sometimes, the lines cross over each other; in these cases, it needs to
#' continue going the same direction, and only turn left or right when
#' there's no other option. In addition, someone has left *letters* on the
#' line; these also don't change its direction, but it can use them to keep
#' track of where it's been. For example:
#'
#'          |
#'          |  +--+
#'          A  |  C
#'      F---|----E|--+
#'          |  |  |  D
#'          +B-+  +--+
#'
#' Given this diagram, the packet needs to take the following path:
#'
#' -   Starting at the only line touching the top of the diagram, it must
#'     go down, pass through `A`, and continue onward to the first `+`.
#' -   Travel right, up, and right, passing through `B` in the process.
#' -   Continue down (collecting `C`), right, and up (collecting `D`).
#' -   Finally, go all the way left through `E` and stopping at `F`.
#'
#' Following the path to the end, the letters it sees on its path are
#' `ABCDEF`.
#'
#' The little packet looks up at you, hoping you can help it find the way.
#' *What letters will it see* (in the order it would see them) if it
#' follows the path? (The routing diagram is very wide; make sure you view
#' it without line wrapping.)
#'
#' **Part Two**
#'
#' The packet is curious how many steps it needs to go.
#'
#' For example, using the same routing diagram from the example above...
#'
#'          |
#'          |  +--+
#'          A  |  C
#'      F---|--|-E---+
#'          |  |  |  D
#'          +B-+  +--+
#'
#' ...the packet would go:
#'
#' -   `6` steps down (including the first line at the top of the diagram).
#' -   `3` steps right.
#' -   `4` steps up.
#' -   `3` steps right.
#' -   `4` steps down.
#' -   `3` steps right.
#' -   `2` steps up.
#' -   `13` steps left (including the `F` it stops on).
#'
#' This would result in a total of `38` steps.
#'
#' *How many steps* does the packet need to go?
#'
#' @export
#' @param board a board description
#' @examples
#' # The vector below has this grid line by line:
#' #     |
#' #     |  +--+
#' #     A  |  C
#' # F---|----E|--+
#' #     |  |  |  D
#' #     +B-+  +--+
#' grid <- c("    |", "    |  +--+", "    A  |  C", "F---|----E|--+",
#'           "    |  |  |  D", "    +B-+  +--+")
#'
#' grid %>%
#'   create_board() %>%
#'   collect_board_letters() %>%
#'   paste0(collapse = "")
#'
#' grid %>%
#'   create_board() %>%
#'   walk_board_from_start() %>%
#'   length()
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
#' @param strings a vector of strings describing a board
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

