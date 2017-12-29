#' Day 11: Hex Ed
#'
#' [Hex Ed](http://adventofcode.com/2017/day/11)
#'
#' @name day11
#' @rdname day11
#' @details
#'
#' **Part One**
#'
#' Crossing the bridge, you've barely reached the other side of the stream
#' when a program comes up to you, clearly in distress. "It's my child
#' process," she says, "he's gotten lost in an infinite grid!"
#'
#' Fortunately for her, you have plenty of experience with infinite grids.
#'
#' Unfortunately for you, it's a [hex
#' grid](https://en.wikipedia.org/wiki/Hexagonal_tiling).
#'
#' The hexagons ("hexes") in this grid are aligned such that adjacent hexes can
#' be found to the north, northeast, southeast, south, southwest, and northwest:
#'
#'       \ n  /
#'     nw +--+ ne
#'       /    \
#'     -+      +-
#'       \    /
#'     sw +--+ se
#'       / s  \
#'
#' You have the path the child process took. Starting where he started, you
#' need to determine the fewest number of steps required to reach him. (A
#' "step" means to move from the hex you are in to any adjacent hex.)
#'
#' For example:
#'
#' -   `ne,ne,ne` is `3` steps away.
#' -   `ne,ne,sw,sw` is `0` steps away (back where you started).
#' -   `ne,ne,s,s` is `2` steps away (`se,se`).
#' -   `se,sw,se,sw,sw` is `3` steps away (`s,s,sw`).
#'
#' **Part Two**
#'
#' *How many steps away* is the *furthest* he ever got from his starting
#' position?
#'
#' @export
#' @param steps a sequence of hexagon directions
#' @examples
#' hexagon_distance(c("se", "sw", "se", "sw", "sw"))
#' find_longest_hexagon_distance(c("se", "sw", "se", "sw", "sw"))
hexagon_distance <- function(steps) {
  distances <- steps_to_distance(steps)
  reduce_me <- distances %>% as.list()

  x_reduce_steps <- character(0)
  y_reduce_steps <- character(0)

  # Find minimum number of steps to get x to 0
  if (reduce_me$x != 0) {
    ew <- ifelse(reduce_me$x > 0, "e", "w")
    ew_ones <- rep(ew, abs(reduce_me$x / .5))
    x_reduce_steps <- character(length(ew_ones))

    # Decide whether to go north w/e or south w/e based on north-south distance
    for (move_i in seq_along(ew_ones)) {

      ns <- ifelse(reduce_me$y >= 0, "n", "s")
      x_reduce_steps[move_i] <- paste0(ns, ew_ones[move_i])
      distances <- distances - steps_to_distance(x_reduce_steps[move_i])
      reduce_me <- as.list(distances)
    }
  }

  # Find minimum number of steps to get y to 0
  if (reduce_me$y != 0) {
    ns <- ifelse(reduce_me$y > 0, "n", "s")
    y_reduce_steps <- rep(ns, abs(reduce_me$y))
  }

  list(
    n_x_steps = length(x_reduce_steps),
    n_y_steps = length(y_reduce_steps),
    x_steps = x_reduce_steps,
    y_steps = y_reduce_steps)
}


#' @rdname day11
#' @export
find_longest_hexagon_distance <- function(steps) {
  saved_steps <- character(0)
  distances <- integer(0)

  # Compute the minimal hexagon distance after each step
  for (step in steps) {
    current_distance <- hexagon_distance(c(saved_steps, step))
    saved_steps <- c(current_distance$x_steps, current_distance$y_steps)
    distances <- c(distances, length(saved_steps))
  }
  max(distances)
}

# Add up distances accumulated by a series of hexagon steps
steps_to_distance <- function(steps) {
  mapping <- matrix(c(
     0.0,  1.0,
     0.0, -1.0,
     0.5,  0.5,
    -0.5,  0.5,
     0.5, -0.5,
    -0.5, -0.5), ncol = 2, byrow = TRUE)

  colnames(mapping) <- c("x", "y")
  rownames(mapping) <- c("n", "s", "ne", "nw", "se", "sw")
  colSums(mapping[steps, , drop = FALSE])
}
