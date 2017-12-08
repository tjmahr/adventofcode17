
#' Day 2: Spiral Memory
#'
#' Spiral Memory
#'
#' @details
#'
#' To quote the [website](http://adventofcode.com/2017/day/3)
#'
#' **Part One**
#'
#' You come across an experimental new kind of memory stored on an infinite
#' two-dimensional grid.
#'
#' Each square on the grid is allocated in a spiral pattern starting at a
#' location marked 1 and then counting up while spiraling outward. For example,
#' the first few squares are allocated like this:
#'
#' ```
#' 17  16  15  14  13
#' 18   5   4   3  12
#' 19   6   1   2  11
#' 20   7   8   9  10
#' 21  22  23---> ...
#' ```
#'
#' While this is very space-efficient (no squares are skipped), requested data
#' must be carried back to square 1 (the location of the only access port for
#' this memory system) by programs that can only move up, down, left, or right.
#' They always take the shortest path: the Manhattan Distance between the
#' location of the data and square 1.
#'
#' For example:
#'
#' - Data from square 1 is carried 0 steps, since it's at the access port.
#' - Data from square 12 is carried 3 steps, such as: down, left, left.
#' - Data from square 23 is carried only 2 steps: up twice.
#' - Data from square 1024 must be carried 31 steps.
#'
#'
#' How many steps are required to carry the data from the square identified in
#' your puzzle input all the way to the access port?
#'
#' **Part Two**
#'
#' ...
#'
#' @rdname day03
#' @export
spiral_distance <- function(x) {
  coords <- find_coordinate_of_number(x)
  abs(coords$x) + abs(coords$y)
}

#' @rdname day03
#' @export
stub_part_two <- function() {
  # to write after I solve part 1
}

# Find taxicab coordinate of numbers in a spiral
find_coordinate_of_number <- function(x) {
  ring_df <- x %>%
    find_ring_of_number() %>%
    enumerate_ring_items()

  ring_df[ring_df$i == x, ]
}

# Find which ring of the spiral contains a number
find_ring_of_number <- function(x) {
  if (x == 1) return(0)
  odds <- seq(1, ceiling(sqrt(x)) + 2, by = 2)
  ring_starts <- (odds ^ 2) + 1
  max(which((ring_starts) <= x))
}

# Make a table with coordinates of items in a spiral's ring
enumerate_ring_items <- function(ring_num) {
  if (ring_num == 0) {
    return(data.frame(i = 1, x = 0, y = 0))
  }

  odds <- seq(from = 1, by = 2, length.out = ring_num)
  ring_width <- max(odds) + 2

  # Maximum is number of items (area)
  # Minimum is area minus perimeter
  ring_end <- ring_width ^ 2
  num_items <- discrete_perimeter(ring_width)
  ring_start <- ring_end - num_items + 1

  # Last item in each ring is at (1, -1), (2, -2), etc. along a lower diagonal
  # line. So walk back from last item and store x coordinates.
  end_x <- ring_num
  x_bottom <- rev(seq(from = end_x, to = -end_x))
  x_left_side <- rep(x_bottom[1], ring_width - 2)
  x_top <- x_bottom
  x_right_side <- -x_left_side

  # But reorder from first item
  xs <- c(x_right_side, rev(x_top), x_left_side, x_bottom)

  # Walk back from last item
  y_bottom <- rep(-end_x, length(x_bottom))
  y_left_side <- rev(seq(from = end_x - 1, to = -end_x + 1))
  y_top <- -y_bottom
  y_right_side <- y_left_side

  # But reorder from first item
  ys <- c(y_right_side, y_top, rev(y_left_side), y_bottom)

  data.frame(
    i = seq(ring_start, ring_end),
    x = xs,
    y = ys)
}

# Number of items in a ring
discrete_perimeter <- function(x) {
  ifelse(x == 1, 1, (4 * x) - 4)
}

