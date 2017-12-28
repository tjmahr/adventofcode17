#' Day 03: Spiral Memory
#'
#' [Spiral Memory](http://adventofcode.com/2017/day/3)
#'
#' @details
#'
#' **Part One**
#'
#' You come across an experimental new kind of memory stored on an
#' infinite two-dimensional grid.
#'
#' Each square on the grid is allocated in a spiral pattern starting at a
#' location marked `1` and then counting up while spiraling outward. For
#' example, the first few squares are allocated like this:
#'
#'     17  16  15  14  13
#'     18   5   4   3  12
#'     19   6   1   2  11
#'     20   7   8   9  10
#'     21  22  23---> ...
#'
#' While this is very space-efficient (no squares are skipped), requested
#' data must be carried back to square `1` (the location of the only access
#' port for this memory system) by programs that can only move up, down,
#' left, or right. They always take the shortest path: the [Manhattan
#' Distance](https://en.wikipedia.org/wiki/Taxicab_geometry) between the
#' location of the data and square `1`.
#'
#' For example:
#'
#' -   Data from square `1` is carried `0` steps, since it's at the access
#'     port.
#' -   Data from square `12` is carried `3` steps, such as: down, left,
#'     left.
#' -   Data from square `23` is carried only `2` steps: up twice.
#' -   Data from square `1024` must be carried `31` steps.
#'
#' *How many steps* are required to carry the data from the square
#' identified in your puzzle input all the way to the access port?
#'
#' **Part Two**
#'
#' As a stress test on the system, the programs here clear the grid and
#' then store the value `1` in square `1`. Then, in the same allocation
#' order as shown above, they store the sum of the values in all adjacent
#' squares, including diagonals.
#'
#' So, the first few squares' values are chosen as follows:
#'
#' -   Square `1` starts with the value `1`.
#' -   Square `2` has only one adjacent filled square (with value `1`), so
#'     it also stores `1`.
#' -   Square `3` has both of the above squares as neighbors and stores the
#'     sum of their values, `2`.
#' -   Square `4` has all three of the aforementioned squares as neighbors
#'     and stores the sum of their values, `4`.
#' -   Square `5` only has the first and fourth squares as neighbors, so it
#'     gets the value `5`.
#'
#' Once a square is written, its value does not change. Therefore, the
#' first few squares would receive the following values:
#'
#'     147  142  133  122   59
#'     304    5    4    2   57
#'     330   10    1    1   54
#'     351   11   23   25   26
#'     362  747  806--->   ...
#'
#' What is the *first value written* that is *larger* than your puzzle
#' input?
#'
#' @param x a target number for a spiral
#' @rdname day03
#' @export
#' @examples
#' spiral_distance(190)
#'
#' find_first_spiral_step_bigger_than_target(190)
spiral_distance <- function(x) {
  coords <- find_coordinate_of_number(x)
  abs(coords$x) + abs(coords$y)
}

# Find taxicab coordinate of numbers in a spiral
find_coordinate_of_number <- function(x) {
  # I wanted to avoid creating the whole spiral up to the input value. My
  # strategy was to use the fact that the spiral is made of rings: ring 0 has 1,
  # ring 1 has 2:9, ring 2 has 10:25, etc. The bottom edge of the ring has an
  # odd number of elements. Ring 0 has 1 item, ring 1 has 3 items, ring 3 has 5
  # items. The largest number in each ring is equal to the width squared. I use
  # these facts to determine which ring contains a number. Then once I know
  # which ring has an item, I make a table with the coordinates of every number
  # in that ring by following the spiral backwards from the largest number.
  ring_df <- x %>%
    find_ring_of_number() %>%
    enumerate_ring_items()

  ring_df[ring_df$i == x, ]
}

# Find which ring of the spiral contains a number
find_ring_of_number <- function(x) {
  if (x == 1) return(0)
  # Create a sequence of odd numbers for the width of the bottom edge
  odds <- seq(1, ceiling(sqrt(x)) + 2, by = 2)
  # Biggest item in each ring is width squared. The smallest item in each next
  # ring is the biggest item + 1. Start of ring 3 = (max of ring 2) + 1
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


#' @rdname day03
#' @export
find_first_spiral_step_bigger_than_target <- function(target) {
  # Okay, I really do have to make the spiral now.
  move_df <- create_move_df()

  # Get values along the next 90-degree bend until we find a number larger than
  # target
  while(max(move_df$value) < target) {
    move_df <- move_df %>%
      follow_next_bend() %>%
      update_move_df_values()
  }
  values_larger_than_target <- move_df$value[target <= move_df$value]
  min(values_larger_than_target)
}

# To walk around the spiral, we make a series a longer 90-degree turns
#
# bend 1: > ^
# bend 2: << vv
# bend 3: >>> ^^^
# bend 4: <<<< vvvv
# bend 5: >>>>> ^^^^^
# etc.
#
# So we can use these to generate a series of x,y changes.

determine_bend_steps <- function(bend_num) {
  is_rightward <- (bend_num %% 2) == 1
  x_move <- if (is_rightward) 1 else -1
  y_move <- if (is_rightward) 1 else -1

  data.frame(
    bend = bend_num,
    x_diff = c(rep(x_move, bend_num), rep(0, bend_num)),
    y_diff = c(rep(0, bend_num), rep(y_move, bend_num)))

}

create_move_df <- function() {
  data.frame(value = 1, move = 0, bend = 0, x = 0, y = 0)
}

# Add x-y values of next bend
follow_next_bend <- function(move_df) {
  last_row <- move_df[nrow(move_df), ]

  next_steps <- determine_bend_steps(last_row$bend + 1)
  new_move <- last_row$move + seq_len(nrow(next_steps))
  new_bend <- last_row$bend + 1
  new_x <- cumsum(c(last_row$x, next_steps$x_diff))[-1]
  new_y <- cumsum(c(last_row$y, next_steps$y_diff))[-1]

  moves <- data.frame(
    value = 0,
    move = new_move,
    bend = new_bend,
    x = new_x,
    y = new_y)

  rbind(move_df, moves)
}

# Replace any 0's with sum of neighbors
update_move_df_values <- function(move_df) {
  while (any_move_df_rows_to_update(move_df)) {
    move_df <- update_next_move_df_row(move_df)
  }
  move_df
}

# Are there any zeroes?
any_move_df_rows_to_update <- function(move_df) {
  any(move_df$value == 0)
}

# Update first 0 value with sum of neighbors
update_next_move_df_row <- function(move_df) {
  curr_row <- find_next_move_df_row_to_update(move_df)
  x <- curr_row$x
  y <- curr_row$y
  # Find neighboring units but exclude self
  neighbors <- move_df[(move_df$x %in% c(x + -1:1)) &
                         (move_df$y %in% c(y + -1:1)), ]
  neighbors <- neighbors[neighbors$move != curr_row$move, ]
  move_df[move_df$move == curr_row$move, "value"] <- sum(neighbors$value)
  move_df
}

find_next_move_df_row_to_update <- function(move_df) {
  next_to_update <- min(which(move_df$value == 0))
  move_df[next_to_update, ]
}


