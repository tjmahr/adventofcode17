#' Day 22: Sporifica Virus
#'
#' [Sporifica Virus](http://adventofcode.com/2017/day/22)
#'
#' @details
#'
#' **Part One**
#'
#' Diagnostics indicate that the local *grid computing cluster* has been
#' contaminated with the *Sporifica Virus*. The grid computing cluster is a
#' seemingly-infinite
#' two-dimensional grid of compute nodes. Each node is either *clean* or
#' *infected* by the virus.
#'
#' To [prevent
#' overloading](https://en.wikipedia.org/wiki/Morris_worm#The_mistake) the
#' nodes (which would render them useless to the virus) or detection by
#' system administrators, exactly one *virus carrier* moves through the
#' network, infecting or cleaning nodes as it moves. The virus carrier is
#' always located on a single node in the network (the *current node*) and
#' keeps track of the *direction* it is facing.
#'
#' To avoid detection, the virus carrier works in bursts; in each burst, it
#' *wakes up*, does some *work*, and goes back to *sleep*. The following
#' steps are all executed *in order* one time each burst:
#'
#' -   If the *current node* is *infected*, it turns to its *right*.
#'     Otherwise, it turns to its *left*. (Turning is done in-place; the
#'     *current node* does not change.)
#' -   If the *current node* is *clean*, it becomes *infected*. Otherwise,
#'     it becomes *cleaned*. (This is done *after* the node is considered
#'     for the purposes of changing direction.)
#' -   The virus carrier
#'     [moves](https://www.youtube.com/watch?v=2vj37yeQQHg) *forward* one
#'     node in the direction it is facing.
#'
#' Diagnostics have also provided a *map of the node infection status*
#' (your puzzle input). *Clean* nodes are shown as `.`; *infected* nodes
#' are shown as `#`. This map only shows the center of the grid; there are
#' many more nodes beyond those shown, but none of them are currently
#' infected.
#'
#' The virus carrier begins in the middle of the map facing *up*.
#'
#' For example, suppose you are given a map like this:
#'
#'     ..#
#'     #..
#'     ...
#'
#' Then, the middle of the infinite grid looks like this, with the virus
#' carrier's position marked with `[ ]`:
#'
#'     . . . . . . . . .
#'     . . . . . . . . .
#'     . . . . . . . . .
#'     . . . . . # . . .
#'     . . . #[.]. . . .
#'     . . . . . . . . .
#'     . . . . . . . . .
#'     . . . . . . . . .
#'
#' The virus carrier is on a *clean* node, so it turns *left*, *infects*
#' the node, and moves left:
#'
#'     . . . . . . . . .
#'     . . . . . . . . .
#'     . . . . . . . . .
#'     . . . . . # . . .
#'     . . .[#]# . . . .
#'     . . . . . . . . .
#'     . . . . . . . . .
#'     . . . . . . . . .
#'
#' The virus carrier is on an *infected* node, so it turns *right*,
#' *cleans* the node, and moves up:
#'
#'     . . . . . . . . .
#'     . . . . . . . . .
#'     . . . . . . . . .
#'     . . .[.]. # . . .
#'     . . . . # . . . .
#'     . . . . . . . . .
#'     . . . . . . . . .
#'     . . . . . . . . .
#'
#' Four times in a row, the virus carrier finds a *clean*, *infects* it,
#' turns *left*, and moves forward, ending in the same place and still
#' facing up:
#'
#'     . . . . . . . . .
#'     . . . . . . . . .
#'     . . . . . . . . .
#'     . . #[#]. # . . .
#'     . . # # # . . . .
#'     . . . . . . . . .
#'     . . . . . . . . .
#'     . . . . . . . . .
#'
#' Now on the same node as before, it sees an infection, which causes it to
#' turn *right*, *clean* the node, and move forward:
#'
#'     . . . . . . . . .
#'     . . . . . . . . .
#'     . . . . . . . . .
#'     . . # .[.]# . . .
#'     . . # # # . . . .
#'     . . . . . . . . .
#'     . . . . . . . . .
#'     . . . . . . . . .
#'
#' After the above actions, a total of `7` bursts of activity had taken
#' place. Of them, `5` bursts of activity caused an infection.
#'
#' After a total of `70`, the grid looks like this, with the virus carrier
#' facing up:
#'
#'     . . . . . # # . .
#'     . . . . # . . # .
#'     . . . # . . . . #
#'     . . # . #[.]. . #
#'     . . # . # . . # .
#'     . . . . . # # . .
#'     . . . . . . . . .
#'     . . . . . . . . .
#'
#' By this time, `41` bursts of activity caused an infection (though most
#' of those nodes have since been cleaned).
#'
#' After a total of `10000` bursts of activity, `5587` bursts will have
#' caused an infection.
#'
#' Given your actual map, after `10000` bursts of activity, *how many
#' bursts cause a node to become infected*? (Do not count nodes that begin
#' infected.)
#'
#' **Part Two**
#'
#' As you go to remove the virus from the infected nodes, it *evolves* to
#' resist your attempt.
#'
#' Now, before it infects a clean node, it will *weaken* it to disable your
#' defenses. If it encounters an infected node, it will instead *flag* the
#' node to be cleaned in the future. So:
#'
#' -   *Clean* nodes become *weakened*.
#' -   *Weakened* nodes become *infected*.
#' -   *Infected* nodes become *flagged*.
#' -   *Flagged* nodes become *clean*.
#'
#' Every node is always in exactly one of the above states.
#'
#' The virus carrier still functions in a similar way, but now uses the
#' following logic during its bursts of action:
#'
#' -   Decide which way to turn based on the *current node*:
#'     -   If it is *clean*, it turns *left*.
#'     -   If it is *weakened*, it does *not* turn, and will continue
#'         moving in the same direction.
#'     -   If it is *infected*, it turns *right*.
#'     -   If it is *flagged*, it *reverses* direction, and will go back
#'         the way it came.
#' -   Modify the state of the *current node*, as described above.
#' -   The virus carrier moves *forward* one node in the direction it is
#'     facing.
#'
#' Start with the same map (still using `.` for *clean* and `#` for
#' infected) and still with the virus carrier starting in the middle and
#' facing *up*.
#'
#' Using the same initial state as the previous example, and drawing
#' *weakened* as `W` and *flagged* as `F`, the middle of the infinite grid
#' looks like this, with the virus carrier's position again marked with
#' `[ ]`:
#'
#'     . . . . . . . . .
#'     . . . . . . . . .
#'     . . . . . . . . .
#'     . . . . . # . . .
#'     . . . #[.]. . . .
#'     . . . . . . . . .
#'     . . . . . . . . .
#'     . . . . . . . . .
#'
#' This is the same as before, since no initial nodes are *weakened* or
#' *flagged*. The virus carrier is on a clean node, so it still turns left,
#' instead *weakens* the node, and moves left:
#'
#'     . . . . . . . . .
#'     . . . . . . . . .
#'     . . . . . . . . .
#'     . . . . . # . . .
#'     . . .[#]W . . . .
#'     . . . . . . . . .
#'     . . . . . . . . .
#'     . . . . . . . . .
#'
#' The virus carrier is on an infected node, so it still turns right,
#' instead *flags* the node, and moves up:
#'
#'     . . . . . . . . .
#'     . . . . . . . . .
#'     . . . . . . . . .
#'     . . .[.]. # . . .
#'     . . . F W . . . .
#'     . . . . . . . . .
#'     . . . . . . . . .
#'     . . . . . . . . .
#'
#' This process repeats three more times, ending on the previously-flagged
#' node and facing right:
#'
#'     . . . . . . . . .
#'     . . . . . . . . .
#'     . . . . . . . . .
#'     . . W W . # . . .
#'     . . W[F]W . . . .
#'     . . . . . . . . .
#'     . . . . . . . . .
#'     . . . . . . . . .
#'
#' Finding a flagged node, it reverses direction and *cleans* the node:
#'
#'     . . . . . . . . .
#'     . . . . . . . . .
#'     . . . . . . . . .
#'     . . W W . # . . .
#'     . .[W]. W . . . .
#'     . . . . . . . . .
#'     . . . . . . . . .
#'     . . . . . . . . .
#'
#' The *weakened* node becomes infected, and it continues in the same
#' direction:
#'
#'     . . . . . . . . .
#'     . . . . . . . . .
#'     . . . . . . . . .
#'     . . W W . # . . .
#'     .[.]# . W . . . .
#'     . . . . . . . . .
#'     . . . . . . . . .
#'     . . . . . . . . .
#'
#' Of the first `100` bursts, `26` will result in *infection*.
#' Unfortunately, another feature of this evolved virus is *speed*; of the
#' first `10000000` bursts, `2511944` will result in *infection*.
#'
#' Given your actual map, after `10000000` bursts of activity, *how many
#' bursts cause a node to become infected*? (Do not count nodes that begin
#' infected.)
#'
#' @rdname day22
#' @export
#' @param start starting start of grid (one string per row)
#' @examples
#' start <- "..#\n#..\n..."
#' grid1 <- start %>% read_text_lines() %>% virus_grid()
#' grid1$step()
#' grid2 <- start %>% read_text_lines() %>% evolved_virus_grid()
#' grid2$step()
virus_grid <- function(start) {
  chars <- start %>% strsplit("") %>% unlist()
  rows <- start %>% length()
  cols <- start %>% getElement(1) %>% nchar()
  x_position <- seq_len(cols) %>% median()
  y_position <- seq_len(rows) %>% median()

  grid <- matrix(chars, nrow = rows, ncol = cols, byrow = TRUE)

  cursor <- list(
    x = x_position,
    y = y_position,
    angle = pi / 2
  )

  infect_count <- 0

  # Grow a grid n rows or column. Negative values grow leftwards or upwards.
  expand_grid <- function(unit, n_units) {
    n <- abs(n_units)

    if (unit == "row") {
      ndim <- ncol
      n_arg <- list(nrow = n)
      fbind <- if (sign(n_units) == 1) rbind_post else rbind_pre
    } else {
      ndim <- nrow
      n_arg <- list(ncol = n)
      fbind <- if (sign(n_units) == 1) cbind_post else cbind_pre
    }

    one_dim <- rep(".", ndim(grid))
    args <- c(list(data = rep(one_dim, n)), n_arg)
    new_dims <- do.call(matrix, args)

    grid <<- fbind(grid, new_dims)
  }

  # Shortcuts to grow the grid and update the cursor position
  expand_left <- function(n = 1) {
    expand_grid("column", -1 * n)
    cursor$x <<- cursor$x + n
  }

  expand_up <- function(n = 1) {
    expand_grid("row", -1 * n)
    cursor$y <<- cursor$y + n
  }

  expand_right <- function(n = 1) expand_grid("column", n)
  expand_down <- function(n = 1) expand_grid("row", n)

  # Move the cursor along its current direction
  move_cursor <- function() {
    new_x <- cursor$x + round(cos(cursor$angle))
    new_y <- cursor$y - round(sin(cursor$angle))

    if (new_x == 0) expand_left()
    if (new_y == 0) expand_up()
    if (ncol(grid) < new_x) expand_right()
    if (nrow(grid) < new_y) expand_down()

    cursor$x <<- cursor$x + round(cos(cursor$angle))
    cursor$y <<- cursor$y - round(sin(cursor$angle))
    invisible(NULL)
  }


  turn_left <- function() cursor$angle <<- cursor$angle + (pi / 2)
  turn_right <- function() cursor$angle <<- cursor$angle - (pi / 2)

  on_infected <- function() grid[cursor$y, cursor$x] == "#"

  clean_node <- function() grid[cursor$y, cursor$x] <<- "."

  infect_node <- function() {
    grid[cursor$y, cursor$x] <<- "#"
    infect_count <<- infect_count + 1
  }

  # Run n bursts of activity
  step <- function(n = 1) {
    while (n != 0) {
      if (!on_infected()) {
        turn_left()
        infect_node()
      } else {
        turn_right()
        clean_node()
      }
      move_cursor()
      n <- n - 1
    }
  }

  # Getters
  get_infect_count <- function() infect_count
  get_grid <- function() grid
  get_cursor <- function() cursor

  # Print the local area around the cursor
  view_grid <- function(t = 1, r = 1, b = 1, l = 1) {
    if (cursor$x - l <= 0) expand_left(l)
    if (cursor$y - t <= 0) expand_up(t)
    if (ncol(grid) < cursor$x + r) expand_right(r)
    if (nrow(grid) < cursor$y + b) expand_down(b)

    x_range <- seq(from = cursor$x - l, to = cursor$x + r)
    y_range <- seq(from = cursor$y - t, to = cursor$y + b)
    print_grid <- grid
    value <- print_grid[cursor$y, cursor$x]
    print_grid[cursor$y, cursor$x] <- paste0("[", value, "]")
    print_grid[y_range, x_range]
  }

  expand_down(100)
  expand_up(100)
  expand_left(100)
  expand_right(100)

  list(
    get_infect_count = get_infect_count,
    get_grid = get_grid,
    get_cursor = get_cursor,
    step = step,
    view_grid = view_grid,
    move = move_cursor,
    turn_left = turn_left,
    turn_right = turn_right
  )

}

#' @rdname day22
#' @export
evolved_virus_grid <- function(start) {
  chars <- start %>% strsplit("") %>% unlist()
  rows <- start %>% length()
  cols <- start %>% getElement(1) %>% nchar()
  x_position <- seq_len(cols) %>% median()
  y_position <- seq_len(rows) %>% median()

  grid <- matrix(chars, nrow = rows, ncol = cols, byrow = TRUE)

  cursor <- list(
    x = x_position,
    y = y_position,
    angle = pi / 2
  )

  infect_count <- 0

  # Grow a grid n rows or column. Negative values grow leftwards or upwards.
  expand_grid <- function(unit, n_units) {
    n <- abs(n_units)

    if (unit == "row") {
      ndim <- ncol
      n_arg <- list(nrow = n)
      fbind <- if (sign(n_units) == 1) rbind_post else rbind_pre
    } else {
      ndim <- nrow
      n_arg <- list(ncol = n)
      fbind <- if (sign(n_units) == 1) cbind_post else cbind_pre
    }

    one_dim <- rep(".", ndim(grid))
    args <- c(list(data = rep(one_dim, n)), n_arg)
    new_dims <- do.call(matrix, args)

    grid <<- fbind(grid, new_dims)
  }

  # Shortcuts to grow the grid and update the cursor position
  expand_left <- function(n = 1) {
    expand_grid("column", -1 * n)
    cursor$x <<- cursor$x + n
  }

  expand_up <- function(n = 1) {
    expand_grid("row", -1 * n)
    cursor$y <<- cursor$y + n
  }

  expand_right <- function(n = 1) expand_grid("column", n)
  expand_down <- function(n = 1) expand_grid("row", n)

  # Move the cursor along its current direction
  move_cursor <- function() {
    new_x <- cursor$x + round(cos(cursor$angle))
    new_y <- cursor$y - round(sin(cursor$angle))

    if (new_x == 0) expand_left(10)
    if (new_y == 0) expand_up(10)
    if (ncol(grid) < new_x) expand_right(10)
    if (nrow(grid) < new_y) expand_down(10)

    cursor$x <<- cursor$x + round(cos(cursor$angle))
    cursor$y <<- cursor$y - round(sin(cursor$angle))
    invisible(NULL)
  }


  turn_left <- function() cursor$angle <<- cursor$angle + (pi / 2)
  turn_right <- function() cursor$angle <<- cursor$angle - (pi / 2)

  on_clean <- function() grid[cursor$y, cursor$x] == "."
  on_weakened <- function() grid[cursor$y, cursor$x] == "W"
  on_flagged <- function() grid[cursor$y, cursor$x] == "F"
  on_infected <- function() grid[cursor$y, cursor$x] == "#"

  clean_node <- function() grid[cursor$y, cursor$x] <<- "."
  weaken_node <- function() grid[cursor$y, cursor$x] <<- "W"
  flag_node <- function() grid[cursor$y, cursor$x] <<- "F"

  infect_node <- function() {
    grid[cursor$y, cursor$x] <<- "#"
    infect_count <<- infect_count + 1
  }

  # Run n bursts of activity
  step <- function(n = 1) {
    while (n != 0) {
      if (on_clean()) {
        weaken_node()
        turn_left()
      } else if (on_weakened()) {
        infect_node()
      } else if (on_flagged()) {
        clean_node()
        turn_left()
        turn_left()

      } else {
        flag_node()
        turn_right()
      }
      move_cursor()
      n <- n - 1
    }
  }

  # Getters
  get_infect_count <- function() infect_count
  get_grid <- function() grid
  get_cursor <- function() cursor

  # Print the local area around the cursor
  view_grid <- function(t = 1, r = 1, b = 1, l = 1) {
    if (cursor$x - l <= 0) expand_left(l)
    if (cursor$y - t <= 0) expand_up(t)
    if (ncol(grid) < cursor$x + r) expand_right(r)
    if (nrow(grid) < cursor$y + b) expand_down(b)

    x_range <- seq(from = cursor$x - l, to = cursor$x + r)
    y_range <- seq(from = cursor$y - t, to = cursor$y + b)
    print_grid <- grid
    value <- print_grid[cursor$y, cursor$x]
    print_grid[cursor$y, cursor$x] <- paste0("[", value, "]")
    print_grid[y_range, x_range]
  }

  expand_down(10)
  expand_up(10)
  expand_left(10)
  expand_right(10)

  list(
    get_infect_count = get_infect_count,
    get_grid = get_grid,
    get_cursor = get_cursor,
    step = step,
    view_grid = view_grid,
    move = move_cursor,
    turn_left = turn_left,
    turn_right = turn_right
  )

}

cbind_pre  <- function(x, y) cbind(y, x, deparse.level = 0)
cbind_post <- function(x, y) cbind(x, y, deparse.level = 0)
rbind_pre  <- function(x, y) rbind(y, x, deparse.level = 0)
rbind_post <- function(x, y) rbind(x, y, deparse.level = 0)
