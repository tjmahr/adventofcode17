
#' @export
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
