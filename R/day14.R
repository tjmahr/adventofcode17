#' @export
generate_grid_hashes <- function(key, seq = 0:127) {
  paste0(key, "-", seq) %>%
    lapply(knot_hash) %>%
    lapply(convert_knot_hash_to_bits) %>%
    unlist()
}

#' @export
str_sum_ones <- function(string) {
  string %>%
    strsplit("") %>%
    unlist() %>%
    as.numeric() %>%
    sum()
}

convert_knot_hash_to_bits <- function(string) {
  string %>%
    strsplit("") %>%
    unlist() %>%
    strtoi(16) %>%
    lapply(int_to_bits) %>%
    unlist() %>%
    paste0(collapse = "")
}

int_to_bits <- function(x) {
  bits <- x %>% intToBits() %>% as.integer()
  bits[1:4] %>% rev() %>% paste0(collapse = "")
}

grid_to_binary <- function(xs) {
  chars <- xs %>% strsplit("") %>% unlist()
  bits <- ifelse(chars == "#", 1, 0)
  paste0(bits, collapse = "")
}

binary_to_grid <- function(xs) {
  chars <- xs %>% strsplit("") %>% unlist()
  bits <- ifelse(chars == "1", "#", ".")
  paste0(bits, collapse = "")
}


#' @export
count_grid_regions <- function(bits) {
  # Get coordinates of next "one" in matrix
  find_next_one <- function(grid, width = 128) {
    ones <- which(grid == "one")[1]
    locate_cell_position(grid, ones)
  }

  # Get the row, column of the nth item in a matrix
  locate_cell_position <- function(matrix, position) {
    col <- ((position - 1) %/% ncol(matrix)) + 1
    row <- ((position - 1) %% nrow(matrix)) + 1
    c(row, col)
  }

  # Set all adjacent "one"s to the same value
  flood_fill <- function(grid, start, value) {
    cells_to_check <- list(start)

    while (length(cells_to_check) > 0) {
      grid <- mark_cell(grid, cells_to_check[[1]], value)
      adjacent_cells <- find_neighboring_ones(grid, cells_to_check[[1]])
      cells_to_check <- unique(c(cells_to_check, adjacent_cells))
      cells_to_check[[1]] <- NULL
    }

    grid
  }

  # Find neighbors with a sentinel "one" value
  find_neighboring_ones <- function(grid, cell) {
    adjacent <- get_neighbors(cell)
    values <- adjacent %>% lapply(look_up_cell, grid) %>% unlist()
    adjacent[values == "one"]
  }

  # Get a cell's value
  look_up_cell <- function(cell, grid) {
    grid[cell[1], cell[2]]
  }

  # Update a cell
  mark_cell <- function(grid, cell, value) {
    grid[cell[1], cell[2]] <- value
    grid
  }

  # Get indices of a cell's neigbors
  get_neighbors <- function(cell, width = 128) {
    x <- c(-1,  0, 0, 1)
    y <- c( 0, -1, 1, 0)
    xs <- x + cell[1]
    ys <- y + cell[2]
    bad_x <- which(xs <= 0 | width < xs)
    bad_y <- which(ys <= 0 | width < ys)
    if (length(c(bad_x, bad_y)) != 0) {
      xs <- xs[-c(bad_x, bad_y)]
      ys <- ys[-c(bad_x, bad_y)]
    }
    Map(function(x, y) c(x, y), xs, ys)
  }

  chars <- bits %>%
    strsplit("") %>%
    unlist()

  grid <- matrix(chars, nrow = 128, byrow = TRUE)
  grid[grid == "1"] <- "one"

  seed <- 1
  while ("one" %in% grid) {
    grid <- flood_fill(grid, find_next_one(grid), seed)
    seed <- seed + 1
  }
  max(as.numeric(grid))
}
