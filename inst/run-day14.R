library(adventofcode17)
input <- "ljoxqyyw"
bits <- generate_grid_hashes("ljoxqyyw")







ones <- bits %>% str_sum_ones()

chars <- bits %>% strsplit("") %>% unlist()

grid <- matrix(chars, nrow = 128, byrow = TRUE)
grid[grid == "1"] <- "one"

find_next_one <- function(grid) {
  ones <- which(grid == "one")[1]
  col <- (ones %/% 128) + 1
  row <- (ones %% 128) + 1
  c(row, col)
}

seed <- 1
while ("one" %in% grid) {
  grid <- flood_fill(grid, find_next_one(grid), seed)
  seed <- seed + 1
}

flood_fill <- function(grid, start, value) {
  cells_to_check <- list(start)

  while (length(cells_to_check) > 0) {
    message(cells_to_check[[1]])
    grid <- mark_grid(grid, cells_to_check[[1]], value)
    adjacent_cells <- find_neighboring_ones(grid, cells_to_check[[1]])
    cells_to_check <- unique(c(cells_to_check, adjacent_cells))
    cells_to_check[[1]] <- NULL
  }

  grid
}


find_neighboring_ones <- function(grid, cell) {
  adjacent <- neighbors(cell)
  values <- adjacent %>% lapply(look_up_cell, grid) %>% unlist()
  adjacent[values == "one"]
}

look_up_cell <- function(cell, grid) {
  grid[cell[1], cell[2]]
}

mark_grid <- function(grid, cell, value) {
  grid[cell[1], cell[2]] <- value
  grid
}


neighbors <- function(cell) {
  x <- c(-1,  0, 0, 1)
  y <- c( 0, -1, 1, 0)
  xs <- x + cell[1]
  ys <- y + cell[2]
  bad_x <- which(xs <= 0 | 128 < xs)
  bad_y <- which(ys <= 0 | 128 < ys)
  if (length(c(bad_x, bad_y)) != 0) {
    xs <- xs[-c(bad_x, bad_y)]
    ys <- ys[-c(bad_x, bad_y)]
  }
  Map(function(x, y) c(x, y), xs, ys)
}
