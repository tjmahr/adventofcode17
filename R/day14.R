




#' Day 14: Disk Defragmentation
#'
#' [Disk Defragmentation](http://adventofcode.com/2017/day/14)
#'
#' @details
#'
#' **Part One**
#'
#' Suddenly, a scheduled job activates the system's [disk
#' defragmenter](https://en.wikipedia.org/wiki/Defragmentation). Were the
#' situation different, you might [sit and watch it for a
#' while](https://www.youtube.com/watch?v=kPv1gQ5Rs8A&t=37), but today, you
#' just don't have that kind of time. It's soaking up valuable system
#' resources that are needed elsewhere, and so the only option is to help
#' it finish its task as soon as possible.
#'
#' The disk in question consists of a 128x128 grid; each square of the grid
#' is either *free* or *used*. On this disk, the state of the grid is
#' tracked by the bits in a sequence of [knot
#' hashes](http://adventofcode.com/2017/day/10).
#'
#' A total of 128 knot hashes are calculated, each corresponding to a
#' single row in the grid; each hash contains 128 bits which correspond to
#' individual grid squares. Each bit of a hash indicates whether that
#' square is *free* (`0`) or *used* (`1`).
#'
#' The hash inputs are a key string (your puzzle input), a dash, and a
#' number from `0` to `127` corresponding to the row. For example, if your
#' key string were `flqrgnkx`, then the first row would be given by the
#' bits of the knot hash of `flqrgnkx-0`, the second row from the bits of
#' the knot hash of `flqrgnkx-1`, and so on until the last row,
#' `flqrgnkx-127`.
#'
#' The output of a knot hash is traditionally represented by 32 hexadecimal
#' digits; each of these digits correspond to 4 bits, for a total of
#' `4 * 32 = 128` bits. To convert to bits, turn each hexadecimal digit to
#' its equivalent binary value, high-bit first: `0` becomes `0000`, `1`
#' becomes `0001`, `e` becomes `1110`, `f` becomes `1111`, and so on; a
#' hash that begins with `a0c2017...` in hexadecimal would begin with
#' `10100000110000100000000101110000...` in binary.
#'
#' Continuing this process, the *first 8 rows and columns* for key
#' `flqrgnkx` appear as follows, using `#` to denote used squares, and `.`
#' to denote free ones:
#'
#'     ##.#.#..-->
#'     .#.#.#.#
#'     ....#.#.
#'     #.#.##.#
#'     .##.#...
#'     ##..#..#
#'     .#...#..
#'     ##.#.##.-->
#'     |      |
#'     V      V
#'
#' In this example, `8108` squares are used across the entire 128x128 grid.
#'
#' Given your actual key string, *how many squares are used*?
#'
#' **Part Two**
#'
#' Now, all the defragmenter needs to know is the number
#' of *regions*. A region is a group of *used* squares that are all
#' *adjacent*, not including diagonals. Every used square is in exactly one
#' region: lone used squares form their own isolated regions, while several
#' adjacent squares all count as a single region.
#'
#' In the example above, the following nine regions are visible, each
#' marked with a distinct digit:
#'
#'     11.2.3..-->
#'     .1.2.3.4
#'     ....5.6.
#'     7.8.55.9
#'     .88.5...
#'     88..5..8
#'     .8...8..
#'     88.8.88.-->
#'     |      |
#'     V      V
#'
#' Of particular interest is the region marked `8`; while it does not
#' appear contiguous in this small view, all of the squares marked `8` are
#' connected when considering the whole 128x128 grid. In total, in this
#' example, `1242` regions are present.
#'
#' *How many regions* are present given your key string?
#'
#' @rdname day14
#' @export
generate_grid_hashes <- function(key, seq = 0:127) {
  paste0(key, "-", seq) %>%
    lapply(knot_hash) %>%
    lapply(convert_knot_hash_to_bits) %>%
    unlist()
}

#' @rdname day14
#' @export
str_sum_ones <- function(string) {
  # Count the ones in a string
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

# To mimic the illustration in the description and vice versa...
grid_to_binary <- function(xs) {
  chars <- xs %>% strsplit("") %>% unlist()
  bits <- ifelse(chars == "#", 1, 0)
  paste0(bits, collapse = "")
}

# To mimic the illustration in the description and vice versa...
binary_to_grid <- function(xs) {
  chars <- xs %>% strsplit("") %>% unlist()
  bits <- ifelse(chars == "1", "#", ".")
  paste0(bits, collapse = "")
}



#' @rdname day14
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

    # As long as there is a cell to mark, mark it and get add its unmarked
    # neighbors to the list of neighbors to mark
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
    # Offsets for cells to left, below, above, right
    x <- c(-1,  0, 0, 1)
    y <- c( 0, -1, 1, 0)
    xs <- x + cell[1]
    ys <- y + cell[2]

    # Don't go off the grid
    bad_x <- which(xs <= 0 | width < xs)
    bad_y <- which(ys <= 0 | width < ys)
    if (length(c(bad_x, bad_y)) != 0) {
      xs <- xs[-c(bad_x, bad_y)]
      ys <- ys[-c(bad_x, bad_y)]
    }

    Map(function(x, y) c(x, y), xs, ys)
  }

  # Convert the bit strings into a grid
  chars <- bits %>%
    strsplit("") %>%
    unlist()

  # Initialize the region-less 1's to the sentinel value "one"
  grid <- matrix(chars, nrow = 128, byrow = TRUE)
  grid[grid == "1"] <- "one"

  # Keep flood-filling new regions until there are no "one"s in grid
  seed <- 1
  while ("one" %in% grid) {
    grid <- flood_fill(grid, find_next_one(grid), seed)
    seed <- seed + 1
  }
  max(as.numeric(grid))
}
