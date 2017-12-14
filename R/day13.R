#' Day 13: Title
#'
#' [Title](http://adventofcode.com/2017/day/13)
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
#' @rdname day13
#' @export
locate_scanner <- function(range, time) {
  stopifnot(length(range) == 1, length(time) == 1)

  # Remove complete cycles
  # 1-2-3-2 has length 4
  # 1-2-3-4-5-4-3-2 has length 8
  substep <- (time - 1) %% (2 * (range - 1)) + 1

  # Determine which side to start on
  full_lengths <- substep %/% range
  starting_point <- ifelse(full_lengths %% 2 == 0, 0, range)

  # ...which way to go
  direction <- ifelse(full_lengths %% 2 == 0, 1, -1)

  # ...how many steps
  offset <- substep %% range

  if (range == 1) {
    1
  } else {
    starting_point + (direction * offset)
  }
}

parse_scanner_line <- function(x) {
  x <- strsplit(x, ": ") %>%
    unlist() %>%
    as.numeric()
  list(depth = x[1], range = x[2])
}

#' @rdname day13
#' @export
dayxx_b_stub <- function(x) {

}
