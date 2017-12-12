#' Day 6: Title
#'
#' [Title](http://adventofcode.com/2017/day/6)
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
#' @rdname day06
#' @export
analyze_reallocations <- function(xs) {
  cycle <- 1
  history <- character()
  splat <- function(xs) paste0(xs, collapse = ",")

  # Write down vector in history and reallocate until we see a repeat
  while(splat(xs) %nin% history) {
    history[cycle] <- splat(xs)
    cycle <- cycle + 1
    xs <- xs %>% reallocate()
  }
  history[cycle] <- splat(xs)

  list(
    cycles_until_repeat = length(history) - 1,
    loop_length = which(history == splat(xs)) %>% diff()
  )
}

reallocate <- function(xs) {
  reallocation <- rep(0, length(xs))

  # Find the index after the maximum
  to_redistribute <- which.max(xs)
  starting_index <- wrap_around((to_redistribute + 1), length(xs))

  # Walk n steps around the vector and count visits to each index
  block_counts <- starting_index %>%
    seq(length.out = xs[to_redistribute]) %>%
    wrap_around(length(xs)) %>%
    table()

  # Add the number of visits to each index
  reallocation[as.numeric(names(block_counts))] <- as.vector(block_counts)
  xs[to_redistribute] <- 0
  xs + reallocation
}
