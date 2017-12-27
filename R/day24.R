#' Day 24: Title
#'
#' [Title](http://adventofcode.com/2017/day/24)
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
#' @rdname day24
#' @export
find_strongest_bridge <- function(bridges) {
  strongest <- bridges %>%
    lapply(compute_bridge_strength) %>%
    unlist() %>%
    which.max()

  bridges[strongest]
}

#' @rdname day24
#' @export
find_longest_bridge <- function(bridges) {
  longest <- bridges %>%
    lapply(compute_bridge_length) %>%
    unlist()

  bridges[longest == max(longest)]
}

#' @rdname day24
#' @export
compute_bridge_strength <- function(bridge) {
  bridge %>%
    strsplit("--") %>%
    unlist() %>%
    strsplit("/") %>%
    unlist() %>%
    as.numeric() %>%
    sum()
}

#' @rdname day24
#' @export
compute_bridge_length <- function(bridge) {
  bridge %>%
    strsplit("--") %>%
    unlist() %>%
    length()
}

#' @export
connect_free_pieces <- function(piece, active_value, pieces) {
  # When finding matches for x/y, one of the sides (x or y) is "active". We want
  # to find other pieces with that same value.
  matches <- Filter(function(x) has_digit(x, active_value), pieces)

  # Recursion! If there are no matches, return the piece. Otherwise, merge the
  # piece with each of its matches, and repeat this process on each of merged
  # pieces.
  result <- if (length(matches) == 0) {
    piece
  } else {
    all_branches <- character(0)
    for (match_i in seq_along(matches)) {
      match <- matches[match_i]
      new_value <- drop_first_instance(as_digits(match), active_value)
      new_piece <- if (piece == "") match else paste0(piece, "--", match)
      branches <- Recall(
        piece = new_piece,
        active_value = new_value,
        pieces = drop_first_instance(pieces, match))
      all_branches <- c(all_branches, branches)
    }
    all_branches
  }
  result
}

drop_first_instance <- function(xs, y) {
  first <- which(xs == y)[1]
  xs[-first]
}

as_digits <- function(piece) {
  as.integer(unlist(strsplit(piece, "/")))
}

has_digit <- function(piece, value) {
  any(as_digits(piece) == value)
}
