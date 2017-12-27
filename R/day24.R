


#' @export
find_strongest_bridge <- function(bridges) {
  strongest <- bridges %>%
    lapply(compute_bridge_strength) %>%
    unlist() %>%
    which.max()

  bridges[strongest]
}

#' @export
find_longest_bridge <- function(bridges) {
  longest <- unlist(lapply(bridges, compute_bridge_length))
  bridges[longest == max(longest)]
}

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

#' @export
compute_bridge_length <- function(bridge) {
  length(unlist(strsplit(bridge, "--")))
}


# merge_connector_pairs <- function(bridge) {
#   Reduce(merge_connector_pair, bridge)
# }
#
#
# merge_connector_pair <- function(p1, p2) {
#   d1 <- as_digits(p1)
#   d2 <- as_digits(p2)
#
#   if (d1[2] == d2[1]) {
#     paste0(p1, "/", d2[2])
#   } else {
#     paste0(p1, "/", d2[1])
#   }
# }

#' @export
connect_free_pieces <- function(piece, active_value, pieces) {
  matches <- Filter(function(x) has_digit(x, active_value), pieces)
  # Recursion! If there are no matches, return the piece. Otherwise, find the
  # matches for each of the current matches.
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
