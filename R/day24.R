#' Day 24: Electromagnetic Moat
#'
#' [Electromagnetic Moat](http://adventofcode.com/2017/day/24)
#'
#' @details
#'
#' **Part One**
#'
#' The CPU itself is a large, black building surrounded by a bottomless
#' pit. Enormous metal tubes extend outward from the side of the building
#' at regular intervals and descend down into the void. There's no way to
#' cross, but you need to get inside.
#'
#' No way, of course, other than building a *bridge* out of the magnetic
#' components strewn about nearby.
#'
#' Each component has two *ports*, one on each end. The ports come in all
#' different types, and only matching types can be connected. You take an
#' inventory of the components by their port types (your puzzle input).
#' Each port is identified by the number of *pins* it uses; more pins mean
#' a stronger connection for your bridge. A `3/7` component, for example,
#' has a type-`3` port on one side, and a type-`7` port on the other.
#'
#' Your side of the pit is metallic; a perfect surface to connect a
#' magnetic, *zero-pin port*. Because of this, the first port you use must
#' be of type `0`. It doesn't matter what type of port you end with; your
#' goal is just to make the bridge as strong as possible.
#'
#' The *strength* of a bridge is the sum of the port types in each
#' component. For example, if your bridge is made of components `0/3`,
#' `3/7`, and `7/4`, your bridge has a strength of `0+3 + 3+7 + 7+4 = 24`.
#'
#' For example, suppose you had the following components:
#'
#'     0/2
#'     2/2
#'     2/3
#'     3/4
#'     3/5
#'     0/1
#'     10/1
#'     9/10
#'
#' With them, you could make the following valid bridges:
#'
#' -   `0/1`
#' -   `0/1`--`10/1`
#' -   `0/1`--`10/1`--`9/10`
#' -   `0/2`
#' -   `0/2`--`2/3`
#' -   `0/2`--`2/3`--`3/4`
#' -   `0/2`--`2/3`--`3/5`
#' -   `0/2`--`2/2`
#' -   `0/2`--`2/2`--`2/3`
#' -   `0/2`--`2/2`--`2/3`--`3/4`
#' -   `0/2`--`2/2`--`2/3`--`3/5`
#'
#' (Note how, as shown by `10/1`, order of ports within a component doesn't
#' matter. However, you may only use each port on a component once.)
#'
#' Of these bridges, the *strongest* one is `0/1`--`10/1`--`9/10`; it has a
#' strength of `0+1 + 1+10 + 10+9 = 31`.
#'
#' *What is the strength of the strongest bridge you can make* with the
#' components you have available?
#'
#' **Part Two**
#'
#' The bridge you've built isn't long enough; you can't jump the rest of
#' the way.
#'
#' In the example above, there are two longest bridges:
#'
#' -   `0/2`--`2/2`--`2/3`--`3/4`
#' -   `0/2`--`2/2`--`2/3`--`3/5`
#'
#' Of them, the one which uses the `3/5` component is stronger; its
#' strength is `0+2 + 2+2 + 2+3 + 3+5 = 19`.
#'
#' *What is the strength of the longest bridge you can make?* If you can
#' make multiple bridges of the longest length, pick the *strongest* one.
#'
#' @rdname day24
#' @export
#'
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
