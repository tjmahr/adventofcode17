#' Day 16: Permutation Promenade
#'
#' [Permutation Promenade](http://adventofcode.com/2017/day/16)
#'
#' @details
#'
#' **Part One**
#'
#' You come upon a very unusual sight; a group of programs here appear to
#' be [dancing](https://www.youtube.com/watch?v=lyZQPjUT5B4&t=53).
#'
#' There are sixteen programs in total, named `a` through `p`. They start
#' by standing in a line: `a` stands
#' in position `0`, `b` stands in position `1`, and so on until `p`, which
#' stands in position `15`.
#'
#' The programs' *dance* consists of a sequence of *dance moves*:
#'
#' -   *Spin*, written `sX`, makes `X` programs move from the end to the
#'     front, but maintain their order otherwise. (For example, `s3` on
#'     `abcde` produces `cdeab`).
#' -   *Exchange*, written `xA/B`, makes the programs at positions `A` and
#'     `B` swap places.
#' -   *Partner*, written `pA/B`, makes the programs named `A` and `B` swap
#'     places.
#'
#' For example, with only five programs standing in a line (`abcde`), they
#' could do the following dance:
#'
#' -   `s1`, a spin of size `1`: `eabcd`.
#' -   `x3/4`, swapping the last two programs: `eabdc`.
#' -   `pe/b`, swapping programs `e` and `b`: `baedc`.
#'
#' After finishing their dance, the programs end up in order `baedc`.
#'
#' You watch the dance for a while and record their dance moves (your
#' puzzle input). *In what order are the programs standing* after their
#' dance?
#'
#' **Part Two**
#'
#' Now that you're starting to get a feel for the dance moves, you turn
#' your attention to *the dance as a whole*.
#'
#' Keeping the positions they ended up in from their previous dance, the
#' programs perform it again and again: including the first dance, a total
#' of *one billion* (`1000000000`) times.
#'
#' In the example above, their second dance would *begin* with the order
#' `baedc`, and use the same dance moves:
#'
#' -   `s1`, a spin of size `1`: `cbaed`.
#' -   `x3/4`, swapping the last two programs: `cbade`.
#' -   `pe/b`, swapping programs `e` and `b`: `ceadb`.
#'
#' *In what order are the programs standing* after their billion dances?
#'
#' @rdname day16
#' @export
dance <- function(programs, moves) {
  x <- programs %>%
    strsplit("") %>%
    unlist()

  # Convert the dance moves into a list of R expressions
  move_list <- moves %>%
    strsplit(",") %>%
    unlist() %>%
    lapply(parse_dance_move) %>%
    lapply(rlang::get_expr)

  # Evaluate each of the expressions
  for (move in move_list) x <- rlang::eval_bare(move)
  x
}

#' @rdname day16
#' @export
dance_a_billion_times <- function(programs, moves) {
  x <- programs %>%
    strsplit("") %>%
    unlist()

  move_list <- moves %>%
    strsplit(",") %>%
    unlist() %>%
    lapply(parse_dance_move) %>%
    lapply(rlang::get_expr)

  # Don't really run for a billion cycles! Just dance until the order of players
  # from step 1 is repeated.
  initial_state <- x
  times_done <- 0
  cycle_found <- FALSE

  while (!cycle_found) {
    for (move in move_list) x <- rlang::eval_bare(move)
    times_done <- times_done + 1
    if (times_done != 1 && all(x == initial_state)) {
      cycle_found <- TRUE
    }
  }

  # Figure out where in the cycle the billionth step is
  extra_steps <- 1000000000 %% times_done

  # Dance until that point
  x <- initial_state
  for (i in seq_len(extra_steps)) {
    for (move in move_list) x <- rlang::eval_bare(move)
  }

  x
}

# The dance moves
spin <- function(x, number) {
  c(tail(x, number), head(x, -number))
}

exchange <- function(x, position1, position2) {
  x2 <- x
  x[position1 + 1] <- x2[position2 + 1]
  x[position2 + 1] <- x2[position1 + 1]
  x
}

partner <- function(x, name1, name2) {
  x1 <- which(x == name1) - 1
  x2 <- which(x == name2) - 1
  exchange(x, x1, x2)
}

# Drop the first character of a string
str_rest <- function(string) substr(string, 2, nchar(string))

# Convert a dance move into an R expression
parse_dance_move <- function(string) {
  if (substr(string, 1, 1) == "s") {
    n <- as.numeric(str_rest(string))
    q <- quo(spin(x, !! n))
  } else if (substr(string, 1, 1) == "x") {
    n <- as.numeric(unlist(strsplit(str_rest(string), "/")))
    q <- quo(exchange(x, !! n[1], !! n[2]))
  } else if (substr(string, 1, 1) == "p") {
    y <- unlist(strsplit(str_rest(string), "/"))
    q <- quo(partner(x, !! y[1], !! y[2]))
  }
  q
}

