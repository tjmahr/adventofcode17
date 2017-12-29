#' Day 21: Fractal Art
#'
#' [Fractal Art](http://adventofcode.com/2017/day/21)
#'
#' @name day21
#' @rdname day21
#' @details
#'
#' **Part One**
#'
#' You find a program trying to generate some art. It uses a strange
#' process that involves repeatedly enhancing the detail
#' of an image through a set of rules.
#'
#' The image consists of a two-dimensional square grid of pixels that are
#' either on (`#`) or off (`.`). The program always begins with this
#' pattern:
#'
#'     .#.
#'     ..#
#'     ###
#'
#' Because the pattern is both `3` pixels wide and `3` pixels tall, it is
#' said to have a *size* of `3`.
#'
#' Then, the program repeats the following process:
#'
#' -   If the size is evenly divisible by `2`, break the pixels up into
#'     `2x2` squares, and convert each `2x2` square into a `3x3` square by
#'     following the corresponding *enhancement rule*.
#' -   Otherwise, the size is evenly divisible by `3`; break the pixels up
#'     into `3x3` squares, and convert each `3x3` square into a `4x4`
#'     square by following the corresponding *enhancement rule*.
#'
#' Because each square of pixels is replaced by a larger one, the image
#' gains pixels and so its *size* increases.
#'
#' The artist's book of enhancement rules is nearby (your puzzle input);
#' however, it seems to be missing rules. The artist explains that
#' sometimes, one must *rotate* or *flip* the input pattern to find a
#' match. (Never rotate or flip the output pattern, though.) Each pattern
#' is written concisely: rows are listed as single units, ordered top-down,
#' and separated by slashes. For example, the following rules correspond to
#' the adjacent patterns:
#'
#'     ../.#  =  ..
#'               .#
#'
#'                     .#.
#'     .#./..#/###  =  ..#
#'                     ###
#'
#'                             #..#
#'     #..#/..../#..#/.##.  =  ....
#'                             #..#
#'                             .##.
#'
#' When searching for a rule to use, rotate and flip the pattern as
#' necessary. For example, all of the following patterns match the same
#' rule:
#'
#'     .#.   .#.   #..   ###
#'     ..#   #..   #.#   ..#
#'     ###   ###   ##.   .#.
#'
#' Suppose the book contained the following two rules:
#'
#'     ../.# => ##./#../...
#'     .#./..#/### => #..#/..../..../#..#
#'
#' As before, the program begins with this pattern:
#'
#'     .#.
#'     ..#
#'     ###
#'
#' The size of the grid (`3`) is not divisible by `2`, but it is divisible
#' by `3`. It divides evenly into a single square; the square matches the
#' second rule, which produces:
#'
#'     #..#
#'     ....
#'     ....
#'     #..#
#'
#' The size of this enhanced grid (`4`) is evenly divisible by `2`, so that
#' rule is used. It divides evenly into four squares:
#'
#'     #.|.#
#'     ..|..
#'     --+--
#'     ..|..
#'     #.|.#
#'
#' Each of these squares matches the same rule (`../.# => ##./#../...`),
#' three of which require some flipping and rotation to line up with the
#' rule. The output for the rule is the same in all four cases:
#'
#'     ##.|##.
#'     #..|#..
#'     ...|...
#'     ---+---
#'     ##.|##.
#'     #..|#..
#'     ...|...
#'
#' Finally, the squares are joined into a new grid:
#'
#'     ##.##.
#'     #..#..
#'     ......
#'     ##.##.
#'     #..#..
#'     ......
#'
#' Thus, after `2` iterations, the grid contains `12` pixels that are *on*.
#'
#' *How many pixels stay on* after `5` iterations?
#'
#' **Part Two**
#'
#' *How many pixels stay on* after `18` iterations?
#'
#' @export
#' @param m a matrix
#' rules <- c(
#'   "../.# => ##./#../...",
#'   ".#./..#/### => #..#/..../..../#..#")
#'
#' rules <- rules %>%
#'   lapply(expand_rule) %>%
#'   unlist()
#'
#' seed <- ".#./..#/###" %>%
#'   string_to_matrix()
#'
#' step1 <- enhance_matrix(seed, rules)
#'
#' matrix_to_string(step1)
#'
#' step2 <- enhance_matrix(step1, rules)
#' matrix_to_string(step2)
matrix_to_string <- function(m) {
  m %>%
    apply(1, paste0, collapse = "") %>%
    paste0(collapse = "/")
}

#' @rdname day21
#' @export
#' @param string a string description of a matrix
string_to_matrix <- function(string) {
  pattern <- string %>%
    strsplit("/") %>%
    unlist() %>%
    strsplit("") %>%
    unlist()

  matrix(pattern, nrow = sqrt(length(pattern)), byrow = TRUE)
}

#' @rdname day21
#' @export
#' @param rule_string a string describing a rule
expand_rule <- function(rule_string) {
  parts <- rule_string %>% strsplit(" => ") %>% unlist()
  aliases <- find_rule_aliases(parts[1])
  targets <- rep(parts[2], length(aliases))
  targets %>% as.list() %>% setNames(aliases)
}

#' @rdname day21
#' @export
#' @param rules rules for progressive enhancement
enhance_matrix <- function(m, rules) {
  if (nrow(m) %% 2 == 0) {
    div_by <- 2
  } else {
    div_by <- 3
  }

  row_seq_starts <- seq(1, nrow(m), by = div_by)
  row_seq_ends <- seq(div_by, nrow(m), by = div_by)
  col_seq_starts <- seq(1, ncol(m), by = div_by)
  col_seq_ends <- seq(div_by, ncol(m), by = div_by)

  rowwise <- Map(seq, row_seq_starts, row_seq_ends)
  colwise <- Map(seq, col_seq_starts, col_seq_ends)

  indices_of_submatrices <- cross(colwise, rowwise)

  sub_matrices <- Map(function(x, y) m[x, y],
                      indices_of_submatrices[[1]],
                      indices_of_submatrices[[2]])

  matrices <- sub_matrices %>%
    lapply(matrix_to_string) %>%
    lapply(function(string) unname(rules[string])) %>%
    lapply(string_to_matrix)

  # Assemble the new matrix one column at a time
  cols <- list()
  while (length(matrices) != 0) {
    cols <- c(cols, list(do.call(rbind, matrices[seq_along(colwise)])))
    matrices[seq_along(colwise)] <- NULL
  }

  do.call(cbind, cols)
}

cross <- function(x1, x2) {
  # Some sugar to name to columns in the dataframe
  x1 <- enquo(x1)
  x2 <- enquo(x2)
  x1_value <- rlang::eval_tidy(x1)
  x2_value <- rlang::eval_tidy(x2)

  both <- as.data.frame(expand.grid(x1_value, x2_value))
  attributes(both) <- NULL
  names(both) <- c(rlang::quo_name(x1), rlang::quo_name(x2))
  both
}

find_rule_aliases <- function(pattern) {
  m <- string_to_matrix(pattern)
  permutations <- c(m %>% generate_rotations(),
                    m %>% flip_h() %>% generate_rotations(),
                    m %>% flip_v() %>% generate_rotations(),
                    m %>% flip_h() %>% flip_v() %>% generate_rotations())

  permutations %>%
    lapply(matrix_to_string) %>%
    unique() %>%
    unlist()
}

generate_rotations <- function(m) {
  list(m,
       m %>% rotate(),
       m %>% rotate() %>% rotate(),
       m %>% rotate() %>% rotate() %>% rotate())
}

rotate <- function(m) t(apply(m, 2, rev))

flip_h <- function(m) {
  cells <- seq_len(nrow(m))
  m[rev(cells), cells]
}

flip_v <- function(m) {
  cells <- seq_len(nrow(m))
  m[cells, rev(cells)]
}
