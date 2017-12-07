
#' Day 1: Inverse Captcha
#'
#' Inverse Captcha.
#'
#' @details
#'
#' To quote the [website](http://adventofcode.com/2017/day/1)
#'
#' **Part One**
#'
#' The captcha requires you to review a sequence of digits (your puzzle input)
#' and find the sum of all digits that match the next digit in the list. The
#' list is circular, so the digit after the last digit is the first digit in the
#' list.
#'
#' For example:
#'
#'  - 1122 produces a sum of 3 (1 + 2) because the first digit (1) matches the
#'    second digit and the third digit (2) matches the fourth digit.
#'  - 1111 produces 4 because each digit (all 1) matches the next.
#'  - 1234 produces 0 because no digit matches the next.
#'  - 91212129 produces 9 because the only digit that matches the next one is
#'    the last digit, 9.
#'
#' **Part Two**
#'
#' You notice a progress bar that jumps to 50% completion. Apparently, the door
#' isn't yet satisfied, but it did emit a star as encouragement. The
#' instructions change:
#'
#' Now, instead of considering the next digit, it wants you to consider the
#' digit halfway around the circular list. That is, if your list contains 10
#' items, only include a digit in your sum if the digit 10/2 = 5 steps forward
#' matches it. Fortunately, your list has an even number of elements.
#'
#' For example:
#'
#'  - 1212 produces 6: the list contains 4 items, and all four digits match the
#'    digit 2 items ahead.
#'  - 1221 produces 0, because every comparison is between a 1 and a 2.
#'  - 123425 produces 4, because both 2s match each other, but no other digit
#'    has a match.
#'  - 123123 produces 12.
#'  - 12131415 produces 4.
#'
#' @rdname day01
#' @export
sum_of_digits_matching_next <- function(x) {
  x <- as.character(x)
  digits <- extract_digits(x)

  # Find indices of next digits
  positions <- seq_len(nchar(x))
  next_positions <- c(tail(positions, -1), head(positions, 1))

  # Find matches
  matches_next <- digits[positions] == digits[next_positions]
  sum(digits[matches_next])
}

#' @rdname day01
#' @export
sum_of_digits_matching_halfway_around <- function(x) {
  x <- as.character(x)
  digits <- extract_digits(x)

  # Find indices of digits halfway around the circle
  positions <- seq_len(nchar(x))
  half_size <- nchar(x) / 2
  next_positions <- c(tail(positions, -half_size), head(positions, half_size))

  # Find matches
  matches_next <- digits[positions] == digits[next_positions]
  sum(digits[matches_next])
}

extract_digits <- function(x) {
  x %>%
    str_tokenize() %>%
    unlist() %>%
    as.numeric()
}

str_tokenize <- function(xs) {
  strsplit(xs, "")
}

