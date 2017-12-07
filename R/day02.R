#' Day 2: Corruption Checksum
#'
#' Corruption Checksum
#'
#' @details
#'
#' To quote the [website](http://adventofcode.com/2017/day/2)
#'
#' **Part One**
#'
#' As you walk through the door, a glowing humanoid shape yells in your
#' direction. "You there! Your state appears to be idle. Come help us repair the
#' corruption in this spreadsheet - if we take another millisecond, we'll have
#' to display an hourglass cursor!"
#'
#' The spreadsheet consists of rows of apparently-random numbers. To make sure
#' the recovery process is on the right track, they need you to calculate the
#' spreadsheet's checksum. For each row, determine the difference between the
#' largest value and the smallest value; the checksum is the sum of all of these
#' differences.
#'
#' For example, given the following spreadsheet:
#'
#' ```
#' 5 1 9 5
#' 7 5 3
#' 2 4 6 8
#' ```
#'
#'   - The first row's largest and smallest values are 9 and 1, and their
#'     difference is 8.
#'   - The second row's largest and smallest values are 7 and 3, and their
#'     difference is 4.
#'   - The third row's difference is 6.
#'
#' In this example, the spreadsheet's checksum would be 8 + 4 + 6 = 18.
#'
#' **Part Two**
#'
#' "Based on what we're seeing, it looks like all the User wanted is some
#' information about the evenly divisible values in the spreadsheet.
#' Unfortunately, none of us are equipped for that kind of calculation - most of
#' us specialize in bitwise operations."
#'
#' It sounds like the goal is to find the only two numbers in each row where one
#' evenly divides the other - that is, where the result of the division
#' operation is a whole number. They would like you to find those numbers on
#' each line, divide them, and add up each line's result.
#'
#' For example, given the following spreadsheet:
#'
#' ```
#' 5 9 2 8
#' 9 4 7 3
#' 3 8 6 5
#' ```
#'
#'   - In the first row, the only two numbers that evenly divide are 8 and 2;
#'     the result of this division is 4.
#'   - In the second row, the two numbers are 9 and 3; the result is 3.
#'   - In the third row, the result is 2.
#'
#' In this example, the sum of the results would be 4 + 3 + 2 = 9.
#' @rdname day02
#' @export
checksum_range <- function(x) {
  parse_spreadsheet(x) %>%
    lapply(function(x) max(x) - min(x)) %>%
    unlist() %>%
    sum()
}

#' @rdname day02
#' @export
checksum_evenly_divisible <- function(x) {
  parse_spreadsheet(x) %>%
    lapply(find_evenly_divisible_pair) %>%
    lapply(function(x) max(x) / min(x)) %>%
    unlist() %>%
    sum()
}

find_evenly_divisible_pair <- function(x) {
  x %>%
    # Take all pairs
    utils::combn(2) %>%
    apply(2, list) %>%
    lapply(unlist) %>%
    # Keep just evenly divisible pairs
    Filter(function(x) max(x) %% min(x) == 0, .) %>%
    unlist()
}

parse_spreadsheet <- function(x) {
  x %>%
    # Split lines
    strsplit("\n") %>%
    unlist() %>%
    # Find numbers on each line
    stringr::str_extract_all("\\d+") %>%
    lapply(as.numeric) %>%
    Filter(function(x) length(x) != 0, .)
}

