#' Day 13: Packet Scanners
#'
#' [Packet Scanners](http://adventofcode.com/2017/day/13)
#'
#' @details
#'
#' **Part One**
#'
#' You need to cross a vast *firewall*. The firewall consists of several
#' layers, each with a *security scanner* that moves back and forth across
#' the layer. To succeed, you must not be detected by a scanner.
#'
#' By studying the firewall briefly, you are able to record (in your puzzle
#' input) the *depth* of each layer and the *range* of the scanning area
#' for the scanner within it, written as `depth: range`. Each layer has a
#' thickness of exactly `1`. A layer at depth `0` begins immediately inside
#' the firewall; a layer at depth `1` would start immediately after that.
#'
#' For example, suppose you've recorded the following:
#'
#'     0: 3
#'     1: 2
#'     4: 4
#'     6: 4
#'
#' This means that there is a layer immediately inside the firewall (with
#' range `3`), a second layer immediately after that (with range `2`), a
#' third layer which begins at depth `4` (with range `4`), and a fourth
#' layer which begins at depth 6 (also with range `4`). Visually, it might
#' look like this:
#'
#'      0   1   2   3   4   5   6
#'     [ ] [ ] ... ... [ ] ... [ ]
#'     [ ] [ ]         [ ]     [ ]
#'     [ ]             [ ]     [ ]
#'                     [ ]     [ ]
#'
#' Within each layer, a security scanner moves back and forth within its
#' range. Each security scanner starts at the top and moves down until it
#' reaches the bottom, then moves up until it reaches the top, and repeats.
#' A security scanner takes *one picosecond* to move one step. Drawing
#' scanners as `S`, the first few picoseconds look like this:
#'
#'     Picosecond 0:
#'      0   1   2   3   4   5   6
#'     [S] [S] ... ... [S] ... [S]
#'     [ ] [ ]         [ ]     [ ]
#'     [ ]             [ ]     [ ]
#'                     [ ]     [ ]
#'
#'     Picosecond 1:
#'      0   1   2   3   4   5   6
#'     [ ] [ ] ... ... [ ] ... [ ]
#'     [S] [S]         [S]     [S]
#'     [ ]             [ ]     [ ]
#'                     [ ]     [ ]
#'
#'     Picosecond 2:
#'      0   1   2   3   4   5   6
#'     [ ] [S] ... ... [ ] ... [ ]
#'     [ ] [ ]         [ ]     [ ]
#'     [S]             [S]     [S]
#'                     [ ]     [ ]
#'
#'     Picosecond 3:
#'      0   1   2   3   4   5   6
#'     [ ] [ ] ... ... [ ] ... [ ]
#'     [S] [S]         [ ]     [ ]
#'     [ ]             [ ]     [ ]
#'                     [S]     [S]
#'
#' Your plan is to hitch a ride on a packet about to move through the
#' firewall. The packet will travel along the top of each layer, and it
#' moves at *one layer per picosecond*. Each picosecond, the packet moves
#' one layer forward (its first move takes it into layer 0), and then the
#' scanners move one step. If there is a scanner at the top of the layer
#' *as your packet enters it*, you are *caught*. (If a scanner moves into
#' the top of its layer while you are there, you are *not* caught: it
#' doesn't have time to notice you before you leave.) If you were to do
#' this in the configuration above, marking your current position with
#' parentheses, your passage through the firewall would look like this:
#'
#'     Initial state:
#'      0   1   2   3   4   5   6
#'     [S] [S] ... ... [S] ... [S]
#'     [ ] [ ]         [ ]     [ ]
#'     [ ]             [ ]     [ ]
#'                     [ ]     [ ]
#'
#'     Picosecond 0:
#'      0   1   2   3   4   5   6
#'     (S) [S] ... ... [S] ... [S]
#'     [ ] [ ]         [ ]     [ ]
#'     [ ]             [ ]     [ ]
#'                     [ ]     [ ]
#'
#'      0   1   2   3   4   5   6
#'     ( ) [ ] ... ... [ ] ... [ ]
#'     [S] [S]         [S]     [S]
#'     [ ]             [ ]     [ ]
#'                     [ ]     [ ]
#'
#'
#'     Picosecond 1:
#'      0   1   2   3   4   5   6
#'     [ ] ( ) ... ... [ ] ... [ ]
#'     [S] [S]         [S]     [S]
#'     [ ]             [ ]     [ ]
#'                     [ ]     [ ]
#'
#'      0   1   2   3   4   5   6
#'     [ ] (S) ... ... [ ] ... [ ]
#'     [ ] [ ]         [ ]     [ ]
#'     [S]             [S]     [S]
#'                     [ ]     [ ]
#'
#'
#'     Picosecond 2:
#'      0   1   2   3   4   5   6
#'     [ ] [S] (.) ... [ ] ... [ ]
#'     [ ] [ ]         [ ]     [ ]
#'     [S]             [S]     [S]
#'                     [ ]     [ ]
#'
#'      0   1   2   3   4   5   6
#'     [ ] [ ] (.) ... [ ] ... [ ]
#'     [S] [S]         [ ]     [ ]
#'     [ ]             [ ]     [ ]
#'                     [S]     [S]
#'
#'
#'     Picosecond 3:
#'      0   1   2   3   4   5   6
#'     [ ] [ ] ... (.) [ ] ... [ ]
#'     [S] [S]         [ ]     [ ]
#'     [ ]             [ ]     [ ]
#'                     [S]     [S]
#'
#'      0   1   2   3   4   5   6
#'     [S] [S] ... (.) [ ] ... [ ]
#'     [ ] [ ]         [ ]     [ ]
#'     [ ]             [S]     [S]
#'                     [ ]     [ ]
#'
#'
#'     Picosecond 4:
#'      0   1   2   3   4   5   6
#'     [S] [S] ... ... ( ) ... [ ]
#'     [ ] [ ]         [ ]     [ ]
#'     [ ]             [S]     [S]
#'                     [ ]     [ ]
#'
#'      0   1   2   3   4   5   6
#'     [ ] [ ] ... ... ( ) ... [ ]
#'     [S] [S]         [S]     [S]
#'     [ ]             [ ]     [ ]
#'                     [ ]     [ ]
#'
#'
#'     Picosecond 5:
#'      0   1   2   3   4   5   6
#'     [ ] [ ] ... ... [ ] (.) [ ]
#'     [S] [S]         [S]     [S]
#'     [ ]             [ ]     [ ]
#'                     [ ]     [ ]
#'
#'      0   1   2   3   4   5   6
#'     [ ] [S] ... ... [S] (.) [S]
#'     [ ] [ ]         [ ]     [ ]
#'     [S]             [ ]     [ ]
#'                     [ ]     [ ]
#'
#'
#'     Picosecond 6:
#'      0   1   2   3   4   5   6
#'     [ ] [S] ... ... [S] ... (S)
#'     [ ] [ ]         [ ]     [ ]
#'     [S]             [ ]     [ ]
#'                     [ ]     [ ]
#'
#'      0   1   2   3   4   5   6
#'     [ ] [ ] ... ... [ ] ... ( )
#'     [S] [S]         [S]     [S]
#'     [ ]             [ ]     [ ]
#'                     [ ]     [ ]
#'
#' In this situation, you are *caught* in layers `0` and `6`, because your
#' packet entered the layer when its scanner was at the top when you
#' entered it. You are *not* caught in layer `1`, since the scanner moved
#' into the top of the layer once you were already there.
#'
#' The *severity* of getting caught on a layer is equal to its *depth*
#' multiplied by its *range*. (Ignore layers in which you do not get
#' caught.) The severity of the whole trip is the sum of these values. In
#' the example above, the trip severity is `0*3 + 6*4 = 24`.
#'
#' Given the details of the firewall you've recorded, if you leave
#' immediately, *what is the severity of your whole trip*?
#'
#' **Part Two**
#'
#' Now, you need to pass through the firewall without being caught - easier
#' said than done.
#'
#' You can't control the speed of the packet,
#' but you can *delay* it any number of picoseconds. For each picosecond
#' you delay the packet before beginning your trip, all security scanners
#' move one step. You're not in the firewall during this time; you don't
#' enter layer `0` until you stop delaying the packet.
#'
#' In the example above, if you delay `10` picoseconds (picoseconds `0` -
#' `9`), you won't get caught:
#'
#'     State after delaying:
#'      0   1   2   3   4   5   6
#'     [ ] [S] ... ... [ ] ... [ ]
#'     [ ] [ ]         [ ]     [ ]
#'     [S]             [S]     [S]
#'                     [ ]     [ ]
#'
#'     Picosecond 10:
#'      0   1   2   3   4   5   6
#'     ( ) [S] ... ... [ ] ... [ ]
#'     [ ] [ ]         [ ]     [ ]
#'     [S]             [S]     [S]
#'                     [ ]     [ ]
#'
#'      0   1   2   3   4   5   6
#'     ( ) [ ] ... ... [ ] ... [ ]
#'     [S] [S]         [S]     [S]
#'     [ ]             [ ]     [ ]
#'                     [ ]     [ ]
#'
#'
#'     Picosecond 11:
#'      0   1   2   3   4   5   6
#'     [ ] ( ) ... ... [ ] ... [ ]
#'     [S] [S]         [S]     [S]
#'     [ ]             [ ]     [ ]
#'                     [ ]     [ ]
#'
#'      0   1   2   3   4   5   6
#'     [S] (S) ... ... [S] ... [S]
#'     [ ] [ ]         [ ]     [ ]
#'     [ ]             [ ]     [ ]
#'                     [ ]     [ ]
#'
#'
#'     Picosecond 12:
#'      0   1   2   3   4   5   6
#'     [S] [S] (.) ... [S] ... [S]
#'     [ ] [ ]         [ ]     [ ]
#'     [ ]             [ ]     [ ]
#'                     [ ]     [ ]
#'
#'      0   1   2   3   4   5   6
#'     [ ] [ ] (.) ... [ ] ... [ ]
#'     [S] [S]         [S]     [S]
#'     [ ]             [ ]     [ ]
#'                     [ ]     [ ]
#'
#'
#'     Picosecond 13:
#'      0   1   2   3   4   5   6
#'     [ ] [ ] ... (.) [ ] ... [ ]
#'     [S] [S]         [S]     [S]
#'     [ ]             [ ]     [ ]
#'                     [ ]     [ ]
#'
#'      0   1   2   3   4   5   6
#'     [ ] [S] ... (.) [ ] ... [ ]
#'     [ ] [ ]         [ ]     [ ]
#'     [S]             [S]     [S]
#'                     [ ]     [ ]
#'
#'
#'     Picosecond 14:
#'      0   1   2   3   4   5   6
#'     [ ] [S] ... ... ( ) ... [ ]
#'     [ ] [ ]         [ ]     [ ]
#'     [S]             [S]     [S]
#'                     [ ]     [ ]
#'
#'      0   1   2   3   4   5   6
#'     [ ] [ ] ... ... ( ) ... [ ]
#'     [S] [S]         [ ]     [ ]
#'     [ ]             [ ]     [ ]
#'                     [S]     [S]
#'
#'
#'     Picosecond 15:
#'      0   1   2   3   4   5   6
#'     [ ] [ ] ... ... [ ] (.) [ ]
#'     [S] [S]         [ ]     [ ]
#'     [ ]             [ ]     [ ]
#'                     [S]     [S]
#'
#'      0   1   2   3   4   5   6
#'     [S] [S] ... ... [ ] (.) [ ]
#'     [ ] [ ]         [ ]     [ ]
#'     [ ]             [S]     [S]
#'                     [ ]     [ ]
#'
#'
#'     Picosecond 16:
#'      0   1   2   3   4   5   6
#'     [S] [S] ... ... [ ] ... ( )
#'     [ ] [ ]         [ ]     [ ]
#'     [ ]             [S]     [S]
#'                     [ ]     [ ]
#'
#'      0   1   2   3   4   5   6
#'     [ ] [ ] ... ... [ ] ... ( )
#'     [S] [S]         [S]     [S]
#'     [ ]             [ ]     [ ]
#'                     [ ]     [ ]
#'
#' Because all smaller delays would get you caught, the fewest number of
#' picoseconds you would need to delay to get through safely is `10`.
#'
#' *What is the fewest number of picoseconds* that you need to delay the
#' packet to pass through the firewall without being caught?
#'
#' @rdname day13
#' @export
#' @param scanner_lines a description of the scanners
#' @param start_delay start computing the delay from this value
#' @examples
#' "0: 3\n1: 2\n4: 4\n6: 4" %>%
#'   read_text_lines() %>%
#'   calculate_firewall_severity()
#'
#' "0: 3\n1: 2\n4: 4\n6: 4" %>%
#'   read_text_lines() %>%
#'   determine_firewall_delay()
calculate_firewall_severity <- function(scanner_lines) {
  # My strategy for today is to analytically determine the location of each
  # scanner. See the math in `locate_scanner()`. Then I use functional
  # programming to create a list of layers, find the locations of each scanner
  # when the player is in that layer, filter and sum the severities.
  scanner_lines %>%
    lapply(parse_scanner_line) %>%
    lapply(update_scanners) %>%
    keep_if(function(x) x$location == 1) %>%
    lapply(function(x) x$depth * x$range) %>%
    unlist() %>%
    sum()
}

#' @rdname day13
#' @export
determine_firewall_delay <- function(scanner_lines, start_delay = 0) {
  # For part b, count the scanners in top row at each delay value. Find a delay
  # with 0 scanners in top row.
  any_scanners_at_top <- function(scanners) {
    num_scanners_at_top(scanners) != 0
  }

  num_scanners_at_top <- function(scanners) {
    scanners %>%
      keep_if(function(x) x[["location"]] == 1) %>%
      length()
  }

  scanners <- scanner_lines %>%
    lapply(parse_scanner_line) %>%
    lapply(update_scanners, delay = 0)

  # Keep delaying until no scanners in top row
  delay <- start_delay
  while (any_scanners_at_top(scanners)) {
    if (delay %% 100000 == 0) {
      message("delay: ", delay)
    }
    delay <- delay + 1
    scanners <- scanners %>%
      lapply(update_scanners, delay = delay)
  }

  delay
}

# Determine where each scanner is when the player is in the scanner's layer
update_scanners <- function(scanner, delay = 0) {
  scanner[["location"]] <-locate_scanner(
    scanner[["range"]],
    scanner[["depth"]] + delay + 1)
  scanner
}

# Find the location of a scanner with a given range at a given time
locate_scanner <- function(range, time) {
  stopifnot(length(range) == 1, length(time) == 1)

  # I wrote down the scanner locations at various time steps and noticed:
  #
  # range: 3
  # time, location
  # 1, 1 ┐
  # 2, 2 │
  # 3, 3 │
  # 4, 2 ┘
  # 5, 1 ┐
  # 6, 2 │
  # 7, 3 │
  # 8, 2 ┘
  #
  # Each complete cycle is 4 steps.
  #
  # range: 5
  # time, location
  #  1, 1 ┐
  #  2, 2 │
  #  3, 3 │
  #  4, 4 │
  #  5, 5 │
  #  6, 4 │
  #  7, 3 │
  #  8, 2 ┘
  #  9, 1 ┐
  # 10, 2 │
  # 11, 3 │
  # 12, 4 │
  # 13, 5 │
  # 14, 4 │
  # 15, 3 │
  # 16, 2 ┘
  #
  # Each complete cycle is 8 steps.
  #
  # Generally, a complete cycle is equal to the (range - 1) * 2
  #
  # So we will remove the complete cycles and then find the location with the
  # remaining number of steps.

  # Remove complete cycles. The +1 and -1 is account for R's 1-based indexing.
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

# Create a list with information about a scanner
parse_scanner_line <- function(x) {
  x <- strsplit(x, ": ") %>%
    unlist() %>%
    as.numeric()
  list(depth = x[1], range = x[2])
}


