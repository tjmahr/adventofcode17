
#' Day 25: Title
#'
#' [Title](http://adventofcode.com/2017/day/25)
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
#' @rdname day25
#' @export
turing_machine <- function(rules, starting_state) {
  # We'll use an object-oriented approach.

  tape <- rep(0L, 100L)
  pos <- 1L
  state <- starting_state

  machine_table <- rules %>%
    lapply(as.data.frame) %>%
    do.call(rbind, .)

  move_left <- function() move(-1L)
  move_right <- function() move(1L)
  move <- function(n) {
    maybe_expand(pos + n)
    pos <<- pos + n
    invisible(NULL)
  }

  # If the given position falls off the tape, expand the tape
  maybe_expand <- function(position) {
    if (position <= 0L) {
      expand_left()
    } else if (length(tape) <= position) {
      expand_right()
    }
    invisible(NULL)
  }

  expand_left <- function(n = 100L) {
    zeroes <- rep(0L, n)
    tape <<- c(zeroes, tape)
    pos <<- pos + length(zeroes)
    invisible(NULL)
  }

  expand_right <- function(n = 100L) {
    zeroes <- rep(0L, n)
    tape <<- c(tape, zeroes)
    invisible(NULL)
  }

  write <- function(symbol) {
    tape[pos] <<- symbol
    invisible(NULL)
  }

  step <- function() {
    row <- machine_table[machine_table[["start"]] == state &
                           machine_table[["scanned"]] == tape[pos], ]
    write(row[["print"]])
    if (row$move == "L") move_left()
    if (row$move == "R") move_right()
    state <<- row[["final"]]
    invisible(NULL)
  }

  # Print the local area of the tape around the cursor
  # "... X X X X X [currently-scanned (current state)] X X X X X ..."
  print_tape <- function(n = 5) {
    maybe_expand(pos - n)
    maybe_expand(pos + n)
    local_tape <- tape
    local_tape[pos] <- paste0("[", tape[pos] , " (", state, ")", "]")
    local_tape <- local_tape[seq(pos - n, pos + n)]
    out_tape <- c("...", local_tape, "...") %>% paste(collapse = " ")
    print(out_tape)
    invisible(out_tape)
  }

  checksum <- function() {
    sum(as.numeric(tape))
  }

  list(step = step,
       print_tape = print_tape,
       checksum = checksum,
       get_tape = function() tape)
}


#' @rdname day25
#' @export
create_tm_rule <- function(start, scanned, print, move, final) {
  structure(
    list(
      start = start,
      scanned = scanned,
      print = print,
      move = move,
      final = final),
    class = c("tm_rule", "list")
  )
}

#' @export
format.tm_rule <- function(x, ...) {
  sprintf("(%s,%s,%s,%s,%s)", x[[1]], x[[2]], x[[3]], x[[4]], x[[5]])
}

#' @export
print.tm_rule <- function(x, ...) {
  print(format(x), ...)
  invisible(x)
}

as.data.frame.tm_rule <- function(x, ...) {
  as.data.frame.list(x, stringsAsFactors = FALSE)
}
