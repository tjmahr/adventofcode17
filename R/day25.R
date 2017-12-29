#' Day 25: The Halting Problem
#'
#' [The Halting Problem](http://adventofcode.com/2017/day/25)
#'
#' @name day25
#' @rdname day25
#' @details
#'
#' **Part One**
#'
#' Following the twisty passageways deeper and deeper into the CPU, you
#' finally reach the core of the computer.
#' Here, in the expansive central chamber, you find a grand apparatus that
#' fills the entire room, suspended nanometers above your head.
#'
#' You had always imagined CPUs to be noisy, chaotic places, bustling with
#' activity. Instead, the room is quiet, motionless, and dark.
#'
#' Suddenly, you and the CPU's *garbage collector* startle each other.
#' "It's not often we get many visitors here!", he says. You inquire about
#' the stopped machinery.
#'
#' "It stopped milliseconds ago; not sure why. I'm a garbage collector, not
#' a doctor." You ask what the machine is for.
#'
#' "Programs these days, don't know their origins. That's the *Turing
#' machine*! It's what makes the whole computer work." You try to explain
#' that Turing machines are merely models of computation, but he cuts you
#' off. "No, see, that's just what they *want* you to think. Ultimately,
#' inside every CPU, there's a Turing machine driving the whole thing! Too
#' bad this one's broken. [We're
#' doomed!](https://www.youtube.com/watch?v=cTwZZz0HV8I)"
#'
#' You ask how you can help. "Well, unfortunately, the only way to get the
#' computer running again would be to create a whole new Turing machine
#' from scratch, but there's no *way* you can-" He notices the look on your
#' face, gives you a curious glance, shrugs, and goes back to sweeping the
#' floor.
#'
#' You find the *Turing machine blueprints* (your puzzle input) on a tablet
#' in a nearby pile of debris. Looking back up at the broken Turing machine
#' above, you can start to identify its parts:
#'
#' -   A *tape* which contains `0` repeated infinitely to the left and
#'     right.
#' -   A *cursor*, which can move left or right along the tape and read or
#'     write values at its current position.
#' -   A set of *states*, each containing rules about what to do based on
#'     the current value under the cursor.
#'
#' Each slot on the tape has two possible values: `0` (the starting value
#' for all slots) and `1`. Based on whether the cursor is pointing at a `0`
#' or a `1`, the current state says *what value to write* at the current
#' position of the cursor, whether to *move the cursor* left or right one
#' slot, and *which state to use next*.
#'
#' For example, suppose you found the following blueprint:
#'
#'     Begin in state A.
#'     Perform a diagnostic checksum after 6 steps.
#'
#'     In state A:
#'       If the current value is 0:
#'         - Write the value 1.
#'         - Move one slot to the right.
#'         - Continue with state B.
#'       If the current value is 1:
#'         - Write the value 0.
#'         - Move one slot to the left.
#'         - Continue with state B.
#'
#'     In state B:
#'       If the current value is 0:
#'         - Write the value 1.
#'         - Move one slot to the left.
#'         - Continue with state A.
#'       If the current value is 1:
#'         - Write the value 1.
#'         - Move one slot to the right.
#'         - Continue with state A.
#'
#' Running it until the number of steps required to take the listed
#' *diagnostic checksum* would result in the following tape configurations
#' (with the *cursor* marked in square brackets):
#'
#'     ... 0  0  0 [0] 0  0 ... (before any steps; about to run state A)
#'     ... 0  0  0  1 [0] 0 ... (after 1 step;     about to run state B)
#'     ... 0  0  0 [1] 1  0 ... (after 2 steps;    about to run state A)
#'     ... 0  0 [0] 0  1  0 ... (after 3 steps;    about to run state B)
#'     ... 0 [0] 1  0  1  0 ... (after 4 steps;    about to run state A)
#'     ... 0  1 [1] 0  1  0 ... (after 5 steps;    about to run state B)
#'     ... 0  1  1 [0] 1  0 ... (after 6 steps;    about to run state A)
#'
#' The CPU can confirm that the Turing machine is working by taking a
#' *diagnostic checksum* after a specific number of steps (given in the
#' blueprint). Once the specified number of steps have been executed, the
#' Turing machine should pause; once it does, count the number of times `1`
#' appears on the tape. In the above example, the *diagnostic checksum* is
#' *`3`*.
#'
#' Recreate the Turing machine and save the computer! *What is the
#' diagnostic checksum* it produces once it's working again?
#'
#' **Part Two (not really a puzzle)**
#'
#' The Turing machine, and soon the entire computer, springs back to life.
#' A console glows dimly nearby, awaiting your command.
#'
#'     > reboot printer
#'     Error: That command requires priority 50. You currently have priority 0.
#'     You must deposit 50 stars to increase your priority to the required level.
#'
#' The console flickers for a moment, and then prints another message:
#'
#'     Star accepted.
#'     You must deposit 49 stars to increase your priority to the required level.
#'
#' The *garbage collector* winks at you, then continues sweeping.
#'
#' @examples
#' demo_rules <- list(
#'   create_tm_rule("A", "0", "1", "R", "B"),
#'   create_tm_rule("A", "1", "0", "L", "B"),
#'   create_tm_rule("B", "0", "1", "L", "A"),
#'   create_tm_rule("B", "1", "1", "R", "A")
#' )
#'
#' m <- turing_machine(demo_rules, "A")
#' m$step()
#' m$format_tape()
#' m$step()
#' m$format_tape()
#' m$step()
#' m$format_tape()
#' m$step()
#' m$format_tape()
#' m$step()
#' m$format_tape()
#' m$step()
#' m$format_tape()
#' m$checksum()
NULL

#' @rdname day25
#' @export
#' @param rules a list of `tm_rules`
#' @param starting_state initial state of the machine
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

  # Get the local area of the tape around the cursor
  # "... X X X X X [currently-scanned (current state)] X X X X X ..."
  format_tape <- function(n = 5) {
    maybe_expand(pos - n)
    maybe_expand(pos + n)
    local_tape <- tape
    local_tape[pos] <- paste0("[", tape[pos] , " (", state, ")", "]")
    local_tape <- local_tape[seq(pos - n, pos + n)]
    paste(c("...", local_tape, "..."), collapse = " ")
  }

  print_tape <- function(n = 5) {
    out_tape <- format_tape(n)
    print(out_tape)
    invisible(out_tape)
  }

  checksum <- function() {
    sum(as.numeric(tape))
  }

  list(step = step,
       format_tape = format_tape,
       print_tape = print_tape,
       checksum = checksum,
       get_tape = function() tape)
}


#' @rdname day25
#' @export
#' @param start,scanned,print,move,final A Turing machine rule can be described
#'   as a tuple of five elements (`start`: starting state, `scanned`: currently
#'   scanned symbol, `print`: symbol to print, `move`: direction to move,
#'   `final`: finishing state).
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

#' @export
as.data.frame.tm_rule <- function(x, ...) {
  as.data.frame.list(x, stringsAsFactors = FALSE)
}
