#' Day 05: A Maze of Twisty Trampolines, All Alike
#'
#' [A Maze of Twisty Trampolines, All Alike](http://adventofcode.com/2017/day/5)
#'
#' @details
#'
#' **Part One**
#'
#' An urgent interrupt arrives from the CPU: it's trapped in a maze of jump
#' instructions, and it would like assistance from any programs with spare
#' cycles to help find the exit.
#'
#' The message includes a list of the offsets for each jump. Jumps are
#' relative: `-1` moves to the previous instruction, and `2` skips the next
#' one. Start at the first instruction in the list. The goal is to follow
#' the jumps until one leads *outside* the list.
#'
#' In addition, these instructions are a little strange; after each jump,
#' the offset of that instruction increases by `1`. So, if you come across
#' an offset of `3`, you would move three instructions forward, but change
#' it to a `4` for the next time it is encountered.
#'
#' For example, consider the following list of jump offsets:
#'
#'     0
#'     3
#'     0
#'     1
#'     -3
#'
#' Positive jumps ("forward") move downward; negative jumps move upward.
#' For legibility in this example, these offset values will be written all
#' on one line, with the current instruction marked in parentheses. The
#' following steps would be taken before an exit is found:
#'
#' -   `(0) 3  0  1  -3 ` - *before* we have taken any steps.
#' -   `(1) 3  0  1  -3 ` - jump with offset `0` (that is, don't jump at
#'     all). Fortunately, the instruction is then incremented to `1`.
#' -   ` 2 (3) 0  1  -3 ` - step forward because of the instruction we just
#'     modified. The first instruction is incremented again, now to `2`.
#' -   ` 2  4  0  1 (-3)` - jump all the way to the end; leave a `4`
#'     behind.
#' -   ` 2 (4) 0  1  -2 ` - go back to where we just were; increment `-3`
#'     to `-2`.
#' -   ` 2  5  0  1  -2 ` - jump `4` steps forward, escaping the maze.
#'
#' In this example, the exit is reached in `5` steps.
#'
#' *How many steps* does it take to reach the exit?
#'
#' **Part Two**
#'
#' Now, the jumps are even stranger: after each jump, if the offset was
#' *three or more*, instead *decrease* it by `1`. Otherwise, increase it by
#' `1` as before.
#'
#' Using this rule with the above example, the process now takes `10`
#' steps, and the offset values after finding the exit are left as
#' `2 3 2 3 -1`.
#'
#' *How many steps* does it now take to reach the exit?
#'
#' @rdname day05
#' @export
#' @param x a sequence of jump offsets
#' @examples
#' # Uses the first increment rule
#' find_time_to_escape_trampolines("0\n3\n0\n1\n-3")
#
#' # Uses the second increment rule
#' find_time_to_escape_twistolines("0\n3\n0\n1\n-3")
find_time_to_escape_trampolines <- function(x) {
  bot <- x %>%
    read_text_lines() %>%
    as.numeric() %>%
    create_jumpbot()

  bot$set_increment_rule("a")
  bot$run_steps()
  bot$time_to_escape()
}

#' @rdname day05
#' @export
find_time_to_escape_twistolines <- function(x) {
  bot <- x %>%
    read_text_lines() %>%
    as.numeric() %>%
    create_jumpbot()

  bot$set_increment_rule("b")
  bot$run_steps()
  bot$time_to_escape()
}

# Use a closure to create an object that performs and updates instructions
create_jumpbot <- function(steps) {
  # Private data
  steps <- steps
  history <- 1
  active_instruction_number <- 1
  increment <- function(x) stop()

  # Toggle between part a and part b functions
  set_increment_rule <- function(option) {
    if (option == "a") increment <<- function(x) x + 1
    if (option == "b") increment <<- function(x) ifelse(x >= 3, x - 1, x + 1)
  }

  # Some getters and setters
  show_steps <- function() steps
  get_active_instruction_number <- function() active_instruction_number
  get_active_instruction_value <- function() steps[active_instruction_number]
  time_to_escape <- function() if (has_escaped()) history - 1 else NA
  has_escaped <- function() {
    ! (active_instruction_number %in% seq_along(steps))
  }

  # Update current instruction
  apply_increment_rule <- function() {
    steps[get_active_instruction_number()] <<-
      increment(get_active_instruction_value())
  }

  # Do the current instruction
  do_instruction <- function() {
    curr_location <- get_active_instruction_number()
    step_size <- get_active_instruction_value()
    new_location <- curr_location + step_size
    apply_increment_rule()
    active_instruction_number <<- new_location
    history <<- history + 1
  }

  # Move from current position
  run_steps <- function() {
    while (!has_escaped()) {
      do_instruction()
      if (history %% 100000 == 0) message(history, " steps")
    }
  }

  list(
    show_steps = show_steps,
    time_to_escape = time_to_escape,
    set_increment_rule = set_increment_rule,
    run_steps = run_steps
  )
}

