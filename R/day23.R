#' Day 23: Coprocessor Conflagration
#'
#' [Coprocessor Conflagration](http://adventofcode.com/2017/day/23)
#'
#' @name day23
#' @rdname day23
#' @details
#'
#' **Part One**
#'
#' You decide to head directly to the CPU and fix the printer from there.
#' As you get close, you find an *experimental coprocessor* doing so much
#' work that the local programs are afraid it will [halt and catch
#' fire](https://en.wikipedia.org/wiki/Halt_and_Catch_Fire). This would
#' cause serious issues for the rest of the computer, so you head in and
#' see what you can do.
#'
#' The code it's running seems to be a variant of the kind you saw recently
#' on that [tablet](http://adventofcode.com/2017/day/18). The general
#' functionality seems *very similar*, but some of the instructions are
#' different:
#'
#' -   `set X Y` *sets* register `X` to the value of `Y`.
#' -   `sub X Y` *decreases* register `X` by the value of `Y`.
#' -   `mul X Y` sets register `X` to the result of *multiplying* the value
#'     contained in register `X` by the value of `Y`.
#' -   `jnz X Y` *jumps* with an offset of the value of `Y`, but only if
#'     the value of `X` is *not zero*. (An offset of `2` skips the next
#'     instruction, an offset of `-1` jumps to the previous instruction,
#'     and so on.)
#'
#' The coprocessor is currently set to some kind of *debug mode*, which
#' allows for testing, but prevents it from doing any meaningful work.
#'
#' If you run the program (your puzzle input), *how many times is the `mul`
#' instruction invoked?*
#'
#' **Part Two**
#'
#' Now, it's time to fix the problem.
#'
#' The *debug mode switch* is wired directly to register `a`. You flip the
#' switch, which makes *register
#' `a` now start at `1`* when the program is executed.
#'
#' Immediately, the coprocessor begins to overheat. Whoever wrote this
#' program obviously didn't choose a very efficient implementation. You'll
#' need to *optimize the program* if it has any hope of completing before
#' Santa needs that printer working.
#'
#' The coprocessor's ultimate goal is to determine the final value left in
#' register `h` once the program completes. Technically, if it had that...
#' it wouldn't even need to run the program.
#'
#' After setting register `a` to `1`, if the program were to run to
#' completion, *what value would be left in register `h`?*
#'
#' @export
#' @param commands a character vector of commands to evaluate
#' @examples
#' m <- create_coprocessor(c("set a 0", "sub a -2", "mul a 2", "mul a 3"))
#' m$.eval_next()
#' # Different levels of debugging info
#' m$.eval_next(1)
#' m$.eval_next(2)
#' m$.eval_next(2)
#' m$.counts
#' m$a
create_coprocessor <- function(commands) {
  # I'm copying code from Day 18's solution which uses tidy evaluation to
  # evaluate instructions in a custom environment.

  # We are going to evaluate commands and bind symbols in this environment
  register <- rlang::env(a = 0, b = 0, c = 0, d = 0,
                         e = 0, f = 0, g = 0, h = 0)

  ## Set some basic object data and methods.

  # Need to keep track of machine commands so we can execute them and do jumps.
  register[[".commands"]] <- lapply(commands, parse_command)
  register[[".current_command"]] <- 1

  # The new additions for day 23
  register[[".counts"]] <- c(mul = 0, set = 0, sub = 0, jnz = 0)

  register[[".has_next"]] <- function() {
    register[[".current_command"]] <= length(register[[".commands"]])
  }

  # Run the next command
  register[[".eval_next"]] <- function(m = 0) {
    next_one <- register[[".commands"]][[register[[".current_command"]]]]
    command_text <- rlang::expr_text(next_one)

    # Print out debugging messages.
    if (m == 1) {
      message(command_text)
    } else if (m == 2) {
      template <- "%s [a: %s, b: %s, c: %s, d: %s, e: %s, f: %s, g: %s, h: %s]"
      info <- sprintf(template, command_text, register$a, register$b,
                      register$c, register$d, register$e, register$f,
                      register$g, register$h)
      message(info)
    }

    # Update the command counts
    verb <- substr(command_text, 1, 3)
    register[[".counts"]][verb] <- register[[".counts"]][verb] + 1

    rlang::eval_tidy(next_one)
    invisible(NULL)
  }

  # Advance n steps
  step <- function(n = 1) {
    register[[".current_command"]] <- register[[".current_command"]] + n
  }

  # Set a value but don't change the current command
  quiet_set <- function(symbol, value) {
    symbol <- enexpr(symbol)
    rlang::env_bind(register, !! rlang::as_string(symbol) := as.numeric(value))
  }

  ## Machine instructions

  # Set a register value
  set <- function(symbol, value) {
    symbol <- enexpr(symbol)
    value <- enexpr(value)
    value <- rlang::eval_tidy(value, env = register)

    rlang::env_bind(register, !! rlang::as_string(symbol) := value)
    step(1)
  }

  # Helper for creating +, *, %% commands
  make_arithmetic_function <- function(func) {
    function(symbol, value) {
      symbol <- enexpr(symbol)
      value <- enexpr(value)

      old_value <- rlang::eval_tidy(symbol, env = register)
      value <- rlang::eval_tidy(value, env = register)
      quiet_set(!! symbol, func(old_value, value))
      step(1)
    }
  }

  sub <- make_arithmetic_function(`-`)
  mul <- make_arithmetic_function(`*`)

  # `jnz x y`: (J)ump `y` steps if `x` is (N)ot (Z)ero
  jnz <- function(symbol, offset) {
    symbol <- enexpr(symbol)
    offset <- enexpr(offset)

    value <- rlang::eval_tidy(symbol, env = register)
    offset_value <- rlang::eval_tidy(offset, env = register)

    if (value != 0) {
      step(offset)
    } else {
      step(1)
    }
  }

  register
}
