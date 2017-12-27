#' @export
create_coprocessor <- function(commands) {
  # I'm copying code from Day 18's machine which uses tidy evaluation to
  # evaluate instructions in a custom environment.

  # We are going to evaluate commands and bind symbols in this environment
  register <- rlang::env(a = 0, b = 0, c = 0, d = 0,
                         e = 0, f = 0, g = 0, h = 0)

  ## Set some basic object data and methods.

  # Need to keep track of machine commands so we can execute them and do jumps.
  register[[".commands"]] <- lapply(commands, parse_command)
  register[[".current_command"]] <- 1
  register[[".counts"]] <- c(mul = 0, set = 0, sub = 0, jnz = 0)

  register[[".has_next"]] <- function() {
    register[[".current_command"]] <= length(register[[".commands"]])
  }

  # Run the next command
  register[[".eval_next"]] <- function(m = 0) {
    next_one <- register[[".commands"]][[register[[".current_command"]]]]
    command_text <- rlang::expr_text(next_one)

    if (m == 1) {
      message(command_text)
    } else if (m == 2) {
      info <-
        sprintf("%s [a: %s, b: %s, c: %s, d: %s, e: %s, f: %s, g: %s, h: %s]",
                command_text, register$a, register$b, register$c, register$d,
                register$e, register$f, register$g, register$h)
      message(info)
    }

    verb <- substr(command_text, 1, 3)
    register[[".counts"]][verb] <- register[[".counts"]][verb] + 1
    rlang::eval_tidy(next_one)
    invisible(NULL)
  }

  # Advance n steps
  step <- function(n = 1) {
    value <- register[[".current_command"]]
    rlang::env_bind(register, .current_command = value + n)
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
