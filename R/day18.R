

#' @export
create_solo <- function(commands) {
  # I wanted to write an interpreter that evaluated expressions in an
  # environment, but I admit that an object-oriented approach would have been a
  # more familiar way to manage state.

  # We are going to evaluate commands and bind symbols in this environment
  register <- rlang::env()

  # Set some basic object data and methods
  register[[".last_sound"]] <- NULL
  register[[".messages"]] <- NULL
  register[[".commands"]] <- lapply(commands, parse_command)
  register[[".current_command"]] <- 1

  # Run the next command
  register[[".eval_next"]] <- function() {
    next_one <- register[[".commands"]][[register[[".current_command"]]]]
    message(rlang::expr_text(next_one))
    rlang::eval_tidy(next_one)
    invisible(NULL)
  }

  # Advance n steps
  step <- function(n = 1) {
    value <- register[[".current_command"]]
    rlang::env_bind(register, .current_command = value + n)
  }

  # Set a value but don't consume an instruction
  quiet_set <- function(symbol, value) {
    symbol <- enexpr(symbol)
    rlang::env_bind(register, !! symbol := as.numeric(value))
  }

  # Initialize any new symbols to 0
  check_for_new_symbols <- function(symbol) {
    symbol <- enexpr(symbol)
    if (!rlang::is_syntactic_literal(symbol)) {
      if (!rlang::env_has(register, rlang::expr_name(symbol))) {
        quiet_set(!! symbol, 0)
      }
    }
  }

  ## Machine instructions

  # Set a value
  set <- function(symbol, value) {
    symbol <- enexpr(symbol)
    value <- enexpr(value)
    value <- rlang::eval_tidy(value, env = register)

    rlang::env_bind(register, !! symbol := value)
    step(1)
  }

  # Helper for creating +, *, %%
  make_arithmetic_function <- function(func) {
    function(symbol, value) {
      symbol <- enexpr(symbol)
      value <- enexpr(value)
      check_for_new_symbols(!! symbol)
      check_for_new_symbols(!! value)

      old_value <- rlang::eval_tidy(symbol, env = register)
      value <- rlang::eval_tidy(value, env = register)
      quiet_set(!! symbol, func(old_value, value))
      step(1)
    }
  }

  add <- make_arithmetic_function(`+`)
  mod <- make_arithmetic_function(`%%`)
  mul <- make_arithmetic_function(`*`)

  # Play a sound
  snd <- function(symbol) {
    symbol <- enexpr(symbol)
    check_for_new_symbols(!! symbol)

    value <- rlang::eval_tidy(symbol, env = register)
    rlang::env_bind(register, .last_sound = value)
    step(1)
  }

  # Recover last sound
  rcv <- function(symbol) {
    symbol <- enexpr(symbol)
    check_for_new_symbols(!! symbol)

    value <- rlang::eval_tidy(symbol, env = register)
    if (value != 0) {
      x <- rlang::eval_tidy(expr(.last_sound), env = register)
      register$.messages <- c(register$.messages, x)
    }
    step(1)
  }

  # jgz x y jumps (*J*gz) y steps is x is greater than zero (j*GZ*)
  jgz <- function(symbol, offset) {
    symbol <- enexpr(symbol)
    offset <- enexpr(offset)
    check_for_new_symbols(!! symbol)
    check_for_new_symbols(!! offset)

    value <- rlang::eval_tidy(symbol, env = register)
    offset_value <- rlang::eval_tidy(offset, env = register)

    if (value > 0) {
      step(offset)
    } else {
      step(1)
    }
  }

  register
}

#' @export
create_duet <- function(program_id, commands) {
  # The "duet" twist caught me by surprise, so I'm just going to copy/paste the
  # solo version to handle the duet problem.

  # I'm going to add vectors for incoming and outgoing messages, receive/post
  # functions for updating the incoming/outgoing vectors, and a waiting state
  # that is turned when the machine hits a rcv() instruction with an empty set
  # of incoming messages.


  # We are going to evaluate commands and bind symbols in this environment
  register <- rlang::env()

  # Set some basic object data and methods
  register[[".commands"]] <- lapply(commands, parse_command)
  register[[".current_command"]] <- 1
  register[[".outgoing"]] <- numeric()
  register[[".incoming"]] <- numeric()
  register[[".send_count"]] <- 0

  # Can the machine keep running?
  register[[".has_next"]] <- function() {
    register[[".current_command"]] <= length(register[[".commands"]])
  }

  # Is the machine waiting for a message?
  register[[".is_waiting"]] <- function() {
    state <- rlang::expr_text(
      register[[".commands"]][[register[[".current_command"]]]]
    )
    needs_a_msg <- substr(state, 1, 3) == "rcv"
    no_msgs <- length(register[[".incoming"]]) == 0
    all(needs_a_msg, no_msgs)
  }

  # Can the machine do something?
  register[[".is_ready"]] <- function() {
    !register[[".is_waiting"]]() && register[[".has_next"]]()
  }

  register[[".receive"]] <- function(xs) {
    if (length(xs) != 0) {
      # register[[".waiting"]] <- FALSE
      register[[".incoming"]] <- c(register[[".incoming"]], xs)
    }
  }

  # post() like a send a letter
  register[[".post"]] <- function() {
    values <- register[[".outgoing"]]
    register[[".outgoing"]] <- numeric()
    values
  }

  # Run the next command
  register[[".eval_next"]] <- function() {
    next_one <- register[[".commands"]][[register[[".current_command"]]]]
    rlang::eval_tidy(next_one)
    invisible(NULL)
  }

  # Advance n steps
  step <- function(n = 1) {
    value <- register[[".current_command"]]
    rlang::env_bind(register, .current_command = value + n)
  }

  # Set a value but don't consume an instruction
  quiet_set <- function(symbol, value) {
    symbol <- enexpr(symbol)
    rlang::env_bind(register, !! symbol := as.numeric(value))
  }

  # Initialize any new symbols to 0
  check_for_new_symbols <- function(symbol) {
    symbol <- enexpr(symbol)
    if (!rlang::is_syntactic_literal(symbol)) {
      if (!rlang::env_has(register, rlang::expr_name(symbol))) {
        quiet_set(!! symbol, 0)
      }
    }
  }

  # Set a value
  set <- function(symbol, value) {
    symbol <- enexpr(symbol)
    value <- enexpr(value)
    value <- rlang::eval_tidy(value, env = register)

    rlang::env_bind(register, !! symbol := value)
    step(1)
  }

  # Helper for creating +, *, %%
  make_arithmetic_function <- function(func) {
    function(symbol, value) {
      symbol <- enexpr(symbol)
      value <- enexpr(value)
      check_for_new_symbols(!! symbol)
      check_for_new_symbols(!! value)

      old_value <- rlang::eval_tidy(symbol, env = register)
      value <- rlang::eval_tidy(value, env = register)
      quiet_set(!! symbol, func(old_value, value))
      step(1)
    }
  }

  add <- make_arithmetic_function(`+`)
  mod <- make_arithmetic_function(`%%`)
  mul <- make_arithmetic_function(`*`)

  # Send a message
  snd <- function(symbol) {
    symbol <- enexpr(symbol)
    check_for_new_symbols(!! symbol)

    value <- rlang::eval_tidy(symbol, env = register)
    register[[".outgoing"]] <- c(register[[".outgoing"]], value)
    register[[".send_count"]] <- register[[".send_count"]] + 1
    step(1)
  }

  # Receive a message
  rcv <- function(symbol) {
    symbol <- enexpr(symbol)
    check_for_new_symbols(!! symbol)

    if (length(register$.incoming) == 0) {
    } else {
      value <- register$.incoming[1]
      register$.incoming <- register$.incoming[-1]
      quiet_set(!! symbol, value)
      step(1)
    }
  }

  # jgz x y jumps (*J*gz) y steps is x is greater than zero (j*GZ*)
  jgz <- function(symbol, offset) {
    symbol <- enexpr(symbol)
    offset <- enexpr(offset)
    check_for_new_symbols(!! symbol)
    check_for_new_symbols(!! offset)

    value <- rlang::eval_tidy(symbol, env = register)
    offset_value <- rlang::eval_tidy(offset, env = register)

    if (value > 0) {
      step(offset_value)
    } else {
      step(1)
    }
  }

  quiet_set(p, program_id)
  register
}

#' @export
in_deadlock <- function(machine1, machine2) {
  stalled1 <- any(machine1$.is_waiting(), !machine1$.has_next())
  stalled2 <- any(machine2$.is_waiting(), !machine2$.has_next())
  all(stalled1, stalled2)
}

parse_command <- function(command) {
  # Keep numbers as numbers and set strings to symbols
  command <- command %>%
    strsplit(" ") %>%
    unlist() %>%
    as.list() %>%
    lapply(type.convert, as.is = TRUE) %>%
    lapply(function(x) if (is.character(x)) sym(x) else x)

  # Create a function call expression
  f <- rlang::sym(command[[1]])
  rlang::lang(f, !!! command[-1])
}
