#' Day 18: Duet
#'
#' [Duet](http://adventofcode.com/2017/day/18)
#'
#' @details
#'
#' **Part One**
#'
#' You discover a tablet containing some strange assembly code labeled
#' simply "[Duet](https://en.wikipedia.org/wiki/Duet)". Rather than bother
#' the sound card with it, you decide to run the code yourself.
#' Unfortunately, you don't see any documentation, so you're left to figure
#' out what the instructions mean on your own.
#'
#' It seems like the assembly is meant to operate on a set of *registers*
#' that are each named with a single letter and that can each hold a single
#' [integer](https://en.wikipedia.org/wiki/Integer). You suppose each
#' register should start with a value of `0`.
#'
#' There aren't that many instructions, so it shouldn't be hard to figure
#' out what they do. Here's what you determine:
#'
#' -   `snd X` *plays a sound* with a frequency equal to the value of `X`.
#' -   `set X Y` *sets* register `X` to the value of `Y`.
#' -   `add X Y` *increases* register `X` by the value of `Y`.
#' -   `mul X Y` sets register `X` to the result of *multiplying* the value
#'     contained in register `X` by the value of `Y`.
#' -   `mod X Y` sets register `X` to the *remainder* of dividing the value
#'     contained in register `X` by the value of `Y` (that is, it sets `X`
#'     to the result of `X`
#'     [modulo](https://en.wikipedia.org/wiki/Modulo_operation) `Y`).
#' -   `rcv X` *recovers* the frequency of the last sound played, but only
#'     when the value of `X` is not zero. (If it is zero, the command does
#'     nothing.)
#' -   `jgz X Y` *jumps* with an offset of the value of `Y`, but only if
#'     the value of `X` is *greater than zero*. (An offset of `2` skips the
#'     next instruction, an offset of `-1` jumps to the previous
#'     instruction, and so on.)
#'
#' Many of the instructions can take either a register (a single letter) or
#' a number. The value of a register is the integer it contains; the value
#' of a number is that number.
#'
#' After each *jump* instruction, the program continues with the
#' instruction to which the *jump* jumped. After any other instruction, the
#' program continues with the next instruction. Continuing (or jumping) off
#' either end of the program terminates it.
#'
#' For example:
#'
#'     set a 1
#'     add a 2
#'     mul a a
#'     mod a 5
#'     snd a
#'     set a 0
#'     rcv a
#'     jgz a -1
#'     set a 1
#'     jgz a -2
#'
#' -   The first four instructions set `a` to `1`, add `2` to it, square
#'     it, and then set it to itself modulo `5`, resulting in a value of
#'     `4`.
#' -   Then, a sound with frequency `4` (the value of `a`) is played.
#' -   After that, `a` is set to `0`, causing the subsequent `rcv` and
#'     `jgz` instructions to both be skipped (`rcv` because `a` is `0`, and
#'     `jgz` because `a` is not greater than `0`).
#' -   Finally, `a` is set to `1`, causing the next `jgz` instruction to
#'     activate, jumping back two instructions to another jump, which jumps
#'     again to the `rcv`, which ultimately triggers the *recover*
#'     operation.
#'
#' At the time the *recover* operation is executed, the frequency of the
#' last sound played is `4`.
#'
#' *What is the value of the recovered frequency* (the value of the most
#' recently played sound) the *first* time a `rcv` instruction is executed
#' with a non-zero value?
#'
#' **Part Two**
#'
#' As you congratulate yourself for a job well done, you notice that the
#' documentation has been on the back of the tablet this entire time. While
#' you actually got most of the instructions correct, there are a few key
#' differences. This assembly code isn't about sound at all - it's meant to
#' be run *twice at the same time*.
#'
#' Each running copy of the program has its own set of registers and
#' follows the code independently - in fact, the programs don't even
#' necessarily run at the same speed. To coordinate, they use the *send*
#' (`snd`) and *receive* (`rcv`) instructions:
#'
#' -   `snd X` *sends* the value of `X` to the other program. These values
#'     wait in a queue until that program is ready to receive them. Each
#'     program has its own message queue, so a program can never receive a
#'     message it sent.
#' -   `rcv X` *receives* the next value and stores it in register `X`. If
#'     no values are in the queue, the program *waits for a value to be
#'     sent to it*. Programs do not continue to the next instruction until
#'     they have received a value. Values are received in the order they
#'     are sent.
#'
#' Each program also has its own *program ID* (one `0` and the other `1`);
#' the register `p` should begin with this value.
#'
#' For example:
#'
#'     snd 1
#'     snd 2
#'     snd p
#'     rcv a
#'     rcv b
#'     rcv c
#'     rcv d
#'
#' Both programs begin by sending three values to the other. Program `0`
#' sends `1, 2, 0`; program `1` sends `1, 2, 1`. Then, each program
#' receives a value (both `1`) and stores it in `a`, receives another value
#' (both `2`) and stores it in `b`, and then each receives the program ID
#' of the other program (program `0` receives `1`; program `1` receives
#' `0`) and stores it in `c`. Each program now sees a different value in
#' its own copy of register `c`.
#'
#' Finally, both programs try to `rcv` a *fourth* time, but no data is
#' waiting for either of them, and they reach a *deadlock*. When this
#' happens, both programs terminate.
#'
#' It should be noted that it would be equally valid for the programs to
#' run at different speeds; for example, program `0` might have sent all
#' three values and then stopped at the first `rcv` before program `1`
#' executed even its first instruction.
#'
#' Once both of your programs have terminated (regardless of what caused
#' them to do so), *how many times did program `1` send a value*?
#'
#' @rdname day18
#' @export
#' @param commands a set of commands for the machine
#' commands <- "set a 1
#'   add a 2
#'   mul a a
#'   mod a 5
#'   snd a
#'   set a 0
#'   rcv a
#'   jgz a -1
#'   set a 1
#'   jgz a -2"
#'
#' machine <- commands %>%
#'   read_text_lines() %>%
#'   create_solo()
#'
#' machine$.eval_next()
#' machine$.eval_next()
#' machine$.eval_next()
#' machine$.eval_next()
#' machine$.eval_next()
create_solo <- function(commands) {
  # I wanted to write an interpreter that evaluated expressions in an
  # environment. That was fun. But I admit that an object-oriented approach
  # would have been a more familiar way to manage state, especially for the
  # second part of the problem where I had to add a lot of methods to
  # environment.

  # We are going to evaluate commands and bind symbols in this environment
  register <- rlang::env()


  ## Set some basic object data and methods.

  # Need to know last sound to recover it and keep track of recovered sounds.
  register[[".last_sound"]] <- NULL
  register[[".messages"]] <- NULL

  # Need to keep track of machine commands so we can execute them and do jumps.
  register[[".commands"]] <- lapply(commands, parse_command)
  register[[".current_command"]] <- 1

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

  # Set a value but don't change the current command
  quiet_set <- function(symbol, value) {
    symbol <- enexpr(symbol)
    rlang::env_bind(register, !! rlang::as_string(symbol) := as.numeric(value))
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
      x <- rlang::eval_tidy(sym(".last_sound"), env = register)
      register$.messages <- c(register$.messages, x)
    }
    step(1)
  }

  # `jgz x y`: (J)ump `y` steps if `x` is (G)reater than (Z)ero
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


#' @rdname day18
#' @export
#' @param program_id an integer
create_duet <- function(program_id, commands) {
  # The "duet" twist caught me by surprise, so I'm just going to copy/paste the
  # solo version to handle the duet problem.

  # I'm going to add vectors for incoming and outgoing messages, receive/post
  # functions for updating the incoming/outgoing vectors, and a waiting state
  # that is turned when the machine hits a rcv() instruction with an empty set
  # of incoming messages.

  register <- rlang::env()
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

  # .recieve() instructions
  register[[".receive"]] <- function(xs) {
    if (length(xs) != 0) {
      register[[".incoming"]] <- c(register[[".incoming"]], xs)
    }
  }

  # .post(), as in to send a letter
  register[[".post"]] <- function() {
    values <- register[[".outgoing"]]
    register[[".outgoing"]] <- numeric()
    values
  }

  register[[".eval_next"]] <- function() {
    next_one <- register[[".commands"]][[register[[".current_command"]]]]
    rlang::eval_tidy(next_one)
    invisible(NULL)
  }

  step <- function(n = 1) {
    value <- register[[".current_command"]]
    rlang::env_bind(register, .current_command = value + n)
  }

  quiet_set <- function(symbol, value) {
    symbol <- enexpr(symbol)
    rlang::env_bind(register, !! rlang::as_string(symbol) := as.numeric(value))
  }

  check_for_new_symbols <- function(symbol) {
    symbol <- enexpr(symbol)
    if (!rlang::is_syntactic_literal(symbol)) {
      if (!rlang::env_has(register, rlang::expr_name(symbol))) {
        quiet_set(!! symbol, 0)
      }
    }
  }

  set <- function(symbol, value) {
    symbol <- enexpr(symbol)
    value <- enexpr(value)
    value <- rlang::eval_tidy(value, env = register)

    rlang::env_bind(register, !! rlang::as_string(symbol) := value)
    step(1)
  }

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

  # Changed from solo version: command now updates the .outgoing queue
  # and the number of sends.
  snd <- function(symbol) {
    symbol <- enexpr(symbol)
    check_for_new_symbols(!! symbol)

    value <- rlang::eval_tidy(symbol, env = register)
    register[[".outgoing"]] <- c(register[[".outgoing"]], value)
    register[[".send_count"]] <- register[[".send_count"]] + 1
    step(1)
  }

  # Changed from solo version: command now updates the .incoming queue
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

  # Initialize p to the program id
  quiet_set(!! sym("p"), program_id)
  register
}

#' @rdname day18
#' @export
#' @param machine1,machine2 register machines
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
