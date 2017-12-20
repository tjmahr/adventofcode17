library(rlang)


duet <- function(commands) {


  commands <- "set a 1
  add a 2
  mul a a
  mod a 5
  snd a
  set a 0
  rcv a
  jgz a -1
  set a 1
  jgz a -2" %>%
    read_text_lines() %>%
    lapply(parse_command)

  parse_command <- function(command) {
    # Keep numbers as numbers and set strings to symbols
    command <- command %>%
      strsplit(" ") %>%
      unlist() %>%
      as.list() %>%
      lapply(type.convert, as.is = TRUE) %>%
      lapply(function(x) if (is.character(x)) sym(x) else x)

    # Create a function call expression
    f <- sym(command[[1]])
    xp <- lang(f, !!! command[-1])
    xp
  }

  register <- env()
  register$.last_sound <- NULL
  register$.messages <- NULL


  # Set a value
  set <- function(symbol, value) {
    symbol <- enexpr(symbol)
    env_bind(register, !! symbol := as.numeric(value))
    invisible(NULL)
  }

  # Set a value but don't consume an instruction
  quiet_set <- function(symbol, value) {
    symbol <- enexpr(symbol)
    env_bind(register, !! symbol := as.numeric(value))
    invisible(NULL)
  }

  check_for_new_symbols <- function(symbol) {
    symbol <- enexpr(symbol)
    if (!is_syntactic_literal(symbol)) {
      if (!env_has(register, expr_name(symbol))) {
        quiet_set(!! symbol, 0)
      }
    }
  }

  make_arithmetic_function <- function(func) {
    function(symbol, value) {
      symbol <- enexpr(symbol)
      value <- enexpr(value)
      check_for_new_symbols(!! symbol)
      check_for_new_symbols(!! value)

      old_value <- eval_tidy(symbol, env = register)
      value <- eval_tidy(value, env = register)
      quiet_set(!! symbol, func(old_value, value))
    }
  }

  add <- make_arithmetic_function(`+`)
  mod <- make_arithmetic_function(`%%`)
  mul <- make_arithmetic_function(`*`)

  snd <- function(symbol) {
    symbol <- enexpr(symbol)
    value <- eval_tidy(symbol, env = register)
    env_bind(register, .last_sound = value)
  }

  rcv <- function(symbol) {
    symbol <- enexpr(symbol)
    value <- eval_tidy(symbol, env = register)
    if (value != 0) {
      x <- eval_tidy(expr(.last_sound), env = register)
      register$.messages <- c(register$.messages, x)
    }
  }

  eval_tidy(parse_command("set a 1"))
  register$a

  eval_tidy(parse_command("add a 2"))
  register$a

  eval_tidy(parse_command("mul a a"))
  register$a

}


#
#
#
#
# check_for_new_symbols(a)
# register$a
# add(a, 10)
# add(e, 10)
# check_for_new_symbols(a)
# register$a
# register$e
# check_for_new_symbols(5)
# check_for_new_symbols(b)
# register$b
# check_for_new_symbols(c)
#
# register$c
#
# check_for_new_symbols(5)
# symbol <- enexpr(a)
# value <- is_syntactic_literal(expr(a))
#
# env_has(register, "b")
# env_has(register, "5")
# is_symbolic(5)
# is_symbolic(a)
#
#
#
#
#
# set(a, 0)
# add(a, 10)
# add(a, 10)
# snd(a)
# rcv(a)
# register$a
# register$.last_sound
# register$.messages
# mod(a, 5)
#
# register$a
#
# mul(a, a)
# register$a
# register$a
#
