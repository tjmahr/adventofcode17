

parse_line <- function(line) {
  register <- list()

  inc <- function(symbol, value) {

  }

  parse_line <- function(x) {
    re <- "(\\w+) (inc|dec) (-?\\d+) if ((\\w+) (>|<|==|!=|>=|<=) (-?\\d+))"
    matches <- x %>%
      stringr::str_match(re) %>%
      as.data.frame(stringsAsFactors = FALSE) %>%
      as.list() %>%
      setNames(c("line", "target", "verb",
                 "arg", "cond", "cond_a",
                 "cond_op", "cond_b"))



    code <- sprintf("if (%s) %s(%s, %s)", matches$cond, matches$verb,
            matches$target, matches$arg)
    code <- sprintf("`if`(%s, %s(%s, %s))", matches$cond, matches$verb,
            matches$target, matches$arg)
    d <- quo(!! sym(code))
  }

  eval(parse(text = code), envir = register)

  eval_tidy(, data = register)

  c <- as.symbol(code)
  a <- 3 ; aa <- 4 ; evalq(evalq(a+b+aa, list(a = 1)), list(b = 5)) # == 10
  a <- 3 ; aa <- 4 ; evalq(evalq(a+b+aa, -1), list(b = 5))        # == 12

  do_if <- function(cond, verb, ...) {
    if (cond) verb(...)
  }
  verb <- inc

  f <- quo(letters)
  e <- expr(toupper(!! get_expr(f)))
  eval(e)

  # fn can either be a string, a symbol or a call
  lang("f", a = 1)
  lang(quote(f), a = 1)
  lang(quote(f()), a = 1)

  #' Can supply arguments individually or in a list
  lang(quote(f), a = 1, b = 2)
  lang(quote(f), splice(list(a = 1, b = 2)))

  # Creating namespaced calls:
  lang("fun", arg = quote(baz), .ns = "mypkg")

  new_e
  eval_tidy(expr(a > 1))

  get_expr(sym(matches$cond))
  expr()

  okay <- quo(do_if(!!sym(matches$cond), !! verb, a, 1))
  a <- 10
  eval_tidy(okay, data = register)


  with(register, {
    evalq(sym(code))
  })



  # expr <- parse(text = code)
  # code <- "stop()"
  # expr <- parse(text = code)
  expr <- expr(!! sym("stop()"))
  eval_tidy(expr)
  eval_bare(expr)
  eval(expr)
  evalq(expr)

  # Like base::eval() and eval_bare(), eval_tidy() evaluates quoted
  # expressions:
  expr <- expr(1 + 2 + 3)
  eval_tidy(expr)

  # Like base::eval(), it lets you supply overscoping data:
  foo <- 1
  bar <- 2
  expr <- quote(list(foo, bar))
  eval_tidy(expr, list(foo = 100))

  # The main difference is that quosures self-evaluate within
  # eval_tidy():
  quo <- quo(1 + 2 + 3)
  eval(quo)
  eval_tidy(quo)


  eval(as.expression(sym(code)), register)
  evalq(sym(code), register)

  register <- list(a = 2, b = 1)
  inc <- function(x, y) x + y



  parse_line(x)

  expr <- rlang::expr(1 + 2 + 3)





  library(rlang)
  a <- "b"
  default <- 0

  eval_tidy()

  rlang::env_bind(register, !! a := !! default)
  ls(register)

  rlang::eval_bare(b <<- 10, env = register)

  rlang::eval_tidy(rlang::expr(a = 1), env = register)

  rlang::eval_bare(a <- 1, env = register)
  get(a, envir = register)

  my_env <- rlang::env()

  my_env$foo

  rlang::eval_bare()


  x <- "b inc 5 if a > 1"
  lines <- "
  b inc 5 if a > 1
  a inc 1 if b < 5
  c dec -10 if a >= 1
  c inc -20 if c == 10
  "

  stringr::str_match_all(lines, ")



}
line <- "
"
