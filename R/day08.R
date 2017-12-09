#' Day 8: I Heard You Like Registers
#'
#' I Heard You Like Registers
#'
#' @details
#'
#' To quote the [website](http://adventofcode.com/2017/day/8)
#'
#' **Part One**
#'
#' You receive a signal directly from the CPU. Because of your recent assistance
#' with jump instructions, it would like you to compute the result of a series
#' of unusual register instructions.
#'
#' Each instruction consists of several parts: the register to modify, whether
#' to increase or decrease that register's value, the amount by which to
#' increase or decrease it, and a condition. If the condition fails, skip the
#' instruction without modifying the register. The registers all start at 0. The
#' instructions look like this:
#'
#' ```
#' b inc 5 if a > 1
#' a inc 1 if b < 5
#' c dec -10 if a >= 1
#' c inc -20 if c == 10
#' ```
#'
#' These instructions would be processed as follows:
#'
#' - Because `a` starts at 0, it is not greater than 1, and so b is not
#'   modified.
#' - `a` is increased by 1 (to 1) because b is less than 5 (it is 0).
#' - `c` is decreased by -10 (to 10) because a is now greater than or
#'   equal to 1 (it is 1).
#' - `c` is increased by -20 (to -10) because c is equal to 10.
#'
#' After this process, the largest value in any register is `1`.
#'
#' You might also encounter `<=` (less than or equal to) or `!=` (not equal to).
#' However, the CPU doesn't have the bandwidth to tell you what all the
#' registers are named, and leaves that to you to determine.
#'
#' What is the largest value in any register after completing the instructions in
#' your puzzle input?
#'
#' **Part Two**
#'
#' @rdname day08
#' @export
run_many_instructions <- function(lines) {
  # My strategy is to convert the register instructions into R code and execute
  # them in a list environment so that R does all the symbol lookup, logic, and
  # math for me

  # Data lives here and names are looked up here.
  register <- list()

  # Basic flow for running an instruction: Parse the line and get R code.
  # Initialize any as-yet unseen values to 0 and evaluate the line of code
  run_instruction <- function(register, line) {
    instruction <- parse_instruction(line)
    register %>%
      initialize_vars(instruction$vars) %>%
      eval_instruction(instruction$target, instruction$code)
  }

  lines <- read_text_lines(lines)

  for (instruction in lines) {
    register <- run_instruction(register, instruction)
  }

  list(
    maximum = max(unlist(register)),
    register = register
  )

}

# Analyze a register instruction
parse_instruction <- function(x) {
  re <- "(\\w+) (inc|dec) (-?\\d+) if ((\\w+) (>|<|==|!=|>=|<=) (-?\\d+))"
  matches <- x %>%
    stringr::str_match(re) %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    as.list() %>%
    setNames(c("line", "target", "verb", "arg",
               "cond", "cond_a", "cond_op", "cond_b"))

  # Converting inc/dec to arithmetic opertators because I had trouble getting
  # eval_tidy() to look up the meaning of those symbols
  matches$op <- if (matches$verb == "inc") "+" else "-"

  code <- sprintf("if (%s) %s %s %s else %s", matches$cond, matches$target,
                  matches$op, matches$arg, matches$target)

  list(
    code = rlang::parse_expr(code),
    target = matches$target,
    vars = unique(c(matches$target, matches$cond_a)))
}

# Set missing vars to 0
initialize_vars <- function(register, vars) {
  missing <- vars[!rlang::has_name(register, vars)]
  register[missing] <- 0
  register
}

# Run the code to update a register
eval_instruction <- function(register, target, code) {
  result <- rlang::eval_tidy(code, data = register)
  register[[target]] <- result
  register
}

#' @rdname day08
#' @export
dayxx_b_stub <- function(x) {

}