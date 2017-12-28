# In order for the documentation to work, I had to escape all the braces {}

#' Day 09: Stream Processing
#'
#' [Stream Processing](http://adventofcode.com/2017/day/9)
#'
#' @details
#'
#' **Part One**
#'
#' A large stream blocks your path. According to the locals, it's not safe to
#' cross the stream at the moment because it's full of *garbage*. You look down
#' at the stream; rather than water, you discover that it's a *stream of
#' characters*.
#'
#' You sit for a while and record part of the stream (your puzzle input). The
#' characters represent *groups* - sequences that begin with \code{\{} and end
#' with \code{\}}. Within a group, there are zero or more other things,
#' separated by commas: either another *group* or *garbage*. Since groups can
#' contain other groups, a \code{\}} only closes the *most-recently-opened
#' unclosed group* - that is, they are nestable. Your puzzle input represents a
#' single, large group which itself contains many smaller ones.
#'
#' Sometimes, instead of a group, you will find *garbage*. Garbage begins with
#' \code{<} and ends with \code{>}. Between those angle brackets, almost any
#' character can appear, including \code{\{} and \code{\}}. *Within* garbage,
#' \code{<} has no special meaning.
#'
#' In a futile attempt to clean up the garbage, some program has *canceled* some
#' of the characters within it using \code{!}: inside garbage, *any* character
#' that comes after \code{!} should be *ignored*, including \code{<}, \code{>},
#' and even another \code{!}.
#'
#' You don't see any characters that deviate from these rules. Outside
#' garbage, you only find well-formed groups, and garbage always terminates
#' according to the rules above.
#'
#' Here are some self-contained pieces of garbage:
#'
#' - \code{<>}, empty garbage.
#' - \code{<random characters>}, garbage containing random characters.
#' - \code{<<<<>}, because the extra \code{<} are ignored.
#' - \code{<\{!>\}>}, because the first \code{>} is canceled.
#' - \code{<!!>}, because the second \code{!} is canceled, allowing the
#'   \code{>} to terminate the garbage.
#' - \code{<!!!>>}, because the second \code{!} and the first \code{>}
#'   are canceled.
#' - \code{<o"i!a,<\{i<a>}, which ends at the first \code{>}.
#'
#' Here are some examples of whole streams and the number of groups they
#' contain:
#'
#' - \code{\{\}}, \code{1} group.
#' - \code{\{\{\{\}\}\}}, \code{3} groups.
#' - \code{\{\{\},\{\}\}}, also \code{3} groups.
#' - \code{\{\{\{\},\{\},\{\{\}\}\}\}}, \code{6} groups.
#' - \code{\{<\{\},\{\},\{\{\}\}>\}}, \code{1} group (which itself contains
#'   garbage).
#' - \code{\{<a>,<a>,<a>,<a>\}}, \code{1} group.
#' - \code{\{\{<a>\},\{<a>\},\{<a>\},\{<a>\}\}}, \code{5} groups.
#' - \code{\{\{<!>\},\{<!>\},\{<!>\},\{<a>\}\}}, \code{2} groups
#'   (since all but the last \code{>} are canceled).
#'
#'
#' Your goal is to find the total score for all groups in your input. Each
#' group is assigned a *score* which is one more than the score of the
#' group that immediately contains it. (The outermost group gets a score of
#' \code{1}.)
#'
#' - \code{\{\}}, score of \code{1}.
#' - \code{\{\{\{\}\}\}}, score of \code{1 + 2 + 3 = 6}.
#' - \code{\{\{\},\{\}\}}, score of \code{1 + 2 + 2 = 5}.
#' - \code{\{\{\{\},\{\},\{\{\}\}\}\}}, score of
#'   \code{1 + 2 + 3 + 3 + 3 + 4 = 16}.
#' - \code{\{<a>,<a>,<a>,<a>\}}, score of \code{1}.
#' - \code{\{\{<ab>\},\{<ab>\},\{<ab>\},\{<ab>\}\}},
#'   score of \code{1 + 2 + 2 + 2 + 2 = 9}.
#' - \code{\{\{<!!>\},\{<!!>\},\{<!!>\},\{<!!>\}\}},
#'   score of \code{1 + 2 + 2 + 2 + 2 = 9}.
#' - \code{\{\{<a!>\},\{<a!>\},\{<a!>\},\{<ab>\}\}}, score of \code{1 + 2 = 3}.
#'
#' *What is the total score* for all groups in your input?
#'
#' **Part Two**
#'
#' Now, you're ready to remove the garbage.
#'
#' To prove you've removed it, you need to count all of the characters within
#' the garbage. The leading and trailing \code{<} and \code{>} don't count, nor
#' do any canceled characters or the \code{!} doing the canceling.
#'
#' -   \code{<>}, \code{0} characters.
#' -   \code{<random characters>}, \code{17} characters.
#' -   \code{<<<<>}, \code{3} characters.
#' -   \code{<\{!>\}>}, \code{2} characters.
#' -   \code{<!!>}, \code{0} characters.
#' -   \code{<!!!>>}, \code{0} characters.
#' -   \code{<\{o"i!a,<\{i<a>}, \code{10} characters.
#'
#' *How many non-canceled characters are within the garbage* in your puzzle
#' input?
#'
#' @rdname day09
#' @export
#' @param stream a string to process
#' @examples
#' process_stream("{{<ab>},{<ab>},{<ab>},{<ab>}}")
#' count_garbage("{{<ab>},{<ab>},{<ab>},{<ab>}}")
process_stream <- function(stream) {
  stream %>% analyze_stream() %>% getElement("points")
}

#' @rdname day09
#' @export
count_garbage <- function(stream) {
  stream %>% analyze_stream() %>% getElement("garbage")
}

analyze_stream <- function(stream) {
  garbage_chars <- 0
  is_garbage <- FALSE
  depth <- 0
  depths <- numeric(0)

  has_more <- function(stream) nchar(stream) != 0
  drop_char1 <- function(stream) substr(stream, 2, nchar(stream))

  while (has_more(stream)) {
    c1 <- substr(stream, 1, 1)
    c2 <- if (nchar(stream) > 1) substr(stream, 2, 2) else ""

    if (c1 == "!") {
      c1 <- ""
      c2 <- ""
      stream <- drop_char1(stream)
    }

    if (!is_garbage) {
      if (c1 == "<") is_garbage <- TRUE
      if (c1 == "{") depth <- depth + 1
      if (c1 == "}") {
        depths <- c(depths, depth)
        depth <- depth - 1
      }
    } else {
      if (c1 == ">") {
        is_garbage <- FALSE
      } else {
        garbage_chars <- garbage_chars + nchar(c1)
      }
    }
    stream <- drop_char1(stream)
  }

  list(points = sum(depths), garbage = garbage_chars)
}
