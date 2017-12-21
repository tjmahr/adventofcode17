read_text_lines <- function(x) {
  x %>%
    strsplit("\\n") %>%
    unlist() %>%
    stringr::str_trim() %>%
    Filter(function(x) x != "", .)
}

wrap_around <- function(xs, length) {
  ((xs - 1) %% length) + 1
}

wrap_around2 <- function(xs, y) {
  ((xs - 1) %% length(y)) + 1
}

insert_value <- function(vector, position, value) {
  start <- utils::head(vector, position - 1)
  rest <- utils::tail(vector, -position + 1)
  c(start, value, rest)
}

#' @export
`%nin%` <- Negate(`%in%`)

#' @export
keep_if <- function(data, predicate) {
  Filter(predicate, data)
}

which_min <- function(xs) {
  which(xs == min(xs))
}

#' Convert a vector of integers to a vector of strings of n binary digits
#'
#' @param xs a vector of integers
#' @param n number of the highest bit. Defaults to 16.
#' @param start number of the lowest bit. Defaults to 1, so all bits up to the
#'   nth are selected.
#' @return a character vector of binary strings
#' @export
int_to_n_bits <- function(xs, n = 16, start = 1) {
  indices <- seq_len(length(xs))
  # intToBits(xs) returns 32 bits for each number of xs and each bit has a
  # leading zero like 01 00 00 00. The item on the left is the smallest digit.
  #
  # as.integer() will remove the leading zeros.
  #
  # The rep() lines produces 1 repeated 32 times followed by 2 repeated 32
  # times, etc. Splitting on these values will get us a list where each element
  # in the list are the  binary digits for each number.
  bits <- split(as.integer(intToBits(xs)), rep(indices, each = 32))

  # Here we convert the vectors of binary digits into strings. Using `n:start`
  # will reverse the digits so the smallest digit is on the right.
  vapply(bits, function(x) paste0(x[n:start], collapse = ""),
         character(1), USE.NAMES = FALSE)

}


# Too slow versions
old_int_to_n_bits <- function(xs, n = 16, start = 1) {
  vapply(xs, int_to_n_bits_one, FUN.VALUE = character(1),
         n = n, start = start, USE.NAMES = FALSE)
}

# Convert an integer to a string of n binary digits
old_int_to_n_bits_one <- function(x, n = 16, start = 1) {
  # I used pipes here before but those had a huge hit on performance for the
  # problem with 40,000,000 numbers to compare
  sequence <- seq(n, start, by = -1)
  bits <- as.integer(intToBits(x))
  paste0(bits[sequence], collapse = "")
}

