#' Day 04: High-Entropy Passphrases
#'
#' [High-Entropy Passphrases](http://adventofcode.com/2017/day/4)
#'
#' @details
#'
#' **Part One**
#'
#' A new system policy has been put in place that requires all accounts to
#' use a *passphrase* instead of simply a pass*word*. A passphrase consists
#' of a series of words (lowercase letters) separated by spaces.
#'
#' To ensure security, a valid passphrase must contain no duplicate words.
#'
#' For example:
#'
#' -   `aa bb cc dd ee` is valid.
#' -   `aa bb cc dd aa` is not valid - the word `aa` appears more than
#'     once.
#' -   `aa bb cc dd aaa` is valid - `aa` and `aaa` count as different
#'     words.
#'
#' The system's full passphrase list is available as your puzzle input.
#' *How many passphrases are valid?*
#'
#' **Part Two**
#'
#' For added security, yet another system policy
#' has been put in place. Now, a valid passphrase must contain no two words
#' that are anagrams of each other - that is, a passphrase is invalid if
#' any word's letters can be rearranged to form any other word in the
#' passphrase.
#'
#' For example:
#'
#' -   `abcde fghij` is a valid passphrase.
#' -   `abcde xyz ecdab` is not valid - the letters from the third word can
#'     be rearranged to form the first word.
#' -   `a ab abc abd abf abj` is a valid passphrase, because *all* letters
#'     need to be used when forming another word.
#' -   `iiii oiii ooii oooi oooo` is valid.
#' -   `oiii ioii iioi iiio` is not valid - any of these words can be
#'     rearranged to form any other word.
#'
#' Under this new system policy, *how many passphrases are valid?*
#'
#' @rdname day04
#' @export
#' @param passphrases a string of passphrases
#' @param x a passphrase to check
#' @param rule a function for checking passphrases
#' @examples
#' ps <- "abcde xyz ecdab\niiii oiii ooii oooi oooo\naa bb cc dd aa"
#' count_valid_passphrases(ps, no_repeated_words)
#' count_valid_passphrases(ps, no_anagrams)
count_valid_passphrases <- function(passphrases, rule = no_repeated_words) {
  lines <- read_text_lines(passphrases)
  sum(rule(lines))
}

#' @rdname day04
#' @export
no_repeated_words <- function(x) {
  strsplit(x, " ") %>%
    lapply(table) %>%
    lapply(function(xs) all(xs == 1)) %>%
    unlist()
}

#' @rdname day04
#' @export
no_anagrams <- function(x) {
  strsplit(x, " ") %>%
    lapply(str_sort_chars) %>%
    lapply(table) %>%
    lapply(function(xs) all(xs == 1)) %>%
    unlist()
}

# sort each string a vector
str_sort_chars <- function(string) {
  string %>%
    lapply(sort_a_string) %>%
    unlist()
}

# sort a single string
sort_a_string <- function(string) {
  string %>%
    str_tokenize() %>%
    unlist() %>%
    sort() %>%
    paste0(collapse = "")
}
