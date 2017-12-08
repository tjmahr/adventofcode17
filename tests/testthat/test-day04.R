context("test-day04.R")

test_that("passphrase validator", {
  # With no repeated tokens rule
  "aa bb cc dd ee" %>% no_repeated_words() %>% expect_true()
  "aa bb cc dd aa" %>% no_repeated_words() %>% expect_false()
  "aa bb cc dd aaa" %>% no_repeated_words() %>% expect_true()

  "
  aa bb cc dd ee
  aa bb cc dd aa
  aa bb cc dd aaa
  " %>%
    count_valid_passphrases(rule = no_repeated_words) %>%
    expect_equal(2)

  # With no anagram rule
  "abcde fghij" %>% no_anagrams() %>% expect_true()
  "abcde xyz ecdab" %>% no_anagrams() %>% expect_false()
  "a ab abc abd abf abj" %>% no_anagrams() %>% expect_true()
  "iiii oiii ooii oooi oooo" %>% no_anagrams() %>% expect_true()
  "oiii ioii iioi iiio" %>% no_anagrams() %>% expect_false()

  "
  abcde fghij
  abcde xyz ecdab
  a ab abc abd abf abj
  iiii oiii ooii oooi oooo
  oiii ioii iioi iiio
  " %>%
    count_valid_passphrases(rule = no_anagrams) %>%
    expect_equal(3)
})
