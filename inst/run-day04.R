library(adventofcode17)
x <- readLines("./inst/input04.txt")

p1 <- x %>% count_valid_passphrases(rule = no_repeated_words)
p2 <- x %>% count_valid_passphrases(rule = no_anagrams)

stopifnot(p1 == 383)
stopifnot(p2 == 265)
