library(adventofcode17)

rules <- readLines("./inst/input21.txt")

rules <- rules %>%
  lapply(expand_rule) %>%
  unlist()

seed <- ".#./..#/###" %>%
  string_to_matrix()

step1 <- enhance_matrix(seed, rules)
step2 <- enhance_matrix(step1, rules)
# step3 <- enhance_matrix(step2, rules)
# step4 <- enhance_matrix(step3, rules)
# step5 <- enhance_matrix(step4, rules)

m <- step2
