library(adventofcode17)

rules <- readLines("./inst/input21.txt")

rules <- rules %>%
  lapply(expand_rule) %>%
  unlist()

seed <- ".#./..#/###" %>%
  string_to_matrix()

step1 <- enhance_matrix(seed, rules)
step2 <- enhance_matrix(step1, rules)
step3 <- enhance_matrix(step2, rules)
step4 <- enhance_matrix(step3, rules)
step5 <- enhance_matrix(step4, rules)
step6 <- enhance_matrix(step5, rules)
step7 <- enhance_matrix(step6, rules)
step8 <- enhance_matrix(step7, rules)

stopifnot(sum(step5 == "#") == aoc17_solutions$day21a)


m <- seed
i <- 0
while (i < 18) {
  message(i)
  message(ncol(m))
  m <- enhance_matrix(m, rules)
  i <- i + 1
}

stopifnot(sum(m == "#") == aoc17_solutions$day21b)
