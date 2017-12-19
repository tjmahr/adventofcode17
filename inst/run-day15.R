library(adventofcode17)

# Generator A starts with 703
# Generator B starts with 516
a <- create_generator(seed = 703, factor = 16807, divisor = 2147483647)
b <- create_generator(seed = 516, factor = 48271, divisor = 2147483647)

# This takes forever!
seriously <- FALSE
if (seriously) {
  p1_matches <- judge_generators(a, b, 40000000)
  stopifnot(p1_matches == 594)
}

a <- create_generator(seed = 703, factor = 16807, divisor = 2147483647,
                      criterion = is_divisible_by_4)
b <- create_generator(seed = 516, factor = 48271, divisor = 2147483647,
                      criterion = is_divisible_by_8)

if (seriously) {
  p2_matches <- judge_generators(a, b, 5000000)
  stopifnot(p2_matches == 328)
}
