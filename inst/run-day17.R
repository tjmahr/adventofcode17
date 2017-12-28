library(adventofcode17)

buffer <- spinlock(312, 2017)
p1 <- get_spinlock_value_after_last_insertion(buffer)
stopifnot(p1 == aoc17_solutions$day17a)

p2 <- fast_spinlock_after_zero(312, 50000000)
stopifnot(p2 == aoc17_solutions$day17b)
