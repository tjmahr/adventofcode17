library(adventofcode17)

buffer <- spinlock(312, 2017)
p1 <- get_spinlock_value_after_last_insertion(buffer)
stopifnot(p1 == 772)

p2 <- fast_spinlock(312, 50000000)
stopifnot(p2 == 42729050)
