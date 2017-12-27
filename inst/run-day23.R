library(adventofcode17)
commands <- readLines("./inst/input23.txt")

m <- create_coprocessor(commands)
while (m$.has_next()) suppressMessages(m$.eval_next())

stopifnot(m$.counts["mul"] == 3025)



# I solved it by studying and annotating the code. Crucially, h increments if f
# is 0 which happens if d*e ever equals b. All values of d and e from 2 to b are
# used, so d*e will never equal b if b is prime. So h does not increment if b is
# prime. b starts at 105700 and increases by 17 until it is 122700---that is, b
# has 1000 different values.

# 01 set b 57
# 02 set c b
# 03 jnz a 2         03-05
# 04 jnz 1 5         04-09
# 05 mul b 100
# 06 sub b -100000
# 07 set c b
# 08 sub c -17000
#   09 set f 1
#   10 set d 2
#     11 set e 2
#       12 set g d
#       13 mul g e
#       14 sub g b
#       15 jnz g 2         15-17
#         16 set f 0       !!! set f to 0 if (d*e - b) is 0
#       17 sub e -1
#       18 set g e
#       19 sub g b         keep looping until e is as big as b
#       20 jnz g -8        20-12
#     21 sub d -1
#     22 set g d
#     23 sub g b           keep looping until d is as big as b
#     24 jnz g -13         24-11
#   25 jnz f 2             25-27
#   26 sub h -1            increase h if f is 0
#   27 set g b
#   28 sub g c             keep looping until b is as big as c
#   29 jnz g 2             29-31
#   30 jnz 1 3             30-EXIT
#   31 sub b -17
#   32 jnz 1 -23           32-09


# m2$e <- 105700 - 1
m2 <- create_coprocessor(c("set a 1", commands))

m2$.eval_next(2)
m2$.eval_next(2)
m2$.eval_next(2)
m2$.eval_next(2)
m2$.eval_next(2)
m2$.eval_next(2)
m2$.eval_next(2)
m2$.eval_next(2)
m2$.eval_next(2)

is_prime <- function(x) length(unlist(sfsmisc::factorize(x))) == 2
b_values <- seq(105700, 122700, by = 17)
n_primes <- lapply(b_values, is_prime) %>% unlist() %>% sum()

# The guess 1000 - n_primes was wrong so I tried again to see if I had an
# off-by-one error somewhere, so that's how I came up with this answer.
1000 - n_primes + 1
