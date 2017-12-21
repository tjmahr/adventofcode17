library(adventofcode17)
commands <- readLines("./inst/input18.txt")

machine <- create_solo(commands)

while (length(machine$.messages) == 0) {
  machine$.eval_next()
}

stopifnot(machine$.messages == 1187)


m0 <- create_duet(0, commands)
m1 <- create_duet(1, commands)

# While the two machines have something to do, run each one
# until it is out of moves. Then transmit messages and go again.
while (!in_deadlock(m0, m1)) {
  while (m0$.is_ready()) m0$.eval_next()
  while (m1$.is_ready()) m1$.eval_next()

  m0$.receive(m1$.post())
  m1$.receive(m0$.post())
}

stopifnot(m1$.send_count == 5969)
