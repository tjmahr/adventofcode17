context("test-day18.R")

test_that("updating a solo machine", {
  commands <- "set a 1
  add a 2
  mul a a
  mod a 5
  snd a
  set a 0
  rcv a
  jgz a -1
  set a 1
  jgz a -2"

  machine <- commands %>%
    read_text_lines() %>%
    create_solo()

  # "The first four instructions set a to 1, add 2 to it, square it, and then
  # set it to itself modulo 5, resulting in a value of 4."
  machine$.eval_next()
  machine$.eval_next()
  machine$.eval_next()
  machine$.eval_next()
  expect_equal(machine$a, 4)

  # "Then, a sound with frequency 4 (the value of a) is played."
  machine$.eval_next()
  expect_equal(machine$.last_sound, 4)

  # "After that, a is set to 0, causing the subsequent rcv and jgz instructions
  # to both be skipped (rcv because a is 0, and jgz because a is not greater
  # than 0)."
  machine$.eval_next()
  machine$.eval_next()
  machine$.eval_next()
  expect_length(machine$.messages, 0)

  # "Finally, a is set to 1, causing the next jgz instruction to activate,
  # jumping back two instructions to another jump, which jumps again to the rcv,
  # which ultimately triggers the recover operation."
  machine$.eval_next()
  machine$.eval_next()
  machine$.eval_next()
  machine$.eval_next()
  expect_equal(machine$.messages, 4)
})

test_that("perform a machine duet", {
  commands <- "
  snd 1
  snd 2
  snd p
  rcv a
  rcv b
  rcv c
  rcv d"
  commands <- read_text_lines(commands)

  m0 <- create_duet(0, commands)
  m1 <- create_duet(1, commands)

  while (!in_deadlock(m0, m1)) {
    m0$.eval_next()
    m1$.eval_next()
    m0$.receive(m1$.post())
    m1$.receive(m0$.post())
  }
  expect_equal(m0$a, 1)
  expect_equal(m0$b, 2)
  expect_equal(m0$c, 1)
  expect_equal(m1$a, 1)
  expect_equal(m1$b, 2)
  expect_equal(m1$c, 0)
  expect_equal(m1$.send_count, 3)
T})
