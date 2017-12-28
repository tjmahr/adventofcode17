# Begin in state A.
# Perform a diagnostic checksum after 12172063 steps.
#
# In state A:
#   If the current value is 0:
#   - Write the value 1.
# - Move one slot to the right.
# - Continue with state B.
# If the current value is 1:
#   - Write the value 0.
# - Move one slot to the left.
# - Continue with state C.
#
# In state B:
#   If the current value is 0:
#   - Write the value 1.
# - Move one slot to the left.
# - Continue with state A.
# If the current value is 1:
#   - Write the value 1.
# - Move one slot to the left.
# - Continue with state D.
#
# In state C:
#   If the current value is 0:
#   - Write the value 1.
# - Move one slot to the right.
# - Continue with state D.
# If the current value is 1:
#   - Write the value 0.
# - Move one slot to the right.
# - Continue with state C.
#
# In state D:
#   If the current value is 0:
#   - Write the value 0.
# - Move one slot to the left.
# - Continue with state B.
# If the current value is 1:
#   - Write the value 0.
# - Move one slot to the right.
# - Continue with state E.
#
# In state E:
#   If the current value is 0:
#   - Write the value 1.
# - Move one slot to the right.
# - Continue with state C.
# If the current value is 1:
#   - Write the value 1.
# - Move one slot to the left.
# - Continue with state F.
#
# In state F:
#   If the current value is 0:
#   - Write the value 1.
# - Move one slot to the left.
# - Continue with state E.
# If the current value is 1:
#   - Write the value 1.
# - Move one slot to the right.
# - Continue with state A.
library(adventofcode17)

rules <- list(
  create_tm_rule("A", "0", "1", "R", "B"),
  create_tm_rule("A", "1", "0", "L", "C"),
  create_tm_rule("B", "0", "1", "L", "A"),
  create_tm_rule("B", "1", "1", "L", "D"),
  create_tm_rule("C", "0", "1", "R", "D"),
  create_tm_rule("C", "1", "0", "R", "C"),
  create_tm_rule("D", "0", "0", "L", "B"),
  create_tm_rule("D", "1", "0", "R", "E"),
  create_tm_rule("E", "0", "1", "R", "C"),
  create_tm_rule("E", "1", "1", "L", "F"),
  create_tm_rule("F", "0", "1", "L", "E"),
  create_tm_rule("F", "1", "1", "R", "A")
)

m1 <- turing_machine(rules, "A")

i <- 0L
while (i < 12172063L) {
  if (i %% 100000L == 0L) message(i)
  m1$step()
  i <- i + 1L
}

stopifnot(m1$checksum() == aoc17_solutions$day25a)
