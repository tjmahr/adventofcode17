library(adventofcode17)
input <- readLines("./inst/input24.txt")

# Start at three different roots so that the recursion doesn't take forever. (It
# still takes a really long time.)
r1 <- connect_free_pieces("0/39", 39, drop_first_instance(input, "0/39"))
r2 <- connect_free_pieces("0/43", 43, drop_first_instance(input, "0/43"))
r3 <- connect_free_pieces("0/45", 45, drop_first_instance(input, "0/45"))

r1_champ <- find_strongest_bridge(r1)
r2_champ <- find_strongest_bridge(r2)
r3_champ <- find_strongest_bridge(r3)

champ <- find_strongest_bridge(c(r1_champ, r2_champ, r3_champ))

stopifnot(compute_bridge_strength(champ) == aoc17_solutions$day24a)


r1_lengths <- find_longest_bridge(r1)
r2_lengths <- find_longest_bridge(r2)
r3_lengths <- find_longest_bridge(r3)

longest <- find_longest_bridge(c(r1_lengths, r2_lengths, r3_lengths))
length_champ <- find_strongest_bridge(longest)

stopifnot(compute_bridge_strength(length_champ) == aoc17_solutions$day24b)
