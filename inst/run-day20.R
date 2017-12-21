library(adventofcode17)
p_strings <- readLines("./inst/input20.txt")
slowest <- find_slowest_particle(p_strings)
stopifnot(slowest == 157)

particles <- create_particles(p_strings)
particles <- search_and_destroy_particles(particles)
stopifnot(length(particles) == 499)
