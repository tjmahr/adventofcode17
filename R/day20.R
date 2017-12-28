#' Day 20: Particle Swarm
#'
#' [Particle Swarm](http://adventofcode.com/2017/day/20)
#'
#' @details
#'
#' **Part One**
#'
#' Suddenly, the GPU contacts you, asking for help.
#' Someone has asked it to simulate *too many particles*, and it won't be
#' able to finish them all in time to render the next frame at this rate.
#'
#' It transmits to you a buffer (your puzzle input) listing each particle
#' in order (starting with particle `0`, then particle `1`, particle `2`,
#' and so on). For each particle, it provides the `X`, `Y`, and `Z`
#' coordinates for the particle's position (`p`), velocity (`v`), and
#' acceleration (`a`), each in the format `<X,Y,Z>`.
#'
#' Each tick, all particles are updated simultaneously. A particle's
#' properties are updated in the following order:
#'
#' -   Increase the `X` velocity by the `X` acceleration.
#' -   Increase the `Y` velocity by the `Y` acceleration.
#' -   Increase the `Z` velocity by the `Z` acceleration.
#' -   Increase the `X` position by the `X` velocity.
#' -   Increase the `Y` position by the `Y` velocity.
#' -   Increase the `Z` position by the `Z` velocity.
#'
#' Because of seemingly tenuous rationale involving
#' [z-buffering](https://en.wikipedia.org/wiki/Z-buffering), the GPU would
#' like to know which particle will stay closest to position `<0,0,0>` in
#' the long term. Measure this using the [Manhattan
#' distance](https://en.wikipedia.org/wiki/Taxicab_geometry), which in this
#' situation is simply the sum of the absolute values of a particle's `X`,
#' `Y`, and `Z` position.
#'
#' For example, suppose you are only given two particles, both of which
#' stay entirely on the X-axis (for simplicity). Drawing the current states
#' of particles `0` and `1` (in that order) with an adjacent a number line
#' and diagram of current `X` positions (marked in parenthesis), the
#' following would take place:
#'
#'     p=< 3,0,0>, v=< 2,0,0>, a=<-1,0,0>    -4 -3 -2 -1  0  1  2  3  4
#'     p=< 4,0,0>, v=< 0,0,0>, a=<-2,0,0>                         (0)(1)
#'
#'     p=< 4,0,0>, v=< 1,0,0>, a=<-1,0,0>    -4 -3 -2 -1  0  1  2  3  4
#'     p=< 2,0,0>, v=<-2,0,0>, a=<-2,0,0>                      (1)   (0)
#'
#'     p=< 4,0,0>, v=< 0,0,0>, a=<-1,0,0>    -4 -3 -2 -1  0  1  2  3  4
#'     p=<-2,0,0>, v=<-4,0,0>, a=<-2,0,0>          (1)               (0)
#'
#'     p=< 3,0,0>, v=<-1,0,0>, a=<-1,0,0>    -4 -3 -2 -1  0  1  2  3  4
#'     p=<-8,0,0>, v=<-6,0,0>, a=<-2,0,0>                         (0)
#'
#' At this point, particle `1` will never be closer to `<0,0,0>` than
#' particle `0`, and so, in the long run, particle `0` will stay closest.
#'
#' *Which particle will stay closest to position `<0,0,0>`* in the long
#' term?
#'
#' **Part Two**
#'
#' To simplify the problem further, the GPU would like to remove any
#' particles that *collide*. Particles collide if their positions ever
#' *exactly match*. Because particles are updated simultaneously, *more
#' than two particles* can collide at the same time and place. Once
#' particles collide, they are removed and cannot collide with anything
#' else after that tick.
#'
#' For example:
#'
#'     p=<-6,0,0>, v=< 3,0,0>, a=< 0,0,0>
#'     p=<-4,0,0>, v=< 2,0,0>, a=< 0,0,0>    -6 -5 -4 -3 -2 -1  0  1  2  3
#'     p=<-2,0,0>, v=< 1,0,0>, a=< 0,0,0>    (0)   (1)   (2)            (3)
#'     p=< 3,0,0>, v=<-1,0,0>, a=< 0,0,0>
#'
#'     p=<-3,0,0>, v=< 3,0,0>, a=< 0,0,0>
#'     p=<-2,0,0>, v=< 2,0,0>, a=< 0,0,0>    -6 -5 -4 -3 -2 -1  0  1  2  3
#'     p=<-1,0,0>, v=< 1,0,0>, a=< 0,0,0>             (0)(1)(2)      (3)
#'     p=< 2,0,0>, v=<-1,0,0>, a=< 0,0,0>
#'
#'     p=< 0,0,0>, v=< 3,0,0>, a=< 0,0,0>
#'     p=< 0,0,0>, v=< 2,0,0>, a=< 0,0,0>    -6 -5 -4 -3 -2 -1  0  1  2  3
#'     p=< 0,0,0>, v=< 1,0,0>, a=< 0,0,0>                       X (3)
#'     p=< 1,0,0>, v=<-1,0,0>, a=< 0,0,0>
#'
#'     ------destroyed by collision------
#'     ------destroyed by collision------    -6 -5 -4 -3 -2 -1  0  1  2  3
#'     ------destroyed by collision------                      (3)
#'     p=< 0,0,0>, v=<-1,0,0>, a=< 0,0,0>
#'
#' In this example, particles `0`, `1`, and `2` are simultaneously
#' destroyed at the time and place marked `X`. On the next tick, particle
#' `3` passes through unharmed.
#'
#' *How many particles are left* after all collisions are resolved?
#'
#' @rdname day20
#' @export
#' @param p_strings lines of text describing particles
#' @examples
#' p_strings <- c("p=<-6,0,0>, v=< 3,0,0>, a=< 0,0,0>",
#'                "p=<-4,0,0>, v=< 2,0,0>, a=< 0,0,0>",
#'                "p=<-2,0,0>, v=< 1,0,0>, a=< 0,0,0>",
#'                "p=< 3,0,0>, v=<-1,0,0>, a=< 0,0,0>")
#' find_slowest_particle(p_strings)
#'
#' particles <- p_strings %>%
#'   create_particles()
#'
#' particles %>%
#'   search_and_destroy_particles(min_ticks = 10)
find_slowest_particle <- function(p_strings) {
  # Solve with calculus! Kinda!
  particles <- lapply(p_strings, string_to_particle)

  # Find particles with the lowest accelerations
  min_acc <- particles %>%
    lapply(function(x) sum(abs(x$a))) %>%
    unlist() %>%
    which_min()

  # Find remaining particles with the lowest velocities
  curr_min_vel <- particles[min_acc] %>%
    lapply(function(x) sum(abs(x$v))) %>%
    unlist() %>%
    which_min()

  min_vel <- min_acc[curr_min_vel]

  # Find remaining particles with the closest position
  curr_min_pos <- particles[min_vel] %>%
    lapply(function(x) sum(abs(x$p))) %>%
    unlist() %>%
    which_min()

  # Convert to 0-based indexing
  min_vel[curr_min_pos] - 1
}

#' @rdname day20
#' @export
create_particles <- function(p_strings) {
  p_strings %>%
    lapply(string_to_particle)
}

#' @rdname day20
#' @export
#' @param particles particle objects
#' @param min_ticks simulate the swarm for at least this many steps
search_and_destroy_particles <- function(particles, min_ticks = 10) {
  ticks <- find_possible_collision_time(particles)
  if (is.na(ticks)) ticks <- min_ticks
  while (!is.na(ticks) & ticks > 0) {
    particles <- collide_particles(particles, ticks)
    ticks <- find_possible_collision_time(particles)
  }
  particles
}

# Search pairs of particles until we find a pair that is set to collide. This
# function keeps us from ticking forever, because an NA result from this
# function means that no pair of particles will ever collide.
find_possible_collision_time <- function(particles) {
  time <- NA
  n <- length(particles)
  for (i in seq_len(n - 1)) {
    for (j in seq(from = i + 1, n)) {
      time <- when_collide(particles[[i]], particles[[j]])
      if (!is.na(time)) break
    }
    if (!is.na(time)) break
  }
  time
}

collide_particles <- function(particles, ticks) {
  for (i in seq_len(ticks)) {
    particles <- lapply(particles, tick)
    ps <- lapply(particles, getElement, "p")

    # Ugh I lost the most time here because I used the wrong function for
    # detecting which elements were duplicated.
    dupes <- which(duplicated(ps))
    if (length(dupes) != 0) {
      counters <- which(duplicated(ps, fromLast = TRUE))
      to_drop <- unique(c(dupes, counters))
      particles <- particles[-c(to_drop)]
    }
  }
  particles
}

string_to_particle <- function(string) {
  x <- as.numeric(unlist(stringr::str_extract_all(string, "-?\\d+")))
  particle(p = x[1:3], v = x[4:6], a = x[7:9])
}

particle <- function(p, v, a) {
  structure(list(p = p, v = v, a = a), class = c("particle", "list"))
}

print.particle <- function(x, ...) str(x)

tick <- function(p) {
  p$v <- p$v + p$a
  p$p <- p$p + p$v
  p
}

# The general solution for the position n of a particle at time t is
#
#   p_path(t, n) = .5at(t + 1) + vt + p
#   where n is the x, y, or z dimension
#
# or more explicitly
#
#   p_path(t, n) = .5 * a * t * (t + 1) + v * t + p
#
# We can find if/when the paths of p1 and p2 collide using
#
#  p1_path(t, x) - p2_path(t, x) = 0
#  p1_path(t, y) - p2_path(t, y) = 0
#  p1_path(t, z) - p2_path(t, z) = 0
#
# And using the discriminant/quadratic formula on each equation.
when_collide <- function(p1, p2) {
  as <- unlist(sub_pairs(lapply(list(p1, p2), compute_as)))
  bs <- unlist(sub_pairs(lapply(list(p1, p2), compute_vs)))
  cs <- unlist(sub_pairs(lapply(list(p1, p2), compute_ps)))

  # Bail if any of the discriminants are bad
  if (any(discriminant(as, bs, cs) < 0)) {
    answer <- NA_real_
  } else {
    r1 <- (-bs + sqrt(discriminant(as, bs, cs))) / (2 * as)
    r2 <- (-bs - sqrt(discriminant(as, bs, cs))) / (2 * as)

    # If a is 0, there was a division by 0. Find root manually instead.
    if (any(as == 0)) {
      r1[which(as == 0)] <- -1 * cs[which(as == 0)] / bs[which(as == 0)]
      r2[which(as == 0)] <- -1 * cs[which(as == 0)] / bs[which(as == 0)]
    }

    xs <- c(r1[1], r2[1])
    ys <- c(r1[2], r2[2])
    zs <- c(r1[3], r2[3])

    if (length(xs[xs > 0 & xs %in% ys & xs %in% zs]) == 0) {
      answer <- NA_real_
    } else {
      answer <- min(xs[xs > 0 & xs %in% ys & xs %in% zs])
    }
  }
  answer
}

discriminant <- function(as, bs, cs) {
  (bs ^ 2) - (4 * as * cs)
}

compute_as <- function(p) p$a * .5
compute_vs <- function(p) (p$a * .5) + p$v
compute_ps <- function(p) p$p

sub_pairs <- function(x) {
  Map(function(x, y) x - y, x[[1]], x[[2]])
}

