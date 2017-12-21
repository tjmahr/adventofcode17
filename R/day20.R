#' Day 20: Title
#'
#' [Title](http://adventofcode.com/2017/day/20)
#'
#' @details
#'
#' **Part One**
#'
#'
#'
#' **Part Two**
#'
#'
#' @rdname day20
#' @export
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
search_and_destroy_particles <- function(particles) {
  ticks <- find_possible_collision_time(particles)
  while (!is.na(ticks)) {
    particles <- collide_particles(particles, ticks)
    ticks <- find_possible_collision_time(particles)
  }
  particles
}

# Search pairs of particles until we find a pair that is set to collide. This
# function keeps us from ticking forever, because an NA result from this
# function means that no pair of particles will ever collide.
find_possible_collision_time <- function(particles) {
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

