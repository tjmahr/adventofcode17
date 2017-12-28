context("test-day20.R")

test_that("particle collision", {
  p_strings <- c("p=< 3,0,0>, v=< 2,0,0>, a=<-1,0,0>",
                 "p=< 4,0,0>, v=< 0,0,0>, a=<-2,0,0>")

  p_strings %>%
    find_slowest_particle() %>%
    expect_equal(0)

  p_strings <- c("p=<-6,0,0>, v=< 3,0,0>, a=< 0,0,0>",
                 "p=<-4,0,0>, v=< 2,0,0>, a=< 0,0,0>",
                 "p=<-2,0,0>, v=< 1,0,0>, a=< 0,0,0>",
                 "p=< 3,0,0>, v=<-1,0,0>, a=< 0,0,0>")

  particles <- p_strings %>%
    create_particles()

  particles %>%
    search_and_destroy_particles(min_ticks = 10) %>%
    expect_length(1)
})
