library(adventofcode17)

score <- readLines("./inst/input09.txt") %>% process_stream()
stopifnot(score == 20530)

garbage <- readLines("./inst/input09.txt") %>% count_garbage()
stopifnot(garbage == 9978)
