#


convert_matrix_to_rule <- function(m) {
  m %>%
    apply(1, paste0, collapse = "") %>%
    paste0(collapse = "/")
}

generate_rotations <- function(m) {
  list(
    m,
    m %>% rotate(),
    m %>% rotate() %>% rotate(),
    m %>% rotate() %>% rotate() %>% rotate())
}


string_to_matrix <- function(string) {
  pattern <- string %>%
    strsplit("/") %>%
    unlist() %>%
    strsplit("") %>%
    unlist()

  matrix(pattern, nrow = sqrt(length(pattern)), byrow = TRUE)
}

find_rule_aliases <- function(pattern) {
  m <- string_to_matrix(pattern)
  permutations <- c(m %>% generate_rotations(),
                    m %>% flip_h() %>% generate_rotations(),
                    m %>% flip_v() %>% generate_rotations(),
                    m %>% flip_h() %>% flip_v() %>% generate_rotations())

  permutations %>%
    lapply(convert_matrix_to_rule) %>%
    unique() %>%
    unlist()
}

rules <- c(
  "../.# => ##./#../...",
  ".#./..#/### => #..#/..../..../#..#"
)

expand_rule <- function(string) {
  parts <- string %>% strsplit(" => ") %>% unlist()
  aliases <- find_rule_aliases(parts[1])
  targets <- rep(parts[2], length(aliases))
  targets %>% as.list() %>% setNames(aliases)
}

rules <- rules %>% lapply(expand_rule) %>% unlist
start <- ".#./..#/###"

curr_rule <- rules[start]
result <- unname(curr_rule)

next_m <- result %>% string_to_matrix()

if (nrow(next_m) %% 2 == 0) {
  div_by <- 2
} else {
  div_by <- 3
}

seq_starts <- seq(1, nrow(next_m), by = div_by)
seq_ends <- seq(div_by, nrow(next_m), by = div_by)

a <- Map(seq, seq_starts, seq_ends)
b <- Map(seq, seq_starts, seq_ends)

for (x in a) {
  for (y in b) {
    message(next_m[x, y])
  }
}











flip_h <- function(m) {
  cells <- seq_len(nrow(m))
  m[rev(cells), cells]
}

flip_v <- function(m) {
  cells <- seq_len(nrow(m))
  m[cells, rev(cells)]
}

rotate <- function(m) t(apply(m, 2, rev))


t(m)
