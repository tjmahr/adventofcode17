
#' @export
matrix_to_string <- function(m) {
  m %>%
    apply(1, paste0, collapse = "") %>%
    paste0(collapse = "/")
}

#' @export
string_to_matrix <- function(string) {
  pattern <- string %>%
    strsplit("/") %>%
    unlist() %>%
    strsplit("") %>%
    unlist()

  matrix(pattern, nrow = sqrt(length(pattern)), byrow = TRUE)
}

#' @export
expand_rule <- function(string) {
  parts <- string %>% strsplit(" => ") %>% unlist()
  aliases <- find_rule_aliases(parts[1])
  targets <- rep(parts[2], length(aliases))
  targets %>% as.list() %>% setNames(aliases)
}

#' @export
enhance_matrix <- function(m, rules) {
  if (nrow(m) %% 2 == 0) {
    div_by <- 2
  } else {
    div_by <- 3
  }

  row_seq_starts <- seq(1, nrow(m), by = div_by)
  row_seq_ends <- seq(div_by, nrow(m), by = div_by)
  col_seq_starts <- seq(1, ncol(m), by = div_by)
  col_seq_ends <- seq(div_by, ncol(m), by = div_by)

  rowwise <- Map(seq, row_seq_starts, row_seq_ends)
  colwise <- Map(seq, col_seq_starts, col_seq_ends)

  indices_of_submatrices <- cross(colwise, rowwise)

  sub_matrices <- Map(function(x, y) m[x, y],
                      indices_of_submatrices[[1]],
                      indices_of_submatrices[[2]])


  matrices <- sub_matrices %>%
    lapply(matrix_to_string) %>%
    lapply(function(string) unname(rules[string])) %>%
    lapply(string_to_matrix)

  rows <- list()
  for (seq_run in rowwise) {
    rows <- c(rows, list(do.call(cbind, matrices[seq_run])))
  }
  do.call(rbind, rows)
}

cross <- function(x1, x2) {
  # Some sugar to name to columns in the dataframe
  x1 <- enquo(x1)
  x2 <- enquo(x2)
  x1_value <- rlang::eval_tidy(x1)
  x2_value <- rlang::eval_tidy(x2)

  both <- as.data.frame(expand.grid(x1_value, x2_value))
  attributes(both) <- NULL
  names(both) <- c(rlang::quo_name(x1), rlang::quo_name(x2))
  both
}

find_rule_aliases <- function(pattern) {
  m <- string_to_matrix(pattern)
  permutations <- c(m %>% generate_rotations(),
                    m %>% flip_h() %>% generate_rotations(),
                    m %>% flip_v() %>% generate_rotations(),
                    m %>% flip_h() %>% flip_v() %>% generate_rotations())

  permutations %>%
    lapply(matrix_to_string) %>%
    unique() %>%
    unlist()
}

generate_rotations <- function(m) {
  list(
    m,
    m %>% rotate(),
    m %>% rotate() %>% rotate(),
    m %>% rotate() %>% rotate() %>% rotate())
}

rotate <- function(m) t(apply(m, 2, rev))

flip_h <- function(m) {
  cells <- seq_len(nrow(m))
  m[rev(cells), cells]
}

flip_v <- function(m) {
  cells <- seq_len(nrow(m))
  m[cells, rev(cells)]
}
