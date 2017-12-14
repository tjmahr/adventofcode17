#' @export
generate_grid_hashes <- function(key, seq = 0:127) {
  paste0(key, "-", seq) %>%
    lapply(knot_hash) %>%
    lapply(convert_knot_hash_to_bits) %>%
    unlist()
}

#' @export
str_sum_ones <- function(string) {
  string %>%
    strsplit("") %>%
    unlist() %>%
    as.numeric() %>%
    sum()
}

convert_knot_hash_to_bits <- function(string) {
  string %>%
    strsplit("") %>%
    unlist() %>%
    strtoi(16) %>%
    lapply(int_to_bits) %>%
    unlist() %>%
    paste0(collapse = "")
}

int_to_bits <- function(x) {
  bits <- x %>% intToBits() %>% as.integer()
  bits[1:4] %>% rev() %>% paste0(collapse = "")
}

grid_to_binary <- function(xs) {
  chars <- xs %>% strsplit("") %>% unlist()
  bits <- ifelse(chars == "#", 1, 0)
  paste0(bits, collapse = "")
}

binary_to_grid <- function(xs) {
  chars <- xs %>% strsplit("") %>% unlist()
  bits <- ifelse(chars == "1", "#", ".")
  paste0(bits, collapse = "")
}
