read_text_lines <- function(x) {
  x %>%
    strsplit("\\n") %>%
    unlist() %>%
    stringr::str_trim() %>%
    Filter(function(x) x != "", .)
}

wrap_around <- function(xs, length) {
  ((xs - 1) %% length) + 1
}

#' @export
`%nin%` <- Negate(`%in%`)

#' @export
keep_if <- function(data, predicate) {
  Filter(predicate, data)
}
