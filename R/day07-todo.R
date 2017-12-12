#' Day X: Title
#'
#' [Title](http://adventofcode.com/2017/day/X)
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
#' @rdname dayXX
#' @export
find_root_program <- function(lines) {
  root <- lines %>%
    assemble_program_tree() %>%
    Filter(function(x) x$depth == 1, .)

  root[[1]]
}

f <- function() {




  set_depths("tknk", 0)
  str(programs)

  leaves <- programs %>%
    lapply(getElement, "branches") %>%
    Filter(function(x) all(is.na(x)), .) %>%
    names()

  for (leaf in leaves) {
    leaf_weight <- programs[[leaf]]
  }



}

assemble_program_tree <- function(lines) {
  programs <- lines %>%
    lapply(read_program_listing) %>%
    setNames(., lapply(., getElement, "name")) %>%
    lapply(modifyList, list(parent = NA))

  # Add the parents
  for (program in programs) {
    if (any(is.na(program$branches))) {
      next
    } else {
      programs[program$branches] <- programs[program$branches] %>%
        lapply(modifyList, list(parent = program$name))
    }
  }

  root <- programs %>%
    Filter(function(x) is.na(x$parent), .) %>%
    getElement(1)

  # Recursive mark the depth of each program
  set_depths <- function(name, current_depth) {
    programs[[name]][["depth"]] <<- current_depth + 1
    program <- programs[[name]]

    if (!all(is.na(program$branches))) {
      lapply(program$branches, set_depths, current_depth + 1)
    }
    invisible(NULL)
  }

  set_depths(root$name, 0)
  programs
}

read_program_listing <- function(line) {
  nw <- stringr::str_match(line, "(\\w+) [(](\\d+)[)]") %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    setNames(c("line", "name", "weight"))

  branches <- stringr::str_extract(line, "->.+") %>%
    stringr::str_replace("-> ", "") %>%
    strsplit(", ") %>%
    unlist()

  list(
    name = nw$name,
    weight = nw$weight,
    branches = branches
  )
}
