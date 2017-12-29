#' Day 07: Recursive Circus
#'
#' [Recursive Circus](http://adventofcode.com/2017/day/7)
#'
#' @name day07
#' @rdname day07
#' @details
#'
#' **Part One**
#'
#' Wandering further through the circuits of the computer, you come upon a
#' tower of programs that have gotten
#' themselves into a bit of trouble. A recursive algorithm has gotten out
#' of hand, and now they're balanced precariously in a large tower.
#'
#' One program at the bottom supports the entire tower. It's holding a
#' large disc, and on the disc are balanced several more sub-towers. At the
#' bottom of these sub-towers, standing on the bottom disc, are other
#' programs, each holding *their* own disc, and so on. At the very tops of
#' these sub-sub-sub-...-towers, many programs stand simply keeping the
#' disc below them balanced but with no disc of their own.
#'
#' You offer to help, but first you need to understand the structure of
#' these towers. You ask each program to yell out their *name*, their
#' *weight*, and (if they're holding a disc) the *names of the programs
#' immediately above them* balancing on that disc. You write this
#' information down (your puzzle input). Unfortunately, in their panic,
#' they don't do this in an orderly fashion; by the time you're done,
#' you're not sure which program gave which information.
#'
#' For example, if your list is the following:
#'
#'     pbga (66)
#'     xhth (57)
#'     ebii (61)
#'     havc (66)
#'     ktlj (57)
#'     fwft (72) -> ktlj, cntj, xhth
#'     qoyq (66)
#'     padx (45) -> pbga, havc, qoyq
#'     tknk (41) -> ugml, padx, fwft
#'     jptl (61)
#'     ugml (68) -> gyxo, ebii, jptl
#'     gyxo (61)
#'     cntj (57)
#'
#' ...then you would be able to recreate the structure of the towers that
#' looks like this:
#'
#'                     gyxo
#'                   /
#'              ugml - ebii
#'            /      \
#'           |         jptl
#'           |
#'           |         pbga
#'          /        /
#'     tknk --- padx - havc
#'          \        \
#'           |         qoyq
#'           |
#'           |         ktlj
#'            \      /
#'              fwft - cntj
#'                   \
#'                     xhth
#'
#' In this example, `tknk` is at the bottom of the tower (the *bottom
#' program*), and is holding up `ugml`, `padx`, and `fwft`. Those programs
#' are, in turn, holding up other programs; in this example, none of those
#' programs are holding up any other programs, and are all the tops of
#' their own towers. (The actual tower balancing in front of you is much
#' larger.)
#'
#' Before you're ready to help them, you need to make sure your information
#' is correct. *What is the name of the bottom program?*
#'
#' **Part Two**
#'
#' The programs explain the situation: they can't get down. Rather, they
#' *could* get down, if they weren't expending all of their energy trying
#' to keep the tower balanced. Apparently, one program has the *wrong
#' weight*, and until it's fixed, they're stuck here.
#'
#' For any program holding a disc, each program standing on that disc forms
#' a sub-tower. Each of those sub-towers are supposed to be the same
#' weight, or the disc itself isn't balanced. The weight of a tower is the
#' sum of the weights of the programs in that tower.
#'
#' In the example above, this means that for `ugml`'s disc to be balanced,
#' `gyxo`, `ebii`, and `jptl` must all have the same weight, and they do:
#' `61`.
#'
#' However, for `tknk` to be balanced, each of the programs standing on its
#' disc *and all programs above it* must each match. This means that the
#' following sums must all be the same:
#'
#' -   `ugml` + (`gyxo` + `ebii` + `jptl`) = 68 + (61 + 61 + 61) = 251
#' -   `padx` + (`pbga` + `havc` + `qoyq`) = 45 + (66 + 66 + 66) = 243
#' -   `fwft` + (`ktlj` + `cntj` + `xhth`) = 72 + (57 + 57 + 57) = 243
#'
#' As you can see, `tknk`'s disc is unbalanced: `ugml`'s stack is heavier
#' than the other two. Even though the nodes above `ugml` are balanced,
#' `ugml` itself is too heavy: it needs to be `8` units lighter for its
#' stack to weigh `243` and keep the towers balanced. If this change were
#' made, its weight would be `60`.
#'
#' Given that exactly one program is the wrong weight, *what would its
#' weight need to be* to balance the entire tower?
#'
#' @export
#' @param lines a character vector of program descriptions
#' @examples
#' lines <- "pbga (66)
#'   xhth (57)
#'   ebii (61)
#'   havc (66)
#'   ktlj (57)
#'   fwft (72) -> ktlj, cntj, xhth
#'   qoyq (66)
#'   padx (45) -> pbga, havc, qoyq
#'   tknk (41) -> ugml, padx, fwft
#'   jptl (61)
#'   ugml (68) -> gyxo, ebii, jptl
#'   gyxo (61)
#'   cntj (57)"
#'
#' lines %>%
#'   read_text_lines() %>%
#'   find_root_program()
#'
#' lines %>%
#'   read_text_lines() %>%
#'   find_program_imbalance()
find_root_program <- function(lines) {
  root <- lines %>%
    assemble_program_tree() %>%
    Filter(function(x) x$depth == 1, .)

  root[[1]]
}

#' @rdname day07
#' @export
find_program_imbalance <- function(lines) {
  programs <- lines %>%
    assemble_program_tree() %>%
    analyze_child_weights()

  # Identify weight imbalances in child weights
  check_child_weights <- function(xs) {
    counts <- table(xs)

    if (length(counts) == 1) {
      error <- 0
      program <- NA
    } else {
      oddball_x <- names(counts[counts == 1]) %>% as.numeric()
      common_x <- names(counts[counts != 1]) %>% as.numeric()
      error <- common_x - oddball_x
      program <- names(xs[xs == oddball_x])
    }

    list(program = program, error = error)
  }

  # Recursively crawl down tree following the branch with the weight error
  correct_imbalance <- function(origin, weight_error = 0) {
    program <- programs[[origin]]
    checks <- check_child_weights(program$child_weight)

    if (is.na(checks$program)) {
      correction <- program$weight + weight_error
    } else {
      correction <- correct_imbalance(checks$program, checks$error)
    }
    correction
  }

  root <- keep_if(programs, function(x) x$depth == 1)[[1]]
  correct_imbalance(root$name)
}

# Determine the weight of each programs' children
analyze_child_weights <- function(programs) {
  remove_unnamed <- function(xs) xs[names(xs) != ""]
  unique_names <- function(xs) xs[xs %>% names() %>% unique()]

  # Use a zero placeholder
  for (program_name in names(programs)) {
    programs[[program_name]][["child_weight"]] <- 0
  }

  # Start on leaves
  max_depth <- programs %>%
    lapply(getElement, "depth") %>%
    unlist() %>%
    max()

  # Have each leaf report the weight of it and its children to its parent
  while (max_depth > 1) {
    leaves <- programs %>%
      keep_if(function(x) x$depth == max_depth) %>%
      names()

    for (leaf in leaves) {
      # Collect child weights
      program <- programs[[leaf]]
      uptree_weight <- program$weight + sum(program$child_weight)
      names(uptree_weight) <- program$name

      # Add the child weights, clean up 0 placeholder
      parent <- programs[[program$parent]]
      curr_weights <- c(parent$child_weight, uptree_weight) %>%
        remove_unnamed() %>%
        unique_names()

      programs[[program$parent]][["child_weight"]] <- curr_weights
    }

    # The parents become the leaves
    max_depth <- max_depth - 1
  }

  programs
}

# Create a list of programs from text lines
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
    keep_if(function(x) is.na(x$parent)) %>%
    getElement(1)

  # Recursively mark the depth of each program
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

# Parse a single program directory listing
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
    weight = as.numeric(nw$weight),
    branches = branches
  )
}
