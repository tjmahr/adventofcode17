context("test-day07.R")

test_that("analyzing a program tree", {
  lines <- "pbga (66)
  xhth (57)
  ebii (61)
  havc (66)
  ktlj (57)
  fwft (72) -> ktlj, cntj, xhth
  qoyq (66)
  padx (45) -> pbga, havc, qoyq
  tknk (41) -> ugml, padx, fwft
  jptl (61)
  ugml (68) -> gyxo, ebii, jptl
  gyxo (61)
  cntj (57)" %>%
    read_text_lines()

  lines %>%
    find_root_program() %>%
    getElement("name") %>%
    expect_equal("tknk")

  lines %>%
    find_program_imbalance() %>%
    expect_equal(60)
})
