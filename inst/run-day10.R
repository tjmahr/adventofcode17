library(adventofcode17)
lengths <- c(227, 169, 3, 166, 246, 201, 0, 47, 1, 255, 2, 254, 96, 3, 97, 144)

args <- list(
 items = 0:255,
 lengths = lengths,
 current_position = 1,
 step = 0
)

results <- twist_knot(args)
prod(results$items[1:2])

stopifnot(prod(results$items[1:2]) == 13760)

lengths <- "227,169,3,166,246,201,0,47,1,255,2,254,96,3,97,144"
result <- knot_hash("227,169,3,166,246,201,0,47,1,255,2,254,96,3,97,144")
result

stopifnot(result == "2da93395f1a6bb3472203252e3b17fe5")
