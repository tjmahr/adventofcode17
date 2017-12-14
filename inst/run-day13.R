library(adventofcode17)

lines <- readLines("./inst/input13.txt")
severity <- calculate_firewall_severity(lines)
stopifnot(severity == 1316)

# Wow, this is slooow. I probably could do an analytically thing to skip delays
# every other delay if 2 is a range or every 4th if 3 is a range, etc.
delay <- determine_firewall_delay(lines, start_delay = 3840050)
stopifnot(delay == 3840052)



# Actually that's what I'll do to speed up the process...

# Generate a sequence of illegal scanner times
seq_scanner <- function(range, from, to, depth = 1) {
  illegal_times <- seq(from = from, to = to, by = 2 * (range - 1)) - (depth - 1)
  illegal_times[illegal_times > 0]
}

# Start with a search space of 10 million. For each scanner, remove its illegal
# times
scanners <- lines %>% lapply(adventofcode17:::parse_scanner_line)
candidates <- 1:10000000

for (scanner in scanners) {
  illegal <- seq_scanner(scanner$range, 1, 10000000, depth = scanner$depth + 1)
  candidates <- candidates[candidates %nin% illegal]
}
candidates

