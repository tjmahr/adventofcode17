% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/day02.R
\name{day02}
\alias{day02}
\alias{spreadsheet_checksum}
\alias{evenly_divisible_pair}
\alias{max_min_pair}
\title{Day 02: Corruption Checksum}
\usage{
spreadsheet_checksum(spreadsheet, f_filter, f_combine)

evenly_divisible_pair(x)

max_min_pair(x)
}
\arguments{
\item{spreadsheet}{a single string with a spreadsheet}

\item{f_filter}{a function to find a pair on each spreadsheet row}

\item{f_combine}{a function to apply on the pair from each line}

\item{x}{a vector of integers to filter}
}
\description{
\href{http://adventofcode.com/2017/day/2}{Corruption Checksum}
}
\details{
\strong{Part One}

As you walk through the door, a glowing humanoid shape yells in your
direction. "You there! Your state appears to be idle. Come help us
repair the corruption in this spreadsheet - if we take another
millisecond, we'll have to display an hourglass cursor!"

The spreadsheet consists of rows of apparently-random numbers. To make
sure the recovery process is on the right track, they need you to
calculate the spreadsheet's \emph{checksum}. For each row, determine the
difference between the largest value and the smallest value; the
checksum is the sum of all of these differences.

For example, given the following spreadsheet:\preformatted{5 1 9 5
7 5 3
2 4 6 8
}
\itemize{
\item The first row's largest and smallest values are \code{9} and \code{1}, and
their difference is \code{8}.
\item The second row's largest and smallest values are \code{7} and \code{3}, and
their difference is \code{4}.
\item The third row's difference is \code{6}.
}

In this example, the spreadsheet's checksum would be \code{8 + 4 + 6 = 18}.

\emph{What is the checksum} for the spreadsheet in your puzzle input?

\strong{Part Two}

"Great work; looks like we're on the right track after all. Here's a
\emph{star} for your effort." However, the program seems a little worried.
Can programs \emph{be} worried?

"Based on what we're seeing, it looks like all the User wanted is some
information about the \emph{evenly divisible values} in the spreadsheet.
Unfortunately, none of us are equipped for that kind of calculation -
most of us specialize in bitwise operations."

It sounds like the goal is to find the only two numbers in each row
where one evenly divides the other - that is, where the result of the
division operation is a whole number. They would like you to find those
numbers on each line, divide them, and add up each line's result.

For example, given the following spreadsheet:\preformatted{5 9 2 8
9 4 7 3
3 8 6 5
}
\itemize{
\item In the first row, the only two numbers that evenly divide are \code{8}
and \code{2}; the result of this division is \code{4}.
\item In the second row, the two numbers are \code{9} and \code{3}; the result is
\code{3}.
\item In the third row, the result is \code{2}.
}

In this example, the sum of the results would be \code{4 + 3 + 2 = 9}.

What is the \emph{sum of each row's result} in your puzzle input?
}
\examples{
s1 <- "5 1 9 5\\n7 5 3\\n2 4 6 8"
spreadsheet_checksum(s1, max_min_pair, function(x) max(x) - min(x))

s2 <- "5 9 2 8\\n9 4 7 3\\n3 8 6 5"
spreadsheet_checksum(s2, evenly_divisible_pair, function(x) max(x) / min(x))
}
