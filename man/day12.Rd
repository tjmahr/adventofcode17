% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/day12.R
\name{day12}
\alias{day12}
\alias{search_pipes_from_zero}
\alias{count_pipe_groups}
\title{Day 12: Digital Plumber}
\usage{
search_pipes_from_zero(pipes)

count_pipe_groups(pipes)
}
\arguments{
\item{pipes}{a character vector of pipe connections}
}
\description{
\href{http://adventofcode.com/2017/day/12}{Digital Plumber}
}
\details{
\strong{Part One}

Walking along the memory banks of the stream, you find a small village
that is experiencing a little confusion: some programs can't communicate
with each other.

Programs in this village communicate using a fixed system of \emph{pipes}.
Messages are passed between programs using these pipes, but most
programs aren't connected to each other directly. Instead, programs pass
messages between each other until the message reaches the intended
recipient.

For some reason, though, some of these messages aren't ever reaching
their intended recipient, and the programs suspect that some pipes
are missing. They would like you to investigate.

You walk through the village and record the ID of each program and the
IDs with which it can communicate directly (your puzzle input). Each
program has one or more programs with which it can communicate, and
these pipes are bidirectional; if \code{8} says it can communicate with \code{11},
then \code{11} will say it can communicate with \code{8}.

You need to figure out how many programs are in the group that contains
program ID \code{0}.

For example, suppose you go door-to-door like a travelling salesman and
record the following list:\preformatted{0 <-> 2
1 <-> 1
2 <-> 0, 3, 4
3 <-> 2, 4
4 <-> 2, 3, 6
5 <-> 6
6 <-> 4, 5
}

In this example, the following programs are in the group that contains
program ID \code{0}:
\itemize{
\item Program \code{0} by definition.
\item Program \code{2}, directly connected to program \code{0}.
\item Program \code{3} via program \code{2}.
\item Program \code{4} via program \code{2}.
\item Program \code{5} via programs \code{6}, then \code{4}, then \code{2}.
\item Program \code{6} via programs \code{4}, then \code{2}.
}

Therefore, a total of \code{6} programs are in this group; all but program
\code{1}, which has a pipe that connects it to itself.

\emph{How many programs} are in the group that contains program ID \code{0}?

\strong{Part Two}

There are more programs than just the ones in the group containing
program ID \code{0}. The rest of them have no way of reaching that group, and
still might have no way of reaching each other.

A \emph{group} is a collection of programs that can all communicate via pipes
either directly or indirectly. The programs you identified just a moment
ago are all part of the same group. Now, they would like you to
determine the total number of groups.

In the example above, there were \code{2} groups: one consisting of programs
\code{0,2,3,4,5,6}, and the other consisting solely of program \code{1}.

\emph{How many groups are there} in total?
}
\examples{
pipes <- "0 <-> 2
1 <-> 1
2 <-> 0, 3, 4
3 <-> 2, 4
4 <-> 2, 3, 6
5 <-> 6
6 <-> 4, 5"
pipes \%>\% read_text_lines() \%>\% search_pipes_from_zero()
pipes \%>\% read_text_lines() \%>\% count_pipe_groups()
}
