% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/day16.R
\name{day16}
\alias{day16}
\alias{dance}
\alias{dance_a_billion_times}
\title{Day 16: Permutation Promenade}
\usage{
dance(programs, moves)

dance_a_billion_times(programs, moves)
}
\arguments{
\item{programs}{a string of letters to reorder}

\item{moves}{a string of moves to do}
}
\description{
\href{http://adventofcode.com/2017/day/16}{Permutation Promenade}
}
\details{
\strong{Part One}

You come upon a very unusual sight; a group of programs here appear to
be \href{https://www.youtube.com/watch?v=lyZQPjUT5B4&t=53}{dancing}.

There are sixteen programs in total, named \code{a} through \code{p}. They start
by standing in a line: \code{a} stands
in position \code{0}, \code{b} stands in position \code{1}, and so on until \code{p}, which
stands in position \code{15}.

The programs' \emph{dance} consists of a sequence of \emph{dance moves}:
\itemize{
\item \emph{Spin}, written \code{sX}, makes \code{X} programs move from the end to the
front, but maintain their order otherwise. (For example, \code{s3} on
\code{abcde} produces \code{cdeab}).
\item \emph{Exchange}, written \code{xA/B}, makes the programs at positions \code{A} and
\code{B} swap places.
\item \emph{Partner}, written \code{pA/B}, makes the programs named \code{A} and \code{B} swap
places.
}

For example, with only five programs standing in a line (\code{abcde}), they
could do the following dance:
\itemize{
\item \code{s1}, a spin of size \code{1}: \code{eabcd}.
\item \code{x3/4}, swapping the last two programs: \code{eabdc}.
\item \code{pe/b}, swapping programs \code{e} and \code{b}: \code{baedc}.
}

After finishing their dance, the programs end up in order \code{baedc}.

You watch the dance for a while and record their dance moves (your
puzzle input). \emph{In what order are the programs standing} after their
dance?

\strong{Part Two}

Now that you're starting to get a feel for the dance moves, you turn
your attention to \emph{the dance as a whole}.

Keeping the positions they ended up in from their previous dance, the
programs perform it again and again: including the first dance, a total
of \emph{one billion} (\code{1000000000}) times.

In the example above, their second dance would \emph{begin} with the order
\code{baedc}, and use the same dance moves:
\itemize{
\item \code{s1}, a spin of size \code{1}: \code{cbaed}.
\item \code{x3/4}, swapping the last two programs: \code{cbade}.
\item \code{pe/b}, swapping programs \code{e} and \code{b}: \code{ceadb}.
}

\emph{In what order are the programs standing} after their billion dances?
}
\examples{
programs <- "abcde"
moves <- "s1,x3/4,pe/b"
dance(programs, moves)
}
