\name{rseq}
\alias{rseq}

\title{
Rising Sequences
}
\description{
Determines the number of rising sequences in a list of numbers.
}
\usage{
rseq(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
List of numbers
}
}
\details{
A rising sequence is maximal consecutively increasing subsequence.

Ex: 1,4,2,5,6,3,7
There are 3 rising sequences in this list.

(1,4,5,6,7),(2,3)
}
\value{
nrise - the number of rising sequences.
}
\references{
Mann, B. (1995). How many times should you shuffle a deck of cards. Topics in Contemporary Probability and Its Applications, 15, 1-33.

Williams, C. O. (1912). A card reading. The Magician Monthly, 8, 67.
}
\author{
Kyle Caudle
}

\examples{
rseq(c(1,4,2,5,6,3,7))
}
