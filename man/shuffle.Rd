\name{shuffle}
\alias{shuffle}

\title{
Riffle Shuffle
}
\description{
This function simulates a standard riffle shuffle of a deck of 52 playing cards.
}
\usage{
shuffle(deck)
}

\arguments{
  \item{deck}{
A list of numbers.  1:52 would simulate a deck of cards in sequential order.
}
}
\details{
The algorithm is based on the Gilbert-Shannon-Reeds method.
}
\value{
Returns a shuffled list (i.e. deck)
}
\references{
Gilbert, E. (1955).  Theory of shuffling.  Technical memorandum, Bell Laboratories.
}
\author{
Kyle Caudle
}

\examples{
shuffle(1:52)
}

