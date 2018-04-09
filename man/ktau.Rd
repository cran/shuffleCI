\name{ktau}
\alias{ktau}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Kendall's tau
}
\description{
Calculates Kendall's tau distance
}
\usage{
ktau(list)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{list}{
A list of numbers
}
}
\details{
Returns the Kendall's tau distance between the input list and the sorted list 1,2,..n.
}
\value{
The Kendall's tau distance.
}
\references{
Kendall, M. G. (1938). A new measure of rank correlation. Biometrika, 30(1/2), 81-93.
}
\author{
Kyle Caudle
}

\examples{
ktau(c(1,4,2,5,6,3,7))
}

