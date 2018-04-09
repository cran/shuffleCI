\name{sigtest}
\alias{sigtest}

\title{
Significance Testing
}
\description{
This function uses Kendall's tau and Rising sequences to determine how many times to shuffle a an ordinary deck of 52 playing cards.
}
\usage{
sigtest(nreps)
}

\arguments{
  \item{nreps}{
Number of experiment repetitions (should be at least 30/Central Limit Theorem)
}
}
\details{
According to the Bayer & Diaconis paper, after 7 shuffles there is no benefit to shuffling any more.  This simulation shows that using Kendall's tau to show this doesn't work because the variance of the Kendall's distance is too large.  However, if one looks at rising sequences, the variability is smaller therefore it is possible to show the Bayer & Diaconis result.
}
\value{
Plots confidence intervals for each method.  The print method returns the p-values from two sample t-test for the sequential interval comparisons (i.e. intervals: 3-4,4-5,5-6,6-7,7-8,8-9 and 9-10).
}
\references{
Bayer, D., & Diaconis, P. (1992). Trailing the dovetail shuffle to its lair. The Annals of Applied Probability, 294-313.
}
\author{
Kyle Caudle
}

\examples{
sigtest(15)
}
