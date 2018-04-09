######################################################
#
#  Purpose: Perform confidence intervals and hypothesis
#           testing for randomness of a riffle shuffled
#           deck.
#
#  Written by: Kyle A. Caudle, South Dakota School of Mines
#              and technology.
#
#  Date: 8/22/17
#
#  Revisions: Changed into function 3/21/18
#
########################################################

sigtest <- function(nreps){
# set.seed(900) # seed for reproducibility

# nreps = number of sample values for each shuffle

# Initialzes the matrices to hold the results

# Matrix to hold rising shuffle values
resultsRS <- matrix(rep(0,10*nreps),nrow=nreps,ncol=10,byrow=T)

# Matrix to hold Kendall's Tau values
resultsKT <- matrix(rep(0,10*nreps),nrow=nreps,ncol=10,byrow=T)

# Matrix to hold lower and upper bounds for confidence intervals
CI <- matrix(rep(0,40),nrow=10,ncol=4)

# Shuffle the deck 1-10 times and store results.
for (j in 1:nreps){
  deck <- 1:52
  for (i in 1:10){
    pi <- shuffle(1:52)
    deck <- deck[pi]
    if (i==1){
      # Results for shuffling once
      resultsRS[j,1]=rseq(deck)
      resultsKT[j,1]=ktau(deck)
      }
    if (i==2){
      # Results for shuffling twice
      resultsRS[j,2]=rseq(deck)
      resultsKT[j,2]=ktau(deck)
      }
    if (i==3){
      # Results for shuffling three times
      resultsRS[j,3]=rseq(deck)
      resultsKT[j,3]=ktau(deck)
      }
    if (i==4){
      # Results for shuffling four times
      resultsRS[j,4]=rseq(deck)
      resultsKT[j,4]=ktau(deck)
      }
    if (i==5){
      # Results for shuffling five times
      resultsRS[j,5]=rseq(deck)
      resultsKT[j,5]=ktau(deck)
      }
    if (i==6){
      # Results for shuffling six times
      resultsRS[j,6]=rseq(deck)
      resultsKT[j,6]=ktau(deck)
      }
    if (i==7){
      # Results for shuffling seven times
      resultsRS[j,7]=rseq(deck)
      resultsKT[j,7]=ktau(deck)
      }
    if (i==8){
      # Results for shuffling eight times
      resultsRS[j,8]=rseq(deck)
      resultsKT[j,8]=ktau(deck)
      }
    if (i==9){
      # Results for shuffling nine times
      resultsRS[j,9]=rseq(deck)
      resultsKT[j,9]=ktau(deck)
      }
    if (i==10){
      # Results for shuffling ten times
      resultsRS[j,10]=rseq(deck)
      resultsKT[j,10]=ktau(deck)
      }
  }
}

# Package needed to plot confidence intervals
t <- qt(0.975,nreps-1) # Multiplier from t-distribution (95%)

# Determine the confidence intervals
for (i in 1:10){
  m1 <- mean(resultsRS[,i]) # Average rising sequences i shuffles
  s1 <-sd(resultsRS[,i]) # Sd of rising sequences i shuffles
  CI[i,1] <-  m1
  CI[i,2] <- t*s1/sqrt(nreps)
  m2 <- mean(resultsKT[,i]) # Average Kendall's tau i shuffles
  s2 <- sd(resultsKT[,i]) # Sd of Kendall's tau i shuffles
  CI[i,3] <- m2
  CI[i,4] <- t*s2/sqrt(nreps)
}

# Plot the the confidence intervals using "plotrix"
# Install the plotrix package to print CIs

if (requireNamespace("plotrix", quietly = TRUE)) {
  plotrix::plotCI(CI[,1],y=NULL,CI[,2],xlab="Shuffle Number",
         ylab="Number of Rising Sequences",
         xaxt="n",pch=20,sfrac=0.01)
  axis(1, at=1:10, labels=1:10)
} else {
  message("Install plotrix package to see the plot")
}

if (requireNamespace("plotrix", quietly = TRUE)) {
  plotrix::plotCI(CI[,3],y=NULL,CI[,4],xlab="Shuffle Number",
       ylab="Kendall's Tau Distance",
       xaxt="n",pch=20,sfrac=0.01)
axis(1, at=1:10, labels=1:10)
} else {
  message("Install plotrix package to see the plot")
}

# Determine p-values from t-tests

# Tests compare shuffles 3-4,4-5,5-6,6-7,7-8,8-9 and 9-10

# Rising Sequence tests
RStest <- NULL
for (i in 1:7){
  RStest[i] <- t.test(resultsRS[,i+2],resultsRS[,(i+3)])$p.value
}

# Kendall's tau tests
KTtest <- NULL
for (i in 1:7){
  KTtest[i] <- t.test(resultsKT[,i+2],resultsKT[,(i+3)])$p.value
}
  Intervals <- c("3-4","4-5","5-6","6-7","7-8","8-9","9-10")
  KTtest <- round(KTtest,4)
  RStest <- round(RStest,4)
  pvals <-   cbind(Intervals,KTtest,RStest)
  class(pvals) <- pvals
  return(class(pvals))


}
