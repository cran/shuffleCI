rseq <- function(x){

  ######################################################
  #
  #  Purpose: Find the number of rising sequences in a
  #           permutation.
  #
  #  Written by: Kyle A. Caudle, South Dakota School of Mines
  #              and technology.
  #
  #  Date: 8/22/17
  #
  #  Revisions: None
  #
  #  Input: A list of numbers (i.e. the deck).
  #
  #  Output: Number of rising sequences.
  #
  ########################################################

  n <- length(x)
  thresh <- 0 # length of min rising sequence must exceed this
  nrise <- 0  # initialize rising sequence

  seqpos <- which(x==1) # Initialize starting position of first sequence

  # Check to see if you're at the end of the deck
  if(seqpos==52){
    seqpos <- which(x==2)
    nrise <- 1
    }

  ctr <- 1 # Initialize counter for counting sequence length

  while (seqpos < n) {
    # Loop over remaining elements in the set to find the sequence
  j <- seqpos # set loop counter to seqpos
  sval <- x[j]
    for (k in (j+1):n){
      if (sval+1==x[k]){
        sval <- sval+1 # Increment sequence vaue
        ctr <- ctr+1 # increment counter
      }
      seqpos <- seqpos+1
  }
    if (ctr>thresh) {nrise <- nrise+1} # check to see if long enough
    ctr <- 1
    if(sval !=n) {seqpos <- which(x==sval+1)}
  }
return(nrise) # return number of rising sequences

}
