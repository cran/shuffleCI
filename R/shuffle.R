shuffle <- function(deck) {

  ######################################################
  #
  #  Purpose: Obtains a riffle shuffle with the probability
  #  distribution as in Shannon model.  This is done by sorting
  #  the positions and then putting in the actual card values.
  #
  #  Written by: Kyle A. Caudle, South Dakota School of Mines
  #              and technology.
  #
  #  Date: 7/14/17
  #
  #  Revisions: None
  #
  #  Input: deck, a list of numbers. nshufs, number of times to shuffle
  #
  #  Output: A shuffled ordering of the list resembling
  #          a riffle shuffle of playing cards.
  #
  ########################################################


    n <- length(deck)
    k <- rbinom(1,n,0.5) # Arbitary cut point for the shuffle
    S <- 1:n
    m <- n                # Bottom card
    LHpos <- NULL             # Vector to hold Left Hand positions

    # Sort the positions first and then put the cards in the
    # sorted positions

    for (i in 1:k){
      # Randomly pick a card to be "on the pile"
      s <- ceiling(m*runif(1))
      LHpos <- c(LHpos,S[s]) # Store the card
      S <- S[-s]  # Remove card from consideration next time
      m <- m-1
    }

    # End of sorting positions

    # Now put 1:k in the positions where the cards were
    # selected, and zeros in the missing positions.
    LHpos <- sort(LHpos) # Sort the deck of selected positions

    # Find the right hand positions
    RHpos <- sort(S)

    pi <- vector(length=n)

    # put integers 1,2,...,length(LHpos) in positions as specified
    # by the vector LHpos
    pi[c(LHpos)] <- 1:length(LHpos)

    # put integers length(LHpos)+1,length(LHpos)+2,...n in the other
    # positions
    pi[c(RHpos)] <- (length(LHpos)+1):n

    for (i in 1:k){
      pi[LHpos[i]] <- deck[i]
    }
    for (i in 1:52-k){
      pi[RHpos[i]] <- deck[k+i]
    }


    deck <- pi

  return(deck)
}
