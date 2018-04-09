ktau <- function(list){

  ######################################################
  #
  #  Purpose: Find the Kendall's Tau distance from the
  #           sorted deck and the unsorted deck (1-52).
  #
  #  Written by: Kyle A. Caudle, South Dakota School of Mines
  #              and technology.
  #
  #  Date: 7/14/17
  #
  #  Revisions: None
  #
  #  Input: A list of numbers (i.e. the deck).
  #
  #  Output: Kendall's Tau distance.
  #
  ########################################################

  n <- length(list) # length of the list

  # Normalizing constant equal to the number of pairs.
  D <- n*(n-1)/2

  tau <- 0 # Initialize tau to zero

  # Determine the number of pairs that are positive
  # (i.e. T(j)>T(i)

  tau <- D - cor.test(seq_along(list), list, method="kendall")$statistic


  tau <- tau/D # Normalize the number of pairs

  return(tau)
}
