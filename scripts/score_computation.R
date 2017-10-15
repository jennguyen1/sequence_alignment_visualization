# Score Computation
# Date: July 2016
# Author: Jenny Nguyen
# Email: jnnguyen2@wisc.edu

#' Compute the score of an alignment for linear gap penalty
#'
#' Parameters:
#' - str1, str2: (vector) the two strings to compare
#' - str1_i, str2_i: (int) index referring to the character in the string to compare
#' - match, mismatch: (int) scores
#'
#' Returns: (int) score of the alignment
#'
compute_s <- function(str1, str2, str1_i, str2_i, match, mismatch){

  # assign s to the correct score depending on the character comparison
  s <- ifelse(str1[str1_i] == str2[str2_i], match, mismatch)

  # return score
  return(s)

}

#' Compute the score of an alignment for affine gap penalty
#'
#' Parameters:
#' - matrices: (matrix) dictionary of matrices that contain the M, Ix, Iy matrices
#' - row, col: (int) row and column with which we want to compute the score of
#' - s, space: (int) scores
#'
#' Returns: (int) score of the alignment
#'
compute_cell <- function(matrices, row, col, s, space, use_local){

  values <- list(
    diag = matrices[row - 1, col - 1] + s,
    up = matrices[row - 1, col] + space,
    left = matrices[row, col - 1] + space
  )
  if(use_local) values$local <- 0

  return( values )

}

#' Computes the 3 possible scores for the M matrix (best score given that x is aligned to y)
#'
#' Parameters:
#' - matrices: (matrix) dictionary of matrices that contain the M, Ix, Iy matrices
#' - row, col: (int) row and column with which we want to compute the score of
#' - s, gap, space: (int) scores
#'
#' Returns: dictionary w/ 3 possible score values
#'
compute_m <- function(matrices, row, col, s, space, gap, use_local){

  # compute values, named by the matrix the score is derived
  m <- matrices[["m"]][row - 1, col - 1] + s
  ix <- matrices[["ix"]][row - 1, col - 1] + s
  iy <- matrices[["iy"]][row - 1, col - 1] + s

  # return possible scores
  possible_scores <- list(m = m, ix = ix, iy = iy)
  if(use_local) possible_scores$local <- 0
  return( possible_scores )

}

#' Computes the 3 possible scores for the Ix matrix (best score given that x is aligned to a gap)
#'
#' Parameters:
#' - matrices: (matrix) dictionary of matrices that contain the M, Ix, Iy matrices
#' - row, col: (int) row and column with which we want to compute the score of
#' - s, gap, space: (int) scores
#'
#' Returns: dictionary w/ 3 possible score values
#'
compute_ix <- function(matrices, row, col, s, space, gap, use_local){

  # compute values, named by the matrix the score is derived
  m <- matrices[["m"]][row - 1, col] + gap + space
  ix <- matrices[["ix"]][row - 1, col] + space

  return( list(m = m, ix = ix) )

}

#' Computes the 3 possible scores for the Iy matrix (best score given that y is aligned to a gap)
#'
#' Parameters:
#' - matrices: (matrix) dictionary of matrices that contain the M, Ix, Iy matrices
#' - row, col: (int) row and column with which we want to compute the score of
#' - s, gap, space: (int) scores
#'
#' Returns: dictionary w/ 3 possible score values
#'
compute_iy <- function(matrices, row, col, s, space, gap, use_local){

  # compute values, named by matrix the score is derived
  m <- matrices[["m"]][row, col - 1] + gap + space
  iy <- matrices[["iy"]][row, col - 1] + space

  # return the possible scores
  return( list(m = m, iy = iy) )

}

