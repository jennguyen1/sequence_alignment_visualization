# Alignment Algorithm - Traceback
# Date: July 2016
# Author: Jenny Nguyen
# Email: jnnguyen2@wisc.edu


#' Finds the best alignment following generation of DP matrices
#'
#' Parameters: matrices (dictionary of 3 DP matrices)
#'
#' Returns: name of the matrix (str), row and column (int) that corresponds to the best score
#'
# best_align <- function(matrices){
#   # find the last row - in which we will look for the best alignment
#   row = len(matrices['m']) - 1
#
#   # obtain the last rows of each matrix and store values in a dict
#   last_rows = { k: v[row] for k,v in matrices.items() }
#
#   # obtain the maximum value of each row and store the values in a dict
#   max_per_matrix = { k: max(v) for k,v in last_rows.items() }
#
#   # compute the best score from the max of each row of each matrix
#   best_score = max( max_per_matrix.values() )
#
#   # find the original matrix containing the best score - finds the key that best score corresponds to in max_per_matrix
#   best_matrix = [ k for k,v in max_per_matrix.items() if v == best_score ].pop()
#
#   # obtains the column of the best score by finding best score in the last rows
#   col = last_rows[best_matrix].index(best_score)
#
#   # returns the name of the best matrix, the row and column of the best alignment score
#   return best_matrix, row, col
# }
#


#' Traceback 1 step based on score of current position
#'
#' Parameters:
#' - matrices: dictionary of 3 DP matrices
#' - current_score: (int) current score, to match to traceback
#' - current_matrix, current_row, current_col: (str, 2 int) current position
#' - s, gap, space: (int) scores
#' - use_local: whether to use local or global alignment
#'
#' Returns: next matrix (str), row and column (int) corresponding to best score
#'
back_step <- function(matrices, current_matrix, current_score, current_row, current_col, s, space, gap, use_local){

  # initialize next coordinates
  next_row <- NULL
  next_col <- NULL

  if(gap == 0){

    # find the preceding scores that led to current score
    possible_scores <- compute_cell(matrices, current_row, current_col, s, space, use_local)
    next_matrix <- names(which.max(possible_scores))

    # map preceding value to matrix cell coordinates
    if(next_matrix == "up"){
      next_row <- current_row - 1
      next_col <- current_col
    } else if(next_matrix == "diag"){
      next_row <- current_row - 1
      next_col <- current_col - 1
    } else if(next_matrix == "left"){
      next_row <- current_row
      next_col <- current_col - 1
    }

  } else{

    # depending on the current matrix, find the preceding value that led to current value
    if(current_matrix == "ix"){
      compute_scores <- compute_ix

      next_row <- current_row - 1
      next_col <- current_col
    } else if(current_matrix == "m"){
      compute_scores <- compute_m

      next_row <- current_row - 1
      next_col <- current_col - 1
    } else if(current_matrix == "iy"){
      compute_scores <- compute_iy

      next_row <- current_row
      next_col <- current_col - 1
    }

    # find preceding matrix that led to current score
    possible_scores <- compute_scores(matrices, current_row, current_col, s, space, gap, use_local)
    next_matrix <- names(which.max(possible_scores))
  }

  # return next matrix, row, and column in traceback
  return( list(next_matrix = next_matrix, next_row = next_row, next_col = next_col) )
}


#' Traceback through all 3 matrices
#'
#' Parameters:
#' - matrices: dictionary of 3 DP matrices
#' - str1, str2: (str) strings to compare
#' - current_matrix, current_row, current_column: (str, 2 int) current position
#' - match, mismatch, gap, space: (int) scores
#' - use_local: whether to use local or global alignment
#'
#' Returns: (str) strings with alignment and gaps
#'
traceback <- function(matrices, str1, str2, current_matrix, current_row, current_col, match, mismatch, space, gap, use_local){

  # initialize list of coordinates to track
  coordinates <- list( c(current_row, current_col) )

  if(use_local){
    # initialize the strings
    str_x <- str1[current_row]
    str_y <- str2[current_col]

  } else{
    # intialize the strings, saves the portions of the string that come after the best alignment
    # reverses the string so we can easily add to it (will un-reverse later)
    str_x <- rev( str1[(current_row+1):(length(str1)+1)] )
    str_y <- rev( str2[(current_col+1):(length(str2)+1)] )

    # add the character that corresponds to the best alignment
    str_x <- c(str_x, str1[current_row])
    str_y <- c(str_y, str2[current_col])

  }

  # to start the current matrix, row, column correspond to the best score in the last row
  # continues through the loop until we reach a stopping point of the DP matrix
  while(!(current_row == 1 & current_col == 1)){
    
    # obtains the score corresponding to current matrix, row, col
    current_score <- ifelse(gap == 0, matrices[current_row, current_col], matrices[[current_matrix]][current_row, current_col])

    # compute mismatch function
    s <- compute_s(str1, str2, current_row, current_col, match, mismatch)

    # find the next matrix, row, column in traceback
    current_status <- back_step(matrices, current_matrix, current_score, current_row, current_col, s, space, gap, use_local)
    current_matrix <- current_status$next_matrix
    current_row <- current_status$next_row
    current_col <- current_status$next_col

    # best alignment is x (in str1) aligned to a gap (in str2)
    if(current_matrix == "ix" | current_matrix == "up"){
      str_x <- c(str_x, str1[current_row])
      str_y <- c(str_y, "-")

    # best alignment is x (in str1) aligned to y (in str2)
    } else if(current_matrix == "m" | current_matrix == "diag"){
      str_x <- c(str_x, str1[current_row])
      str_y <- c(str_y, str2[current_col])

    # best alignment is a gap (in str1) aligned to y (in str2)
    } else if(current_matrix == "iy" | current_matrix == "left"){
      str_x <- c(str_x, "-")
      str_y <- c(str_y, str2[current_col])

    # local alignment - remove last addition and break out 
    } else{
      coordinates[[length(coordinates)]] <- NULL
      if( length(str_x) > 1 ){
        str_x <- str_x[1:(length(str_x)-1)]
        str_y <- str_y[1:(length(str_y)-1)]
      } else{
        str_x <- ""
        str_y <- ""
      }
      break
    }

    # add to list of coordinates
    coordinates[[length(coordinates) + 1]] <- c(current_row, current_col)

  }

  if(use_local){
    # reverse string order
    str_x <- rev(str_x)
    str_y <- rev(str_y)

  } else{
    # reverse string order - remove the "-" at beginning of string and NA at end
    str_x <- rev(str_x)[2:(length(str_x)-1)]
    str_y <- rev(str_y)[2:(length(str_y)-1)]

    # fix for if two string lengths are not equal
    if(length(str_x) < length(str_y)){
      str_x <- c(str_x, rep("-", length(str_y) - length(str_x)))
    } else if(length(str_y) < length(str_x)){
      str_y <- c(str_y, rep("-", length(str_x) - length(str_y)))
    }

  }

  # return aligned strings and coordinates to highlight
  bars <- ifelse(nchar(str_x) != 0, Map(function(x, y) ifelse(x != y, ".", "|"), str_x, str_y), "")
  strings <- list(x = paste(str_x, collapse = ""), y = paste(str_y, collapse = ""), bars = paste(bars, collapse = " "))
  return( list(strings = strings, coordinates = coordinates) )
}

