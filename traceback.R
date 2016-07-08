# Alignment Algorithm - Traceback
# Date: July 2016
# Author: Jenny Nguyen
# Email: jnnguyen2@wisc.edu


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

  # choose the right algorithm
  if(gap == 0){
    out <- back_step_linear(matrices, current_matrix, current_score, current_row, current_col, s, space, use_local)
  } else{
    out <- back_step_affine(matrices, current_matrix, current_score, current_row, current_col, s, space, gap, use_local)
  }

  return(out)
}

#'
back_step_linear <- function(matrices, current_matrix, current_score, current_row, current_col, s, space, use_local){
  
  # initialize next coordinates
  next_row <- NULL
  next_col <- NULL
  
  # find the preceding scores that led to current score
  possible_scores <- compute_cell(matrices, current_row, current_col, s, space, use_local)
  next_matrix <- names(which.max(possible_scores))
  
  # map preceding value to matrix cell coordinates
  switch(
    next_matrix, 
    'up' = {
      next_row <- current_row - 1
      next_col <- current_col    
    },
    'diag' = {
      next_row <- current_row - 1
      next_col <- current_col - 1
    }, 
    "left" = {
      next_row <- current_row
      next_col <- current_col - 1
    }
  )
  
  # return next matrix, row, and column in traceback
  return( list(next_matrix = next_matrix, next_row = next_row, next_col = next_col) )
}

#'
back_step_affine <- function(matrices, current_matrix, current_score, current_row, current_col, s, space, gap, use_local){
  
  # initialize next coordinates
  next_row <- NULL
  next_col <- NULL
    
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
  
  # return next matrix, row, and column in traceback
  return( list(next_matrix = next_matrix, next_row = next_row, next_col = next_col) )
}


#' Traceback through entire matrices
#'
#' Parameters:
#' - matrices: dictionary of 3 DP matrices
#' - str_c, str_r: (str) strings to compare
#' - current_matrix, current_row, current_column: (str, 2 int) current position
#' - match, mismatch, gap, space: (int) scores
#' - use_local: whether to use local or global alignment
#'
#' Returns: (str) strings with alignment and gaps
#'
traceback <- function(matrices, str_c, str_r, current_matrix, current_row, current_col, match, mismatch, space, gap, use_local){

  # initialize list of coordinates to track
  coordinates <- list( c(current_row, current_col) )
  
  if(use_local){
    
    # initialize the strings
    str_x <- c()
    str_y <- c()
    
  } else{
    
    # intialize the strings, saves the portions of the string that come after the best alignment
    # reverses the string so we can easily add to it (will un-reverse later)
    str_x <- rev( str_c[(current_col+1):(length(str_c)+1)] )
    str_y <- rev( str_r[(current_row+1):(length(str_r)+1)] )
    
  }

  # implement the right algorithm
  if(gap == 0) {
    out <- traceback_local(coordinates, str_x, str_y, matrices, str_c, str_r, current_matrix, current_row, current_col, match, mismatch, space, gap, use_local)
  } else{
    out <- traceback_affine(coordinates, str_x, str_y, matrices, str_c, str_r, current_matrix, current_row, current_col, match, mismatch, space, gap, use_local)
  }
  
  coordinates <- out$coordinates
  str_x <- out$str_x
  str_y <- out$str_y

  # process the string to original format
  if(use_local){
    
    # fix for landing in upper corner which is 0
    if( all(coordinates[[length(coordinates)]] == 1) ) {
      coordinates[[length(coordinates)]] <- NULL
    }
    
    # reverse string order
    str_x <- rev(str_x)
    str_y <- rev(str_y)
    
    # remove extra dash at beginning if there is one
    if( str_x[1] == "-" & str_y[1] == "-" ){
      str_x <- str_x[2:length(str_x)]
      str_y <- str_y[2:length(str_y)]
    }
    
  } else{
    
    # reverse string order - remove the NA at end
    str_x <- rev(str_x)[1:(length(str_x)-1)]
    str_y <- rev(str_y)[1:(length(str_y)-1)]
    
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

#'
traceback_local <- function(coordinates, str_x, str_y, matrices, str_c, str_r, current_matrix, current_row, current_col, match, mismatch, space, gap, use_local){
  
  # to start the current matrix, row, column correspond to the best score in the last row
  # continues through the loop until we reach a stopping point of the DP matrix
  while(!(current_row == 1 & current_col == 1)){
    
    # obtains the score corresponding to current matrix, row, col
    current_score <- matrices[current_row, current_col]
    
    # compute mismatch function
    s <- compute_s(str_c, str_r, current_col, current_row, match, mismatch)
    
    # find the next matrix, row, column in traceback
    current_status <- back_step(matrices, current_matrix, current_score, current_row, current_col, s, space, gap, use_local)
    current_matrix <- current_status$next_matrix 
    
    switch(
      current_matrix, 
      
      # best alignment is x (in str_c) aligned to a gap (in str_r)
      'up' = {
        str_x <- c(str_x, "-")
        str_y <- c(str_y, str_r[current_row])
      }, 
      
      # best alignment is x (in str_c) aligned to y (in str_r)
      'diag' = {
        str_x <- c(str_x, str_c[current_col])
        str_y <- c(str_y, str_r[current_row])
      }, 
      
      # best alignment is a gap (in str_c) aligned to y (in str_r)
      'left' = {
        str_x <- c(str_x, str_c[current_col])
        str_y <- c(str_y, "-")
      }, 
      
      # local alignment - remove last addition and break out 
      'local' = {
        coordinates[[length(coordinates)]] <- NULL
        break
      }
    )
    
    # assign next row and column and add list of coordinates
    current_row <- current_status$next_row
    current_col <- current_status$next_col
    coordinates[[length(coordinates) + 1]] <- c(current_row, current_col)

  }
  
  # return results
  return(list(coordinates = coordinates, str_x = str_x, str_y = str_y))
}

#'
traceback_affine <- function(coordinates, str_x, str_y, matrices, str_c, str_r, current_matrix, current_row, current_col, match, mismatch, space, gap, use_local){
  
  # to start the current matrix, row, column correspond to the best score in the last row
  # continues through the loop until we reach a stopping point of the DP matrix
  while(!(current_row == 1 & current_col == 1)){
    
    switch(
      current_matrix,
      
      # best alignment is x (in str_c) aligned to a gap (in str_r)
      'ix' = {
        str_x <- c(str_x, "-")
        str_y <- c(str_y, str_r[current_row])
      },
      
      # best alignment is x (in str_c) aligned to y (in str_r)
      'm' = {
        str_x <- c(str_x, str_c[current_col])
        str_y <- c(str_y, str_r[current_row])
      }, 
      
      # best alignment is x (in str_c) aligned to y (in str_r)
      'iy' = {
        str_x <- c(str_x, str_c[current_col])
        str_y <- c(str_y, "-")
      }, 
      
      # local alignment
      'local' = {
        coordinates[[length(coordinates)]] <- NULL
        break
      }
    )
    
    # obtains the score corresponding to current matrix, row, col
    current_score <- matrices[[current_matrix]][current_row, current_col]

    # compute mismatch function
    s <- compute_s(str_c, str_r, current_col, current_row, match, mismatch)
    
    # find the next matrix, row, column in traceback
    current_status <- back_step(matrices, current_matrix, current_score, current_row, current_col, s, space, gap, use_local)
    current_matrix <- current_status$next_matrix 

    # assign next row and column and add list of coordinates
    current_row <- current_status$next_row
    current_col <- current_status$next_col
    coordinates[[length(coordinates) + 1]] <- c(current_row, current_col)
    
  }
  
  # return results
  return(list(coordinates = coordinates, str_x = str_x, str_y = str_y))
}

