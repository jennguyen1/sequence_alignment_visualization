# Server Code for Sequence Alignment Visualization
# Date: June 2016
# Author: Jenny Nguyen
# Email: jnnguyen2@wisc.edu

# TODO
# highroad v lowroad
# cleanup
# global alignment auto? no
# local align abc, aac; bottom corner trace matches - traceback

library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(cowplot)
library(jn.general)
library(data.table)
library(shiny)
source('score_computation.R')
source('generate_matrix.R')
source('traceback.R')


shinyServer(function(input, output) {

  ###################
  # Initialize Data #
  ###################

  # function to split a string into vector of characters
  split_word <- function(word) laply(1:nchar(word), function(i) str_sub(word, i, i))


  # functions to convert each character position to a coordinate
  word_to_coordinates <- function(split_word) 0:(length(split_word) - 1) + 0.5

  # function to convert a click into a coordinate
  click_to_coordinates <- function(click, word_length, is_y = FALSE, original_coordinates){

    # error for y data
    if(is_y & missing(original_coordinates)){
      stop("Need to supply original coordinates if mapping the y click")
    }

    # fix: boundary limits because ggdraw doesn't start and end at 0/1
    if(is_y){
      low = 0.05
      high = 0.90
    } else{
      low = 0.10
      high = 0.95
    }

    # set boundary limits
    boundaries <- seq(low, high, length.out = word_length + 1)

    # convert click value to coordinate
    for(i in 1:(length(boundaries) - 1)){
      if( between(click, boundaries[i], boundaries[i + 1]) ){
        click_coordinate <- i - 0.5
      }
    }

    if(is_y){
      click_coordinate <- mapvalues(click_coordinate, original_coordinates, rev(original_coordinates))
    }

    # return results
    click_coordinate <- laply(click_coordinate, function(a) a + 0.5)
    return( click_coordinate )
  }


  # make a grid of all pairwise combos of letters
  make_data <- function(x, y) expand.grid(x = split_word(x), y = split_word(y))


  #######################
  # Plot Output Editing #
  #######################
  # function to draw the plot
  drawPlot <- function(plot_data, params, click_value){

    # plot data
    if( is.numeric(plot_data$value) ){
      g <- ggplot(data = plot_data, aes(x_coordinates, y_coordinates, label = text, fill = value)) +
        geom_tile(color = "black", size = 1.1)
    } else{
      g <- ggplot(data = plot_data, aes(x_coordinates, y_coordinates, label = text)) +
        geom_tile(fill = "#F0FFF0", color = "black", size = 1.1)
    }
    
    # plot data
    g <- g +
      geom_text() + 
      scale_fill_gradient(low = "#F0FFF0", high = "#0AC92B")

    # format the axes
    g <- g + 
      scale_y_reverse(breaks = params$coord_y, labels = params$split_y) +
      scale_x_continuous(breaks = params$coord_x, labels = params$split_x) +
      theme_bw() +
      theme(
        legend.position = "none",
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_text(size = 20, face = "bold"),
        axis.text.y = element_text(size = 20, face = "bold"),
        axis.ticks = element_line(color = "white")
      ) +
      labs(x = "", y = "")

    # move x-axis to the top
    g <- ggdraw(switch_axis_position(g, axis = "x"))

    return(g)
  }

  # process click to obtain strings & highlight plots
  process_click <- function(data, params, click){

    return_original <- FALSE

    # fix for no click data
    if(is.null(click)){
      return( list(data = data, strings = NULL) )
    }
    
    # fix for clicking outside of the boundaries
    if( !between(click$x, 0.10, 0.95) | !between(click$y, 0.05, 0.90) ){
      return( list(data = data, strings = NULL) )
    }

    # find the coordinates of x and y
    click_x <- click_to_coordinates(click$x, params$len_x, is_y = FALSE)
    click_y <- click_to_coordinates(click$y, params$len_y, is_y = TRUE, params$coord_y)

    # find the current matrix for affine gap values
    if(params$gap != 0){
      current_matrix <- llply(1:3, function(i) params$matrices[[i]][click_x, click_y])
      names(current_matrix) <- names(params$matrices)
      current_matrix <- names(which.max(current_matrix))
    } else{
      current_matrix <- NULL
    }

    # run the traceback and highlight the values
    highlight_values <- traceback(params$matrices, params$split_x, params$split_y,
                                  current_matrix, click_x, click_y,
                                  params$match, params$mismatch, params$space, params$gap, params$use_local
                                  )

    # fix for clicking on a box that doesn't lead anywhere
    if( length(highlight_values$coordinates) == 0 ){
      return( list(data = data, strings = NULL) )
    }

    # generate command to color the pathway
    cmd <- lapply(highlight_values$coordinates, function(v) paste0("(x_coordinates == ", v[1] - 0.5, " & ", "y_coordinates == ", v[2] - 0.5, ")")) %>%
      unlist %>%
      paste(collapse = " | ")

    # find the data to color
    split_data <- to_be(data, dplyr::filter_, cmd)

    # color data and return results
    split_data$to_be <- mutate(split_data$to_be, value = 1)
    split_data$not_to_be <- mutate(split_data$not_to_be, value = 0)
    combined_results <- rbindlist(list(split_data$to_be, split_data$not_to_be))

    # return results
    return( list(data = combined_results, strings = highlight_values$strings) )

  }

  # effects of click on plot (highlights) disappears seconds after event
  # ensure that the effect of click on graph is permanent by using a global click variable
  click_value <- NULL
  change_click <- function(previous, in_click){

    if( (is.null(in_click) & is.null(previous)) ){
      click_value <<- NULL
    } else if( is.null(in_click) & !is.null(previous) ){
      click_value <<- previous
    } else{
      click_value <<- in_click
    }

  }


  ################
  # Data Storage #
  ################

  # structure to hold the variables
  params <- eventReactive(input$submit, {

    click_value <<- NULL

    # vector of characters
    split_x <- split_word( paste0("-", input$x) )
    split_y <- split_word( paste0("-", input$y) )

    # length of words
    len_x <- length(split_x)
    len_y <- length(split_y)

    # coordinate of characters
    coord_x <- word_to_coordinates(split_x)
    coord_y <- word_to_coordinates(split_y)

    # grid
    data <- expand.grid(x_coordinates = coord_x, y_coordinates = coord_y)
    data <- mutate(data, x_word = mapvalues(x_coordinates, coord_x, split_x), y_word = mapvalues(y_coordinates, coord_y, split_y))

    # make matrix: x = row indices; y = col indices
    DP_matrix <- make_matrices(split_x, split_y, match = input$match, mismatch = input$mismatch, space = input$space, gap = input$gap, use_local = input$alignment == "local")
    matrices <- DP_matrix$matrices
    formatted_matrices <- DP_matrix$formatted_matrices
    values <- as.vector(matrix((formatted_matrices), ncol = 1))

    # values for each box
    data <- mutate(data, text = values, value = values)

    # output structures
    list(data = data, matrices = matrices,
         coord_x = coord_x, split_x = split_x, len_x = len_x,
         coord_y = coord_y, split_y = split_y, len_y = len_y,
         match = input$match, mismatch = input$mismatch,
         space = input$space, gap = input$gap, use_local = input$alignment == "local"
    )
  })

  # structure to hold plot highlights and alignment results
  clicks <- reactive({

    # change the click value
    change_click(click_value, input$plotClick)

    # run traceback and get highlight plot data and aligned strings
    click_results <- process_click(params()$data, params(), click_value)

    # return results
    return(click_results)

  })

  ####################
  # Generate Outputs #
  ####################

  # generate plot output
  output$myPlot <- renderPlot({
    data <- clicks()$data
    drawPlot(data, params(), click_value)
  })

  # generate aligned strings text
  output$alignResults <- renderText(ifelse( nchar(clicks()$strings[[1]]) > 0, "Alignment Results:", "") )
  output$alignedText1 <- renderText(clicks()$strings[[1]])
  output$alignedText2 <- renderText(clicks()$strings[[2]])
  output$alignedBars <- renderText(clicks()$strings[[3]])

})
