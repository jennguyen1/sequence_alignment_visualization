# Server Code for Sequence Alignment Visualization
# Date: June 2016
# Author: Jenny Nguyen
# Email: jnnguyen2@wisc.edu

library(plyr)
library(stringr)
library(ggplot2)
library(cowplot)
library(jn.general)
library(data.table)
library(shiny)
source('~/Desktop/sequence_alignment_visualization/generate_matrix.R')


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
    if(!is_y){
      low = 0.12
      high = 0.95
    } else{
      low = 0.05
      high = 0.85
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
    return(click_coordinate)
  }


  # make a grid of all pairwise combos of letters
  make_data <- function(x, y) expand.grid(x = split_word(x), y = split_word(y))


  #######################
  # Plot Output Editing #
  #######################
  # function to draw the plot
  drawPlot <- function(plot_data, params){

    # plot data
    g <- ggplot(data = plot_data, aes(x_coordinates, y_coordinates, label = text, fill = value)) +
      geom_tile(color = "black", size = 1.1) +
      geom_text() +

      # format the axes
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

  # plot to highlight results based on click coordinates TODO: edit after algorithm
  highlight_plot <- function(data, params, click){

    if(is.null(click)){
      return(data)
    }

    # find the coordinates of x and y
    click_x <- click_to_coordinates(click$x, params$len_x, is_y = FALSE)
    click_y <- click_to_coordinates(click$y, params$len_y, is_y = TRUE, params$coord_y)

    # find the data to color TODO
    split_data <- to_be(data, dplyr::filter, x_coordinates == click_x, y_coordinates == click_y)

    # color data and return results
    split_data$to_be <- mutate(split_data$to_be, value = 0)
    split_data$not_to_be <- mutate(split_data$not_to_be, value = 1)
    combined_results <- rbindlist(list(split_data$to_be, split_data$not_to_be))

    return(combined_results)
  }

  # effects of click on plot (highlights) disappears seconds after event
  # ensure that the effect of click on graph is permanent by using a global click variable
  click_value <- NULL
  change_click <- function(previous, in_click){
    if( is.null(in_click) & is.null(previous) ){
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

  # structure to hold the variables TODO: edit add the algorithm here
  params <- reactive({

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

    # make matrix: x = col indices; y = row indices
    values <- make_matrices(split_y, split_x, match = input$match, mismatch = input$mismatch, space = input$space, gap = input$gap, use_local = input$alignment == "local")
    values <- as.vector(matrix(t(values), ncol = 1))

    # values for each box
    data <- mutate(data, text = values, value = 0)

    # output structures
    list(data = data,
         coord_x = coord_x, split_x = split_x, len_x = len_x,
         coord_y = coord_y, split_y = split_y, len_y = len_y
    )
  })


  ####################
  # Generate Outputs #
  ####################

  # generate plot output
  output$myPlot <- renderPlot({
    change_click(click_value, input$plotClick)
    data <- highlight_plot(params()$data, params(), click_value)
    drawPlot(data, params())
  })

})
