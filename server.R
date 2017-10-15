# Server Code for Sequence Alignment Visualization
# Date: June 2016
# Author: Jenny Nguyen
# Email: jnnguyen2@wisc.edu

# load libraries
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(jn.general)
library(data.table)
library(shiny)

# load dependencies
source('scripts/score_computation.R')
source('scripts/generate_matrix.R')
source('scripts/traceback.R')

#-------------------------------------------------
# FUNCTIONS FOR INITIATING DATA ------------------

# function to split a string into vector of characters
split_word <- function(word) str_split(word, boundary("character"))[[1]]


# functions to convert each character position to a coordinate (making plot axis)
word_to_coordinates <- function(split_word) 0:(length(split_word) - 1) + 0.5


#-------------------------------------------------
# FUNCTIONS FOR PLOTTING DATA --------------------

# function to draw the plot
drawPlot <- function(plot_data, params){

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
    scale_x_continuous(breaks = params$coord_x, labels = params$split_x, position = "top") +
    theme_bw() +
    theme(
      legend.position = "none",
      panel.grid = element_blank(),
      panel.border = element_blank(),
      axis.text = element_text(size = 20, face = "bold"),
      axis.ticks = element_line(color = "white")
    ) +
    labs(x = "", y = "")

  return(g)
}

# process click to obtain strings & highlight plots
process_click <- function(data, params, click){

  return_nothing <- list(data = data, strings = NULL)

  # fix for no click data
  if(is.null(click)){
    return( return_nothing )
  }

  # fix for clicking outside of the boundaries - resets graph
  if( !between(click$x, 0, params$len_x) | !between(click$y, 0, params$len_y) ){
    return( return_nothing )
  }

  # find the coordinates of x and y
  click_x <- ceiling(click$x)
  click_y <- ceiling(click$y)

  # fix for clicking on the initial position (1, 1)
  if(click_x == 1 & click_y == 1){
    return( return_nothing )
  }


  # find the current matrix for affine gap values
  current_matrix <- NULL
  if(params$gap != 0){

    current_matrix <- lapply(1:3, function(i) params$matrices[[i]][click_y, click_x])
    names(current_matrix) <- names(params$matrices)
    current_matrix <- names(which.max(current_matrix))

  }

  # run the traceback and highlight the values
  highlight_values <- traceback(
    matrices = params$matrices, str_c = params$split_x, str_r = params$split_y,
    current_matrix = current_matrix, current_row = click_y, current_col = click_x,
    match = params$match, mismatch = params$mismatch, space = params$space, gap = params$gap, use_local = params$use_local
  )

  # fix for clicking on a box that doesn't lead anywhere
  if( length(highlight_values$coordinates) == 0 ){
    return( return_nothing )
  }

  # generate command to color the pathway
  cmd <- lapply(highlight_values$coordinates, function(v) paste0("(y_coordinates == ", v[1] - 0.5, " & ", "x_coordinates == ", v[2] - 0.5, ")")) %>%
    unlist %>%
    paste(collapse = " | ")

  # find the data to color
  split_data <- to_be(data, dplyr::filter_, cmd)

  # color data and return results
  split_data$to_be <- mutate(split_data$to_be, value = 1)
  split_data$not_to_be <- mutate(split_data$not_to_be, value = 0)
  combined_results <- bind_rows(split_data$to_be, split_data$not_to_be)

  # return results
  return( list(data = combined_results, strings = highlight_values$strings) )

}


#-------------------------------------------------

shinyServer(function(input, output) {

  # change input options available depending on inputs
  output$space_option <- renderUI({
    switch(input$gap_penalty,
           "linear" = numericInput("space", "Gap Score:", -2, max = 0),
           "affine" = numericInput("space", "Space Score:", -2, max = 0)
    )
  })

  output$gap_option <- renderUI({
    if(input$gap_penalty == "affine") numericInput("gap", "Gap Score:", -4, max = 0)
  })

  ################
  # Data Storage #
  ################

  # structure to hold the variables
  params <- eventReactive(input$submit, {

    # process gap value
    gap <- ifelse( input$gap_penalty == "linear", 0, input$gap )

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
    DP_matrix <- make_matrices(
      str_c = split_x, str_r = split_y,
      match = input$match, mismatch = input$mismatch,
      space = input$space, gap = gap,
      use_local = input$alignment == "local"
    )
    matrices <- DP_matrix$matrices
    formatted_matrices <- DP_matrix$formatted_matrices
    values <- as.vector(matrix(t(formatted_matrices), ncol = 1))

    # values for each box
    data <- mutate(data, text = values, value = values)

    # output structures
    list(data = data, matrices = matrices,
         coord_x = coord_x, split_x = split_x, len_x = len_x,
         coord_y = coord_y, split_y = split_y, len_y = len_y,
         match = input$match, mismatch = input$mismatch,
         space = input$space, gap = gap, use_local = input$alignment == "local"
    )
  })

  # hold the value of the click
  click_value <- reactiveValues(click = NULL)
  observeEvent(input$plotClick, {
    click_value$click <- input$plotClick
  })

  # structure to hold plot highlights and alignment results
  clicks <- reactive({

    # run traceback and get highlight plot data and aligned strings
    click_results <- process_click(params()$data, params(), click_value$click)

    # return results
    return(click_results)

  })

  ####################
  # Generate Outputs #
  ####################

  # generate plot output
  output$myPlot <- renderPlot({
    data <- clicks()$data
    drawPlot(data, params())
  })

  # generate aligned strings text
  output$alignResults <- renderText(ifelse( nchar(clicks()$strings[[1]]) > 0, "Alignment Results:", "") )
  output$alignedText1 <- renderText(clicks()$strings[[1]])
  output$alignedText2 <- renderText(clicks()$strings[[2]])
  output$alignedBars <- renderText(clicks()$strings[[3]])

})
