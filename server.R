
library(plyr)
library(stringr)
library(ggplot2)
library(cowplot)
library(jn.general)
library(data.table)
library(shiny)

shinyServer(function(input, output) {
  
  # function to split a string into vector of characters
  split_word <- function(word) laply(1:nchar(word), function(i) str_sub(word, i, i))
  
  # functions to convert each character position to a coordinate
  word_to_coordinates <- function(split_word) 0:(length(split_word) - 1) + 0.5
  
  # function to convert a click into a coordinate
  
  # make a grid of all pairwise combos of letters
  make_data <- function(x, y) expand.grid(x = split_word(x), y = split_word(y))
  
  # structure to hold the variables
  params <- reactive({
    # length of words
    len_x <- nchar(input$x)
    len_y <- nchar(input$y)
    
    # vector of characters
    split_x <- split_word(input$x)
    split_y <- split_word(input$y)
    
    # coordinate of characters
    coord_x <- word_to_coordinates(split_x)
    coord_y <- word_to_coordinates(split_y)
    
    # grid
    data <- expand.grid(x_coordinates = coord_x, y_coordinates = coord_y)
    data <- mutate(data, x_word = mapvalues(x_coordinates, coord_x, split_x), y_word = mapvalues(y_coordinates, coord_y, split_y))
    
    # values for each box - change later
    data <- mutate(data, value = sample(1:nrow(data), nrow(data)))
    
    # output structures
    list(data = data, 
         coord_x = coord_x, split_x = split_x, len_x = len_x,
         coord_y = coord_y, split_y = split_y, len_y = len_y
    )
  }) 
  
  # plot to highlight results based on click coordinates 

  
  # function to draw the plot
  drawPlot <- function(plot_data, params){
    
    # plot data
    g <- ggplot(data = plot_data, aes(x_coordinates, y_coordinates, fill = value)) + 
      geom_tile(color = "black", size = 1.1) +
      
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
  
  # generate plot output
  output$myPlot <- renderPlot({
    g <- drawPlot(params()$data, params())
    g
  }) 
  
  output$words <- renderPrint(input$plotClick)
  

})
