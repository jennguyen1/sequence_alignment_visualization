# UI Code for Sequence Alignment Visualization
# Date: June 2016
# Author: Jenny Nguyen
# Email: jnnguyen2@wisc.edu

library(shiny)
library(shinydashboard)

dashboardPage(skin = "green", 

  # app title
  dashboardHeader(title = "Sequence Alignment"),

  # disable sidebar
  dashboardSidebar(disable = TRUE),

  # UI body
  dashboardBody( verticalLayout(
    
    # inputs box
    box(
      title = "Choose Input Parameters", width = NULL, 
      solidHeader = TRUE, status = "success", collapsible = TRUE,
      
      fluidRow(
        column(width = 6, textInput("x", "Sequence 1:", value = "ttagagt")),
        column(width = 6, textInput("y", "Sequence 2:", value = "atagggtta"))
      ), 
      fluidRow(
        column(width = 6, radioButtons("alignment", "What kind of alignment?", c("local", "global"))),
        column(width = 6, radioButtons("gap_penalty", "What kind of gap penalty?", c("linear", "affine")))
      ), 
      fluidRow(
        column(width = 6, numericInput("match", "Match Score:", 1, min = 0)),
        column(width = 6, numericInput("mismatch", "Mismatch Score:", -1, max = 0))
      ),
      fluidRow(
        column(width = 6, uiOutput("space_option")), 
        column(width = 6, uiOutput("gap_option"))
      ),
      actionButton("submit", "Submit")
    ), 
  
    # outputs
    box(
      title = "Alignment Results", width = NULL,
      solidHeader = TRUE, status = "success",
      
      plotOutput("myPlot", click = "plotClick", height = "700px"),

      h3(textOutput("alignResults")),
      h4(textOutput("alignedText1"), align = "center"), 
      h4(textOutput("alignedBars"), align = "center"), 
      h4(textOutput("alignedText2"), align = "center")
    )
    
  ))
)
