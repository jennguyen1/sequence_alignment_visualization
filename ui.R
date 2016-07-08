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
        column(width = 12, selectInput("alignment", "What kind of alignment?", c("local", "global")))
      ), 
      fluidRow(
        column(width = 6, numericInput("match", "Match Score:", 1)),
        column(width = 6, numericInput("mismatch", "Mismatch Score:", -1))
      ),
      fluidRow(
        column(width = 6, numericInput("space", "Space Score:", -2)),
               column(width = 6, numericInput("gap", "Gap Score:", 0))
      ),
      actionButton("submit", "Submit")
    ), 
  
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
