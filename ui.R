# UI Code for Sequence Alignment Visualization
# Date: June 2016
# Author: Jenny Nguyen
# Email: jnnguyen2@wisc.edu

library(shiny)
library(shinydashboard)

dashboardPage(

  # App Title
  dashboardHeader(title = "Sequence Alignment"),

  # Sidebar Inputs
  dashboardSidebar(disable = TRUE),

  # Outputs
  dashboardBody(

    box(
      title = "Choose Input Parameters",
      solidHeader = TRUE, status = "primary", collapsible = TRUE,
      textInput("x", "Sequence 1:", value = "ttagagt"),
      textInput("y", "Sequence 2:", value = "atagggtta"),
      selectInput("alignment", "What kind of alignment?", c("local", "global")),
      numericInput("match", "Match Score:", 1),
      numericInput("mismatch", "Mismatch Score:", -1),
      numericInput("space", "Space Score:", -2),
      numericInput("gap", "Gap Score:", 0),
      actionButton("submit", "Submit")
    ),

    box(
      plotOutput("myPlot", click = "plotClick"),
      textOutput("alignedText1"), textOutput("alignedText2")
    )
  )

)
