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
      textInput("x", "Sequence 1:", value = "apple"),
      textInput("y", "Sequence 2:", value = "orange"),
      numericInput("match", "Match Score:", 1),
      numericInput("mismatch", "Mismatch Score:", 0),
      numericInput("gap", "Gap Score:", 0)
    ),

    box(
      plotOutput("myPlot", click = "plotClick")
    )
  )

)
