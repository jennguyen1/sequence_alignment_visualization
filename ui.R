
library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  # Application title
  titlePanel("hi"
  ),

  # Show a plot of the generated distribution
  mainPanel(
    textInput("x", "1", value = "apple"),
    textInput("y", "2", value = "banana"),
    plotOutput("myPlot", click = "plotClick"),
    plotOutput("newPlot"),
    verbatimTextOutput("words")
  )
)
)
