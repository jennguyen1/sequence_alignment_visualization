# UI Code for Sequence Alignment Visualization
# Date: June 2016
# Author: Jenny Nguyen
# Email: jnnguyen2@wisc.edu

library(shiny)
library(shinydashboard)


sidebar <-   dashboardSidebar(
  collapsed = TRUE,
  sidebarMenu(
    menuItem("Sequence Alignment", tabName = "main_app"),
    menuItem("Instructions", tabName = "instructions", icon = icon("info-circle")),
    menuItem("Github Source Code", href = "https://github.com/jennguyen1/sequence_alignment_visualization", icon = icon("github"))
  )
)

instructions <- tabItem(
  tabName = "instructions",
  verticalLayout(
    box(width = NULL,
        p("This is an interactive shiny visualization of the Smith-Waterman/Needleman-Wunsch sequence alignment algorithm. These algorithms are used to align two sequences. It is commonly used to match DNA sequences."),
        
        p("The ", span(strong("match score")),  " is added if two elements match. The ", span(strong("mismatch score")), " is the penalty if two elements do not match."),
        p("The ", span(strong("alignment")), " may be local (Smith Waterman algorithm) or global (Needleman-Wunsch algorithm). Global alignment finds the best match between entire sequences. Local alignment finds the best match between substrings of the two sequences."),
        p("The ", span(strong("gap penalty")), " is the penalty for introducing gaps into a sequence. Affine gap penalties have a gap score, the penalty for opening a gap, and a space score, the penalty for extending a gap. Linear gap penalties treat each gap the same."),
        p("Activate the algorithm by clicking on the Submit button. The dynamic programming matrix will appear in the plot below. Click on any cell in the plot to find the corresponding best alignment, plot will reset if there is no best alignment from that cell. Click anywhere in the margins of the plot to reset the plot.")
    )
  )
)

main_app <- tabItem(
  tabName = "main_app",
  verticalLayout(
    
    # inputs
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
      
      h3(textOutput("alignResults"), style = "font-family:courier"),
      h4(textOutput("alignedText1"), align = "center", style = "font-family:courier"),
      h4(textOutput("alignedBars"), align = "center", style = "font-family:courier"),
      h4(textOutput("alignedText2"), align = "center", style = "font-family:courier")
    )
  )
)

body <- dashboardBody(
  tabItems(
    instructions, 
    main_app
  ),
  span(p("Copyright (c) 2018 Jennifer N Nguyen under the MIT License"), style = "font-size:12px; color:grey")
)


dashboardPage(skin = "green",
  dashboardHeader(title = "Sequence Alignment"),
  sidebar,
  body
)
