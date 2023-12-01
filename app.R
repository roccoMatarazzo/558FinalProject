# Running app code:
# shiny::runGitHub(repo = "558FinalProject", username = "RoccoMatarazzo", ref="main")

library(shiny)
library(tidyverse)

# UI
ui <- fluidPage(
  navbarPage(
    title = "ST558 Final Project",
    id = "navbar",
    tabPanel("About", 
             
    p("This Shiny App is a culmination of the Master's Course ST558,
      Data Science with R, taught at North Carolina State University."
      ),
    p("This app explores baseball data from the popular Baseball Savant website.")
             ),
    tabPanel("Data", "Everything for Tab 2"),
    tabPanel("Modeling",
      tabsetPanel(
        id = "subtabs",
        tabPanel("Modeling Info", "Content for Subtab 1"),
        tabPanel("Model Fitting", "Content for Subtab 2"),
        tabPanel("Prediction", "Content for Subtab 3")
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  #############
  # ABOUT TAB #
  #############
  
}

# Run the application
shinyApp(ui = ui, server = server)
