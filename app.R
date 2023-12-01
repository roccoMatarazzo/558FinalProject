library(shiny)
library(tidyverse)

# UI
ui <- fluidPage(
  navbarPage(
    title = "ST558 Final Project",
    id = "navbar",
    tabPanel("About", "Everything for Tab 1!"),
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
  
}

# Run the application
shinyApp(ui = ui, server = server)
