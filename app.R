# Running app code:
# shiny::runGitHub(repo = "558FinalProject", username = "RoccoMatarazzo", ref="main")





### DO NOT FORGET TO UPDATE YOUR README.MD FILE! ! ! ! ! ! ! 



library(caret)
library(shiny)
library(tidyverse)

# UI
ui <- fluidPage(
  navbarPage(
    title = "ST558 Final Project",
    id = "navbar",
    tabPanel("About", 
    # Brief Intro
    p("This Shiny App is a culmination of the Master's Course ST558,
      Data Science with R, taught at North Carolina State University."),
    
    p("This app explores baseball data from the website Baseball Savant.
      The general goal of this app is to predict a player's Home Run total for the 2023 season.
      Home Runs are when a player scores themself on their own batted ball, which most often occurs when 
      the ball is hit over the fence within fair territory. Basic variables within the dataset 
      include a player's age, position, and league. Advanced variables include xSLG (expected 
      Slugging Percentage), Avg. Exit Velocity, and more. 
      
      The data source website, Baseball Savant, followed by a detailed glossary, are linked
      at the bottom of this tab."),
    # Data Explanation
    
    # Tab Explanation
    p("The Data Exploration tab will allow the user to explore nummerical and
      categorical summaries of the supplied data. Functionality includes the ability
      to plot data, view the data in dataTable format, and view correlation within the data."),
    p("The Modeling tab contains three subtabs. The inital subtab, Modeling Info,
      will give more deatil abotu the two models we are using: a multiple linear regression model and
      random forest model. The Model Fitting tab will allow the user to input desired parameters that
      effect the models and their interpratation, such as the training/testing split and fit statisitcs,
      among others. Users can test prediction in the final subtab, Prediction, by entering values
      for coefficients of the fitted models."),
  
    p(""),
    img(src ="spraychart.png", align = "left", height = "300px"),
    img(src = "aaronJudge.jpg", height = "300px"),
    #,img(src = "baseballSavant.jpg", height = "300px")
    
    p(""),
    a("https://baseballsavant.mlb.com/"),
    p(""),
    a("https://baseballsavant.mlb.com/csv-docs")
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
  # Initial Data Read
  dataset <- read_csv("SavantData.csv")
  
  #############
  # ABOUT TAB #
  #############
  
}

# Run the application
shinyApp(ui = ui, server = server)
