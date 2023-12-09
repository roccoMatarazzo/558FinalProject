# Running app code:
# shiny::runGitHub(repo = "558FinalProject", username = "RoccoMatarazzo", ref="main")





### DO NOT FORGET TO UPDATE YOUR README.MD FILE! ! ! ! ! ! ! 

library(caret)
library(shiny)
library(tidyverse)
library(RColorBrewer)

dataset <- read_csv("BaseballSavant.csv") %>%
  rename("age" = "player_age",
         "fullName" = `last_name, first_name`) %>%
  mutate(PositionGroup =
           case_when(position %in% c('C', '1B', '2B', '3B', 'SS') ~ "Infield",
                     position %in% c('LF', 'CF', 'RF') ~ "Outfield",
                     position %in% c('DH') ~ "Designated Hitter"))


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
      the ball is hit over the fence within fair territory. A brief description of the dataset is as follows. 
      There are 1245 observations from each Major League Baseball season since 2015. Basic variables within the dataset 
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
    tabPanel("Data", 
    tabsetPanel(
             tabPanel("Data Plots",
                      tabsetPanel(
                        tabPanel("Categorical",
                                 "TEXT ABOUT STUFF",
                                 div(style = "display: flex;",
                                     selectInput("CatVariable",
                                                 label="Choose Variable for Barchart and Table:",
                                                 choices = c("Position", "PositionGroup"),
                                                 selected = "Position"
                                     ), 
                                     sliderInput(
                                       inputId = "yearsCat",
                                       label = "Year Range:",
                                       value = c(2015,2023),
                                       min   = 2015,
                                       max   = 2023,
                                       step  = 1,
                                       sep   = '',
                                       ticks = FALSE,
                                       round = 0,
                                       dragRange = FALSE
                                     )
                                 ),
                            plotOutput("catPlot"),
                            textOutput("catText")),
                        tabPanel("Quantitative", "Scatter") 
                      )),
             tabPanel("Data Summaries", 
                      "
                      This tab holds
                      important nummerical summary values
                      as well as a basic data table.
                      The year slider will filter the dataset and impact 
                      the results shown in data table and numerical summary.
                      The top table will display the numerical summary
                      for the variable selected in the drop down menu.
                      ",
                      
                      div(style = "display: flex;",
                          selectInput("summaryType",
                                      label="Choose Type of Summary:",
                                      choices = c("All","Min","Max","Mean", "Median",
                                                  "Variance", "Std. Dev.", "IQR"),
                                      selected = "All"
                          ),
                          selectInput("varSumChoice",
                                      label="Choose Variable for Numerical Summary:",
                                      choices = colnames(dataset 
                                                         %>%  select(where(is.numeric)) 
                                                         %>% select(-year, -player_id)),
                                      selected = "age"
                          ), 
                          sliderInput(
                            inputId = "years",
                            label = "Year Range:",
                            value = c(2015,2023),
                            min   = 2015,
                            max   = 2023,
                            step  = 1,
                            sep   = '',
                            ticks = FALSE,
                            round = 0,
                            dragRange = FALSE
                          )
                      ),
                      DT::dataTableOutput("mendedSummary"),
                      DT::dataTableOutput("dataTableSummary"))
            )
           ),
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
  dataset <- read_csv("BaseballSavant.csv") %>%
    rename("age" = "player_age",
           "fullName" = `last_name, first_name`) %>%
    mutate(PositionGroup =
             case_when(position %in% c('C', '1B', '2B', '3B', 'SS') ~ "Infield",
                       position %in% c('LF', 'CF', 'RF') ~ "Outfield",
                       position %in% c('DH') ~ "Designated Hitter"))

  ############
  # DATA TAB #
  ############
  
  ### CATEGORICAL DATA
  # CatVariable
  filteredDataCat <- reactive({
    dataset %>%
      filter(year >= input$yearsCat[1] & year <= input$yearsCat[2])
  })
  
  output$catText <- renderText(
    
  )
  
  output$catPlot <- renderPlot(
    
    if(input$CatVariable == "PositionGroup"){
    ggplot(filteredDataCat(),
           aes(
             x=factor(PositionGroup, levels = c("Infield", "Outfield",
                                                "Designated Hitter"))
           )) + 
      geom_bar(fill ="#377EB8", color = "black") +
      ylab("Total Count") +
      xlab("Position Group") +
      theme_classic()
    }else{
    ggplot(filteredDataCat(),
           aes(
             x=factor(position, levels = c("C", "1B", "2B", "3B", "SS",
                                           "LF", "CF", "RF", "DH"))
           )) + 
      geom_bar(fill ="#377EB8", color = "black") +
      ylab("Total Count") +
      xlab("Position") +
        theme_classic() +
        theme(axis.text.x = element_text(size = 15),
              axis.text.y = element_text(size = 15),
              axis.title.y = element_text(size = 15),
              axis.title.x = element_text(size = 15))
    }
  )
  
  ### DATA SUMMARIES
  filteredData <- reactive({
          dataset %>%
      filter(year >= input$years[1] & year <= input$years[2])
    })
  
  output$dataTableSummary <- DT::renderDataTable({
    datatable(filteredData(), 
              options = list(pageLength = 10,
                             scrollX=TRUE)
              )
  })

  output$mendedSummary <- DT::renderDataTable({
    var_name <- input$varSumChoice
    summaryType <- input$summaryType
    
    Max <- max(filteredData()[[var_name]])
    Min <- min(filteredData()[[var_name]])
    Mean <- mean(filteredData()[[var_name]])
    Median <- median(filteredData()[[var_name]])
    Variance <- var(filteredData()[[var_name]])
    `Std. Dev.` <- sd(filteredData()[[var_name]])
    IQR <- IQR(filteredData()[[var_name]])
    mended <- cbind(Min, Max, Mean, Median, Variance, `Std. Dev.`, IQR)

    if(summaryType == "All"){
    datatable(mended,
              options = list(pageLength = 10))
    }else if(summaryType == "Max"){
      datatable(as.data.frame(Max),
                options = list(pageLength = 10))
    }
    else if(summaryType == "Min"){
      datatable(as.data.frame(Min),
                options = list(pageLength = 10))
    }
    else if(summaryType == "Variance"){
      datatable(as.data.frame(Variance),
                options = list(pageLength = 10))
    }
    else if(summaryType == "Std. Dev."){
      datatable(as.data.frame(`Std. Dev.`),
                options = list(pageLength = 10))
    }
    else if(summaryType == "Mean"){
      datatable(as.data.frame(Mean),
                options = list(pageLength = 10))
    }
    else if(summaryType == "Median"){
      datatable(as.data.frame(Median),
                options = list(pageLength = 10))
    }
    else{
      datatable(as.data.frame(IQR),
                options = list(pageLength = 10))
    }
  
  })

  
}

# Run the application
shinyApp(ui = ui, server = server)
