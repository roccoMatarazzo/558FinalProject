# Running app code:
# shiny::runGitHub(repo = "558FinalProject", username = "RoccoMatarazzo", ref="main")





### DO NOT FORGET TO UPDATE YOUR README.MD FILE! ! ! ! ! ! ! 

library(caret)
library(DT)
library(shiny)
library(tidyverse)
library(RColorBrewer)
library(shinythemes)
library(shinycssloaders)

# Dataset set up
dataset <- read_csv("BaseballSavant.csv") %>%
  rename("Age" = "player_age",
         "FullName" = `last_name, first_name`,
         "Year" = "year",
         "Position" = "position",
         "xBA" = "xba",
         "xSLG" = "xslg",
         "xIso" = "xiso",
         "xWOBA" = "xwoba",
         "xOBP" = "xobp",
         "xWOBACON" = "xwobacon",
         "BarreledRate" = "barrel_batted_rate",
         "AvgExitVelo" = "exit_velocity_avg",
         "Singles" = "single",
         "Doubles" = "double",
         "Triples" = "triple",
         "HomeRuns" = "home_run",
         "AvgLaunchAngle" = "launch_angle_avg",
         "HardHitPct" = "hard_hit_percent",
         "ZswingPct" = "z_swing_percent",
         "InZonePct" = "in_zone_percent",
         "LineDrivePct" = "linedrives_percent",
         "HomeToFirstTime" = "hp_to_1b"
         ) %>%
  mutate(PositionGroup =
           case_when(Position %in% c('C', '1B', '2B', '3B', 'SS') ~ "Infield",
                     Position %in% c('LF', 'CF', 'RF') ~ "Outfield",
                     Position %in% c('DH') ~ "Designated Hitter"),
         AgeCategorical =
           case_when(Age < 26 ~ "25 or younger",
                     Age >= 26 & Age < 31 ~ "Between 26-30",
                     Age >= 31 & Age < 36 ~ "Between 31-35",
                     Age >= 36 ~ "36 or older"),
         TotalHits = Singles + Doubles + Triples + HomeRuns) 

dataset$Position <- factor(dataset$Position, 
                           levels = c("C", "1B", "2B", "3B", "SS",
                                      "LF", "CF", "RF", "DH"))

dataset$PositionGroup <- factor(dataset$PositionGroup, 
                                levels = c("Infield", "Outfield", "Designated Hitter"))

dataset$AgeCategorical <- factor(dataset$AgeCategorical, 
                                levels = c("25 or younger", "Between 26-30",
                                           "Between 31-35", "36 or older"))
# UI
ui <- fluidPage(theme = shinytheme("flatly"),
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
             tabPanel("Data Plots & Tables",
                      tabsetPanel(
                        tabPanel("Categorical",
                                 "This tab displays a categoricl data plot and
                                  contingency tables. The vertical axis is fixed
                                 to the y-variable of interest, Home Runs, where
                                 necessary.",
                                 div(style = "display: flex;",
                                     radioButtons("CatPlotType",
                                                 label = "Choose Plot Type:",
                                                 choices = c("Barchart", "Box-Plot"),
                                                 selected = "Barchart"),
                                     selectInput("CatVariable",
                                                 label="Choose Variable for Plot:",
                                                 choices = c("Position", "PositionGroup",
                                                             "AgeCategorical"),
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
                            "Contingency Tables:",
                            div(style = "display: flex;",
                            selectInput("Row", "Select Row Variable", 
                                        choices = c("Year","AgeCategorical",
                                                    "Position", "PositionGroup"),
                                        selected = "Position"),
                            selectInput("Column", "Select Column Variable", 
                                        choices = c("Year","AgeCategorical",
                                                    "Position", "PositionGroup"),
                                        selected = "Year")
                            ),
                            tableOutput("catTable")),
                        
                        tabPanel("Quantitative", "Scatter",
                                 div(style = "display: flex;",
                                     radioButtons("QuantPlotType",
                                                  label = "Choose Plot Type:",
                                                  choices = c("Density",
                                                              "Histogram",
                                                              "Scatterplot"),
                                                  selected = "Density"), 
                                     sliderInput(
                                       inputId = "yearsQuant",
                                       label = "Year Range:",
                                       value = c(2015,2023),
                                       min   = 2015,
                                       max   = 2023,
                                       step  = 1,
                                       sep   = '',
                                       ticks = FALSE,
                                       round = 0,
                                       dragRange = FALSE
                                     ),
                                     selectInput("QuantX",
                                                 label = "Choose X-Variable to Plot:",
                                                 choices = colnames(dataset 
                                                                    %>%  select(where(is.numeric)) 
                                                                    %>% select(-Year, -player_id)),
                                                 selected = "HomeRuns"),
                            conditionalPanel(condition = "input.QuantPlotType == 'Scatterplot'",
                                             selectInput("QuantY", 
                                                           "Choose Y-Variable to Plot:",
                                                           choices = colnames(dataset 
                                                                              %>%  select(where(is.numeric)) 
                                                                              %>% select(-Year, -player_id)),
                                                           selected = "HardHitPct"
                                                           )),
                            conditionalPanel(condition = "input.QuantPlotType == 'Scatterplot'",
                                             selectInput("color", 
                                                           "Color by Group?",
                                                         choices = c("No",
                                                                     "Yes - PositionGroup",
                                                                     "Yes - Position",
                                                                     "Yes - AgeCategorical"),
                                                         selected = "PositionGroup"))
                            
                                  ),
                                 plotOutput("QuantPlot")
                                )
                        )),
             tabPanel("Numerical Data Summaries", 
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
                                                         %>% select(-Year, -player_id)),
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
        tabPanel("Modeling Info", 
                 "There will be two types of models fit on our data -- a multiple linear
                 regression model and a random forest model.
                 
                 The benefits of the random forest model are
                 The drawbacks of the random forest model are
                 The benefits of the multiple linear regression model are
                 The drawbacks of the multiple linear regression model are
                 
                 Insert some mathJax
                 ",
                 "Content for Subtab 1"),
        
        tabPanel("Model Fitting",
        sidebarPanel(
        sliderInput("splitPct", "Percentage for Test/Train Split:", value = 70, min = 1, max = 99),
        selectInput("model1", "Select Predictor Variables for Random Forest:", 
                    choices = colnames(dataset %>% select(-HomeRuns, -FullName, -player_id)), multiple = TRUE),
        selectInput("model2", "Select Predictor Variables for Multiple Linear Regression:", 
                    choices = colnames(dataset %>% select(-HomeRuns, -FullName, -player_id)), multiple = TRUE),
        sliderInput("ntree", "Number of Trees for Random Forest:", min = 1, max = 1000, value = 100),
        sliderInput("folds", "Number of Cross-Validation Folds for Random Forest:",
                    min = 2, max = 15, value = 5),
        p("Recall mtry is the number of variables to randomly select.
          Therefore, make sure you select a number of variables
          greater than or equal to your mtry value."),
        sliderInput("grid", "Grid Parameters (mtry) for Random Forest:",
                    min = 5, max = 20, value = c(5,10), sep = 1),
        p("Make sure you have variables selected before you fit your model!
          If not, you will get an error!"),
        actionButton("modelFitButton", "Fit Both Models")
        ),
        mainPanel(
          p("Fit Statistics for Training and Testing Data are below:"),
          withSpinner(DT::dataTableOutput("fitStats"), type = 1),
          p("The summaries for each model are shown below. The Random 
          Forest Variable Importance plot is printed first,
          and the summary from the regression model
          is printed second."),
          withSpinner(plotOutput("varImp"), type = 1),
          withSpinner(verbatimTextOutput("modelSummary2"), type = 1)
        )),
        tabPanel("Prediction", 
                 sidebarPanel(
                   selectInput("predModel1", 
                               "Predictor Values for Model 1:", choices = NULL, multiple = TRUE),
                   actionButton("predModel1_Button", "Predict Model 1"),
                   selectInput("predModel2", 
                               "Predictor Values for Model 2:", choices = NULL, multiple = TRUE),
                   actionButton("predModel2_Button", "Predict Model 2"),
                 ),
                 mainPanel(
                   verbatimTextOutput("model1Prediction"),
                   verbatimTextOutput("model2Prediction")
                   
                 )
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Initial Data Read
  dataset <- read_csv("BaseballSavant.csv") %>%
    rename("Age" = "player_age",
           "FullName" = `last_name, first_name`,
           "Year" = "year",
           "Position" = "position",
           "xBA" = "xba",
           "xSLG" = "xslg",
           "xIso" = "xiso",
           "xWOBA" = "xwoba",
           "xOBP" = "xobp",
           "xWOBACON" = "xwobacon",
           "BarreledRate" = "barrel_batted_rate",
           "AvgExitVelo" = "exit_velocity_avg",
           "Singles" = "single",
           "Doubles" = "double",
           "Triples" = "triple",
           "HomeRuns" = "home_run",
           "AvgLaunchAngle" = "launch_angle_avg",
           "HardHitPct" = "hard_hit_percent",
           "ZswingPct" = "z_swing_percent",
           "InZonePct" = "in_zone_percent",
           "LineDrivePct" = "linedrives_percent",
           "HomeToFirstTime" = "hp_to_1b"
    ) %>%
    mutate(PositionGroup =
             case_when(Position %in% c('C', '1B', '2B', '3B', 'SS') ~ "Infield",
                       Position %in% c('LF', 'CF', 'RF') ~ "Outfield",
                       Position %in% c('DH') ~ "Designated Hitter"),
           AgeCategorical =
             case_when(Age < 26 ~ "25 or younger",
                       Age >= 26 & Age < 31 ~ "Between 26-30",
                       Age >= 31 & Age < 36 ~ "Between 31-35",
                       Age >= 36 ~ "36 or older"),
           TotalHits = Singles + Doubles + Triples + HomeRuns)
  
  dataset$Position <- factor(dataset$Position, 
                             levels = c("C", "1B", "2B", "3B", "SS",
                                        "LF", "CF", "RF", "DH"))
  
  dataset$PositionGroup <- factor(dataset$PositionGroup, 
                                  levels = c("Infield", "Outfield", "Designated Hitter"))
  
  dataset$AgeCategorical <- factor(dataset$AgeCategorical, 
                                   levels = c("25 or younger", "Between 26-30",
                                              "Between 31-35", "36 or older"))
  ############
  # DATA TAB #
  ############
  
  ### CATEGORICAL DATA
  filteredDataCat <- reactive({
    dataset %>%
      filter(Year >= input$yearsCat[1] & Year <= input$yearsCat[2]) 
  })
  
  output$catTable <- renderTable({
    rowname <- input$Row
    colname <- input$Column
    
      table <- table(
        # Row Name
        factor(filteredDataCat()[[rowname]]),
        # Column Name
        factor(filteredDataCat()[[colname]]))
      
      as.data.frame.matrix(table)
  }, include.rownames = TRUE
  )
  
  output$catPlot <- renderPlot(
  if(input$CatPlotType == "Barchart"){
    
      if(input$CatVariable == "PositionGroup"){
      ggplot(filteredDataCat(),
             aes(
               x=factor(PositionGroup)
             )) + 
        geom_bar(fill ="#377EB8", color = "black") +
        ylab("Total Count") +
        xlab("Position Group") +
        theme_classic() +
          theme(axis.text.x = element_text(size = 15),
                axis.text.y = element_text(size = 15),
                axis.title.y = element_text(size = 15),
                axis.title.x = element_text(size = 15))
        
      }else if(input$CatVariable == "Position"){
      ggplot(filteredDataCat(),
             aes(
               x=factor(Position)
             )) + 
        geom_bar(fill ="#377EB8", color = "black") +
        ylab("Total Count") +
        xlab("Position") +
          theme_classic() +
          theme(axis.text.x = element_text(size = 15),
                axis.text.y = element_text(size = 15),
                axis.title.y = element_text(size = 15),
                axis.title.x = element_text(size = 15))
      }else{
        ggplot(filteredDataCat(),
               aes(
                 x=factor(AgeCategorical)
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
    }else{
      
      if(input$CatVariable == "PositionGroup"){
        ggplot(filteredDataCat(),
               aes(
                 x=factor(PositionGroup),
                 y=HomeRuns
               )) + 
          geom_boxplot(fill ="#377EB8", color = "black") +
          ylab("Home Runs") +
          xlab("Position Group") +
          theme_classic() +
          theme(axis.text.x = element_text(size = 15),
                axis.text.y = element_text(size = 15),
                axis.title.y = element_text(size = 15),
                axis.title.x = element_text(size = 15))
        
      }else if(input$CatVariable == "Position"){
        ggplot(filteredDataCat(),
               aes(
                 x=factor(Position),
                 y=HomeRuns
               )) + 
          geom_boxplot(fill ="#377EB8", color = "black") +
          ylab("Home Runs") +
          xlab("Position") +
          theme_classic() +
          theme(axis.text.x = element_text(size = 15),
                axis.text.y = element_text(size = 15),
                axis.title.y = element_text(size = 15),
                axis.title.x = element_text(size = 15))
      }else{
        ggplot(filteredDataCat(),
               aes(
                 x=factor(AgeCategorical),
                 y=HomeRuns
               )) + 
          geom_boxplot(fill ="#377EB8", color = "black") +
          ylab("Home Runs") +
          xlab("Age Grouping") +
          theme_classic() +
          theme(axis.text.x = element_text(size = 15),
                axis.text.y = element_text(size = 15),
                axis.title.y = element_text(size = 15),
                axis.title.x = element_text(size = 15))
      }
    }
  )
  
  ### QUANTITATIVE PLOTS
  filteredDataQuant <- reactive({
    dataset %>%
      filter(Year >= input$yearsQuant[1] & Year <= input$yearsQuant[2])
  })
  
  output$QuantPlot <- renderPlot({
  
    XvarChoice <- (input$QuantX)
    YvarChoice <- (input$QuantY)
    plotChoice <- input$QuantPlotType  
    
  if(plotChoice == "Histogram"){
    # Histogram
    ggplot(filteredDataQuant(), aes_string(x=XvarChoice)) + 
      geom_histogram(bins=10, 
                     fill = "#377EB8", 
                     color = "black") + # choose density or histogram
      theme_classic() +
      ylab("Count") +
      theme(axis.text.x = element_text(size = 15),
            axis.text.y = element_text(size = 15),
            axis.title.y = element_text(size = 15),
            axis.title.x = element_text(size = 15))
    
  }else if(plotChoice == "Density"){
    # Density Plot
    ggplot(filteredDataQuant(), aes_string(x=XvarChoice)) + 
      geom_density(fill = "#377EB8") + # choose density or histogram
      theme_classic()  +
      ylab("Density") +
      theme(axis.text.x = element_text(size = 15),
            axis.text.y = element_text(size = 15),
            axis.title.y = element_text(size = 15),
            axis.title.x = element_text(size = 15),
            legend.text = element_text(size = 10))
    
  }else{
    # Scatterplot
    if(input$color == "No"){
    ggplot(filteredDataQuant(),
           aes_string(x=XvarChoice, # INPUT var
               y=YvarChoice
           )) +
      scale_color_brewer(palette = "Set1") +
      geom_point() +
      theme_bw()  +
      theme(axis.text.x = element_text(size = 15),
            axis.text.y = element_text(size = 15),
            axis.title.y = element_text(size = 15),
            axis.title.x = element_text(size = 15),
            legend.text = element_text(size = 15),
            legend.title = element_text(size = 15))
    }else{
      
      if(input$color == "Yes - PositionGroup"){
      color <- "PositionGroup"
      }else if(input$color == "Yes - Position"){
        color <- "Position"
      }else{
        color <- "AgeCategorical "
      }
      
      
      ggplot(filteredDataQuant(),
             aes_string(x=XvarChoice, # INPUT var
                        y=YvarChoice,
                        color = color
             )) +
        scale_color_brewer(palette = "Set1") +
        geom_point() +
        theme_bw()  +
        theme(axis.text.x = element_text(size = 15),
              axis.text.y = element_text(size = 15),
              axis.title.y = element_text(size = 15),
              axis.title.x = element_text(size = 15),
              legend.text = element_text(size = 15),
              legend.title = element_text(size = 15))
    }
  }
    
  })
  
  ### DATA SUMMARIES
  filteredData <- reactive({
          dataset %>%
      filter(Year >= input$years[1] & Year <= input$years[2])
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

  ################
  # MODELING TAB #
  ################
  # Model Fitting
  observeEvent(input$modelFitButton, {
    # Splitting the data into train and test sets
    
     splitPct <- input$splitPct/100
     trainingIndex <- createDataPartition(dataset$HomeRuns, 
                                              p = splitPct, 
                                              list=FALSE)
     train <- dataset[trainingIndex,]
     test <- dataset[-trainingIndex,]
     
     # Cross Validation options
     ### Specifically for RF
     ctrl <- trainControl(method = "cv", 
                          number = input$folds)
     
     grid <- expand.grid(mtry = input$grid[1]:input$grid[2])
     
     ctrlMLR <- trainControl(method = "cv", 
                          number = 3)
     
     # Random Forest
     model1 <- train(
       as.formula(paste("HomeRuns ~ ", paste(input$model1, collapse = "+"))),
       data = train,
       trControl = ctrl,
       method = "rf",
       tuneGrid = grid,
       ntree = input$ntree
     )
     
     # Multiple Linear Regression
     model2 <- train(
       as.formula(paste("HomeRuns ~ ", paste(input$model2, collapse = "+"))),
       data = train,
       trControl = ctrlMLR,
       method = "lm"
     )
     
     # Running Predicts on TRAINING/TESTING SET for RMSE 
     trainPredictsModel1 <- predict(model1, newdata = train)
     RMSE_TrainRF <- RMSE(trainPredictsModel1, train$HomeRuns)
     
     testPredictsModel1 <- predict(model1, newdata = test)
     RMSE_TestRF <- RMSE(testPredictsModel1, test$HomeRuns)
     
     trainPredictsModel2 <- predict(model2, newdata = train)
     RMSE_TrainMLR <- RMSE(trainPredictsModel2, train$HomeRuns)
     
     testPredictsModel2 <- predict(model2, newdata = test)
     RMSE_TestMLR <- RMSE(testPredictsModel2, test$HomeRuns)
     
     RMSE_Table <- cbind(RMSE_TrainRF, RMSE_TestRF,
           RMSE_TrainMLR, RMSE_TestMLR)
     
     output$fitStats <- renderDataTable({
        datatable(
          as.data.frame(RMSE_Table))
     })
     
     # Variance Importance Plot
     output$varImp <- renderPlot({
       # Var Importance Plot
       importancePlot <- varImp(model1)
       plot <- ggplot(importancePlot, 
                      aes(x = Overall, 
                          y = relevel(rownames(importancePlot),
                                      "MeanDecreaseAccuracy"))) +
         geom_bar(stat = "identity", fill = "#377EB8") +
         xlab("Variable") +
         ylab("Importance") +
         ggtitle("Variable Importance Plot") +
         theme_classic() +
         theme(axis.text.x = element_text(size = 15),
               axis.text.y = element_text(size = 15),
               axis.title.y = element_text(size = 15),
               axis.title.x = element_text(size = 15))
       
       plot})
     # Mult. Lin. Reg. Summary
     output$modelSummary2 <- renderPrint({
       print(summary(model2))
     })
     
     
     ### Prediction Tab
     observeEvent(input$fitModelsBtn, {
       # Updating selectInput choices for prediction tab
       updateSelectInput(session, "predModel1", choices = unique(dataset[[input$predModel1]]))
       updateSelectInput(session, "predModel2", choices = unique(dataset[[input$predModel2]]))
     })
     
     output$model1Prediction <- renderPrint({
       if (input$predModel1_Button > 0) {
         # Predictions for Model 1 based on selected predictor values
         Model1Prediction <- predict(model1, newdata = data.frame(input$predModel1))
         paste("Model 1 Predicted Home Runs:", Model1Prediction)
       }
     })
     
     output$model2Prediction <- renderPrint({
       if (input$predModel2_Button > 0) {
         # Predictions for Model 2 based on selected predictor values
         Model2Prediction <- predict(model2, newdata = data.frame(input$predMode21))
         paste("Model 2 Predicted Home Runs:", Model2Prediction)
       }
     })
     
     
  })

}


# Run the application
shinyApp(ui = ui, server = server)
