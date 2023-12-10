# Running app code:
# shiny::runGitHub(repo = "558FinalProject", username = "RoccoMatarazzo", ref="main")
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
ui <- fluidPage(
  theme = shinytheme("flatly"),
  navbarPage(
    title = "ST558 Final Project",
    id = "navbar",
    tabPanel("About", 
             # Brief Intro
             p("This Shiny App is a culmination of the Master's Course ST558,
      Data Science with R, taught at North Carolina State University."),
             
             p("This app explores baseball data from the website Baseball Savant.
      The general goal of this app is to predict a player's Home Run total for a season using a variety of statistics.
      Home Runs are when a player scores themselves on their own batted ball, which most often occurs when 
      the ball is hit over the fence within fair territory. The dataset used in this app contains
      1245 total observations from Major League Baseball spanning the 2015 to 2023 seasons. Basic variables within the dataset 
      include a player's age and position. Advanced variables include xSLG (expected 
      Slugging Percentage), Avg. Exit Velocity, and more. All of this data is on a season level. For example, Shohei Ohtani's
      Average Exit Velocity of 94.4 for the 2023 season is an average of all of his recorded exit velocities over the course of that season.
      
      
      The data source website, Baseball Savant, followed by a detailed glossary, are linked
      at the bottom of this tab."),
             # Data Explanation
             
             # Tab Explanation
             p("The Data Exploration tab will allow the user to explore nummerical and
      categorical summaries of the supplied data. Functionality includes the ability
      to plot data, view the data in dataTable format, and view the correlation between particular variables.
      The user should use this tab to identify patterns and relationships within the dataset. This tab 
      essentially serves as the 'EDA' part of an analysis, which should always be done prior to doing
      any model fitting! Moreover, this tab should familiarize you with the data and help you 
      prepare to choose variables for the Modeling tab."),
             p("The Modeling tab contains three subtabs. The inital subtab, Modeling Info,
      will share details about the two models we are using: a Generalized Linear Model and
      Random Forest model. The Model Fitting tab will allow the user to directly choose parameters that
      effect the models and their interpratation. These parameters include the training/testing split,
      variables for each model, and tuning parameters among other options. 
      Finally, users can use the final subtab, Prediction, to predict the number of Home Runs for each
      of their fit models by entering values for the variables they chose."),
             
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
                                   p("This tab displays a categorical data plot of choice and
                                  contingency tables. The vertical axis is fixed
                                 to the y-variable of interest, Home Runs, for the Box-Plot."),
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
                                   p("Contingency Tables:"),
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
                          
                          tabPanel("Quantitative", 
                                   p("This tab displays a quantitative plot of choice.
                                   Once the scatterplot option is chosen, the correlation
                                   between the two variables of choice will be displayed at
                                   the bottom of the page. Also, an option to color by 
                                   a particular group is available with the scatterplot."),
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
                                                                      %>% select(-Year, -player_id, -HomeToFirstTime)),
                                                   selected = "HomeRuns"),
                                       conditionalPanel(condition = "input.QuantPlotType == 'Scatterplot'",
                                                        selectInput("QuantY", 
                                                                    "Choose Y-Variable to Plot:",
                                                                    choices = colnames(dataset 
                                                                                       %>%  select(where(is.numeric)) 
                                                                                       %>% select(-Year, -player_id, -HomeToFirstTime)),
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
                                   ,conditionalPanel(condition = "input.QuantPlotType == 'Scatterplot'",
                                                     verbatimTextOutput("correlation"))
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
                                                           %>% select(-Year, -player_id, -HomeToFirstTime)),
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
                        p("There will be two types of models fit on our data -- a Generalized Linear
                Model (GLM) and a Random Forest (RF) model. The reason we're moving forward with
                 a GLM rather than a more standard Multiple Ordinary Least Squares model is that the response variable is not normally 
                 distributed. When running the Shapiro-Wilk Test, the data resulted in an incredibly low p-value,
                 indicating that there is significant evidence to suggest that HomeRuns follows a 
                 non-normal distibution. Therefore, we are treating this as a Poisson distribution. 
                   Poisson distributions count the number of occurences in a given time interval. Here we are trying to predict
                   the number of Home Runs (count) in a given season (time interval) for a player. "),
                        p("The specialty of GLMs is that they each have a unique link function. The link function for the Poisson is log.
                 The link function determines the relationship between the linear predictors 
                 and the mean of the distribution, which is Lamda for Poisson.
                   "),
                        withMathJax(),
                        uiOutput('explanation'),
                        p("A benefit of a Genearlized Linear Model is that it allows for responses from
                   non-normal distributions, like in our case. It is also easy to interpret, specifically in
                   comparison to a Random Forest. 
                   Some drawbacks of GLMs include that they aresensitve to outlier data and predictor variables should be uncorrelated
                   (can run into issues if a model involves multicollinearity). In comparison
                   to a Random Forest, a GLM lacks built-in variable selection. A benefit of a Random Forest is that 
                   it generally bodes well for prediction. RFs also have built in variable selection and are applicable to many types of problems. 
                   Along with the aforementioned interpretability issue, Random Forests are built
                   using a 'greedy algorithm' which is not always efficient, and can be 
                   computationally expensive."),
               ),
               tabPanel("Model Fitting",
                        sidebarPanel(
                          sliderInput("splitPct", "Percentage for Test/Train Split:", value = 70, min = 1, max = 99),
                          selectInput("model1", "Select Predictor Variables for Random Forest:", 
                                      choices = colnames(dataset %>% select(-HomeRuns, -FullName, -player_id, -HomeToFirstTime)), multiple = TRUE),
                          selectInput("model2", "Select Predictor Variables for Generalized Linear Regression:", 
                                      choices = colnames(dataset %>% select(-HomeRuns, -FullName, -player_id, -HomeToFirstTime)), multiple = TRUE),
                          sliderInput("ntree", "Number of Trees for Random Forest:", min = 1, max = 1000, value = 100),
                          sliderInput("folds", "Number of Cross-Validation Folds for Random Forest:",
                                      min = 2, max = 15, value = 5),
                          p("Recall mtry is the number of variables to randomly select.
          Therefore, make sure you select a number of variables
          greater than or equal to your mtry range. For example, if you range from 5 to 7, make sure at least 
                            7 variables are selected."),
                          sliderInput("grid", "Grid Parameter Range (mtry) for Random Forest:",
                                      min = 5, max = 20, value = c(5,10), sep = 1),
                          p("Make sure you have variables selected before you fit your model!
          If not, you will get an error!"),
                          actionButton("modelFitButton", "Fit Both Models")
                        ),
                        mainPanel(
                          p("These outputs will display the loading graphic until the button 'Fit Both Models' is pressed."),
                          p("Fit Statistics for Training and Testing Data are below. Note the deviance in
            the summary table bewlo the plot as it is important for Poisson distributions. We
            hope that Residual Deviance/1  is approximately equal to 1."),
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
                          numericInput("PredYear", "Year:", value = round(mean(dataset$Year),3) , min = 0, max = 2023)
                          ,numericInput("PredAge", "Age:", value = round(mean(dataset$Age),3), min = 0, max = 100)
                          ,numericInput("PredSingles", "Singles:", value = round(mean(dataset$Singles),3), min = 0, max = 500)
                          ,numericInput("PredDoubles", "Doubles:", value = round(mean(dataset$Doubles),3), min = 0, max = 500)
                          ,numericInput("PredTriples", "Triples:", value = round(mean(dataset$Triples),3), min = 0, max = 500)
                          ,numericInput("PredxBA", "xBA:", value = round(mean(dataset$xBA),3), min = 0, max = 1)
                          ,numericInput("PredxSLG", "xSLG:", value = round(mean(dataset$xSLG),3), min = 0, max = 1)
                          ,numericInput("PredxWOBA", "xWOBA:", value = round(mean(dataset$xWOBA),3), min = 0, max = 1)
                          ,numericInput("PredxOBP", "xOBP:", value = round(mean(dataset$xOBP),3), min = 0, max = 1)
                          ,numericInput("PredxIso", "xIso:", value = round(mean(dataset$xIso),3), min = 0, max = 1)
                          ,numericInput("PredxWOBACON", "xWOBACON:", value = round(mean(dataset$xWOBACON),3), min = 0, max = 1)
                          ,numericInput("PredAvgExitVelo", "AvgExitVelo:", value = round(mean(dataset$AvgExitVelo),3), min = 0, max = 500)
                          ,numericInput("PredAvgLaunchAngle", "AvgLaunchAngle:", value = round(mean(dataset$AvgLaunchAngle),3), min = 0, max = 500)
                          ,numericInput("PredBarreledRate", "BarreledRate:", value = round(mean(dataset$BarreledRate),3), min = 0, max = 500)
                          ,numericInput("PredHardHitPct", "HardHitPct:", value = round(mean(dataset$HardHitPct),3), min = 0, max = 500)
                          ,numericInput("PredZswingPct", "ZswingPct:", value = round(mean(dataset$ZswingPct),3), min = 0, max = 500)
                          ,numericInput("PredInZonePct", "InZonePct:", value = round(mean(dataset$InZonePct),3), min = 0, max = 500)
                          ,numericInput("PredLineDrivePct", "LineDrivePct:", value = round(mean(dataset$LineDrivePct),3), min = 0, max = 500)
                          ,numericInput("PredTotalHits", "TotalHits:", value = round(mean(dataset$TotalHits),3), min = 0, max = 500)
                          ,selectInput("PredPositionGroup", "PositionGroup:", choices = c("Infield","Outfield","Designated Hitter"),
                                       selected = "Infield")
                          ,selectInput("PredAgeCategorical", "AgeCategorical:", choices = c('25 or younger','Between 26-30','Between 31-35','36 or older'), 
                                       selected = "25 or younger")
                          ,selectInput("PredPosition", "Position:", choices = c('C','1B','2B','3B','SS','LF','CF','RF','DH'),  selected = "C"),
                          actionButton("predictButton", "Make Prediction!")
                        ),
                        mainPanel(
                          p("Prior to using this tab, please ensure you fit an appropriate model on the 'Model Fitting' tab.
                   Please input values for each of the variables you selected to form a prediction on your model.
                     The values that originally occupy the input boxes are simply the mean of that variable from the dataset. 
                     Note the levels for each categorical variable when inputting them in particular."),
                          p("PositionGroup's levels are 'Infield', 'Outfield', and 'Designated Hitter'.
                     Position's levels are 'C', '1B', '2B', '3B', 'SS', 'LF', 'CF', 'RF', 'DH'.
                     AgeCategorical's levels are '25 or younger', 'Between 26-30', 'Between 31-35', '36 or older'."),
                          p("As a reminder, below are the models you've selected for the Random Forest Model:"),
                          verbatimTextOutput("model1_vars_selected"),
                          p("As a reminder, below are the models you've selected for the Generalized Linear Regression Model:"),
                          verbatimTextOutput("model2_vars_selected"),
                          p("The following two sentences will further populate once you have pressed the 'Make Prediction!' button."),   
                          p("Your Random Forest Model returns the following,"),
                          verbatimTextOutput("model1Outcome"),
                          p("Your Generalized Linear Regression Model returns the following,"),
                          verbatimTextOutput("model2Outcome")
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
  
  output$explanation <- renderUI({
    withMathJax(
      helpText('The link function for the Poisson
    $$X\\beta = \\ln(\\mu)$$')
    )
  })
  
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
  
  
  output$correlation  <- renderPrint({
    if(input$QuantPlotType == "Scatterplot"){
      
      var1 <- (input$QuantX)
      var2 <- (input$QuantY)
      
      correlationValue <- cor(filteredDataQuant()[[var1]], filteredDataQuant()[[var2]])
      
      print(paste("The correlation between these two variables is:", correlationValue))
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
  fitted_Model1 <- reactiveVal(NULL)
  fitted_Model2 <- reactiveVal(NULL)
  
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
    
    # Random Forest
    model_1 <- train(
      as.formula(paste("HomeRuns ~ ", paste(input$model1, collapse = "+"))),
      data = train,
      trControl = ctrl,
      method = "rf",
      tuneGrid = grid,
      ntree = input$ntree
    )
    fitted_Model1(model_1)
    
    # Generalized Linear Regression
    model_2 <- glm(
      as.formula(paste("HomeRuns ~ ", paste(input$model2, collapse = "+"))),
      data = train,
      family = "poisson"
    )
    fitted_Model2(model_2)
    
    
    # Running Predicts on TRAINING/TESTING SET for RMSE 
    trainPredictsModel1 <- predict(fitted_Model1(), newdata = train)
    RMSE_TrainRF <- RMSE(trainPredictsModel1, train$HomeRuns)
    
    testPredictsModel1 <- predict(fitted_Model1(), newdata = test)
    RMSE_TestRF <- RMSE(testPredictsModel1, test$HomeRuns)
    
    trainPredictsModel2 <- predict(fitted_Model2(), newdata = train, type = "response")
    RMSE_TrainGLM <- RMSE(trainPredictsModel2, train$HomeRuns)
    
    testPredictsModel2 <- predict(fitted_Model2(), newdata = test, type = "response")
    RMSE_TestGLM <- RMSE(testPredictsModel2, test$HomeRuns)
    
    RMSE_Table <- cbind(RMSE_TrainRF, RMSE_TestRF,
                        RMSE_TrainGLM, RMSE_TestGLM)
    
    output$fitStats <- renderDataTable({
      datatable(
        as.data.frame(RMSE_Table))
    })
    
    # Variance Importance Plot
    output$varImp <- renderPlot({
      # Var Importance Plot
      importancePlot <- varImp(fitted_Model1())
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
      print(summary(fitted_Model2()))
    })
    
  })
  
  output$model1_vars_selected <- renderPrint({
    
    print(input$model1)
    
  })
  
  output$model2_vars_selected <- renderPrint({
    
    print(input$model2)
    
  })
  
  
  # Predictor Inputs
  
  pred_df <- reactive({
    
    pred_df <- data.frame(
      "Year"         = c(input$PredYear)
      ,"Age"          = c(input$PredAge)
      ,"Singles"      = c(input$PredSingles)
      ,"Doubles"      = c(input$PredDoubles)
      ,"Triples"      = c(input$PredTriples)
      ,"xBA"          = c(input$PredxBA)
      ,"xSLG"         = c(input$PredxSLG)
      ,"xWOBA"        = c(input$PredxWOBA)
      ,"xOBP"         = c(input$PredxOBP)
      ,"xIso"         = c(input$PredxIso)
      ,"xWOBACON"     = c(input$PredxWOBACON)
      ,"AvgExitVelo"   = c(input$PredAvgExitVelo)
      ,"AvgLaunchAngle"  = c(input$PredAvgLaunchAngle)
      ,"BarreledRate"    = c(input$PredBarreledRate)
      ,"HardHitPct"      = c(input$PredHardHitPct)
      ,"ZswingPct"       = c(input$PredZswingPct)
      ,"InZonePct"       = c(input$PredInZonePct)
      ,"LineDrivePct"    = c(input$PredLineDrivePct)
      ,"TotalHits"       = c(input$PredTotalHits)
      ,"PositionGroup"   = c(input$PredPositionGroup)
      ,"AgeCategorical"  = c(input$PredAgeCategorical)
      ,"Position"        = c(input$PredPosition)
    )
    
    pred_df
    
  })
  
  observeEvent(input$predictButton, {
    
    model1_Prediction <- predict(fitted_Model1(), pred_df())
    model2_Prediction <- predict(fitted_Model2(), pred_df(), type = "response")
    
    output$model1Outcome <- renderPrint({
      
      print(paste("Predicted # of Home Runs:", model1_Prediction))
      
    })
    output$model2Outcome <- renderPrint({
      
      print(paste("Predicted # of Home Runs:", model2_Prediction))
      
    })
    
  })
  
}


# Run the application
shinyApp(ui = ui, server = server)
