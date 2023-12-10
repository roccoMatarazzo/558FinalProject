dataset <- read_csv("SavantData.csv")
dataset

# Set seed for reproducible purposes
set.seed(1234)

# p = 0.70 at the end implies 70% of data goes into the training set
proportion <- 0.80 # set by user
trainingIndex <- createDataPartition(dataset$home_run, p=proportion, list=FALSE)

# Actually splitting into testing and training
train <- dataset[trainingIndex,]
test <- dataset[-trainingIndex,]

# 5-fold cross-validation for each method using LogLoss as summary function
ctrl <- trainControl(method = "cv", classProbs = TRUE ,number = 5, 
                     summaryFunction = mnLogLoss)

# Model 1, all variables



splitPct <- 59/100
trainingIndex <- createDataPartition(dataset$HomeRuns, 
                                     p = splitPct, 
                                     list=FALSE)
train <- dataset[trainingIndex,]
test <- dataset[-trainingIndex,]

# Cross Validation options
### Specifically for RF
ctrl <- trainControl(method = "cv", 
                     number = 3)

grid <- expand.grid(mtry = 5:6)

# Random Forest
model1 <- train(
  HomeRuns ~ Age + Singles + Doubles + Triples + xBA + xIso + LineDrivePct,
  data = train,
  trControl = ctrl,
  method = "rf",
  tuneGrid = grid,
  ntree = 1
)


ctrlMLR <- trainControl(method = "cv", 
                        number = 3)

model_MLR <- train(
  HomeRuns ~ Age + Singles + PositionGroup + Position + AgeCategorical + LineDrivePct,
  data = train,
  trControl = ctrlMLR,
  method = "lm",
)

first_column <- c("Age", 
                  "Singles", 
                  "Doubles",
                  "Triples",
                  "xBA", 
                  "xIso",
                  "LineDrivePct")

second_column <- c(26, 100, 34, 5, 0.357, 0.266, 0.30)

df <- data.frame(
  "Age" = c(26), 
  "Singles" = c(100), 
  "Doubles" = c(34),
  "Extra" = c(133),
  "Triples" = c(5),
  "xBA" = c(0.332), 
  "Mock" = c(1000000),
  "xIso" = c(.276),
  "LineDrivePct" = c(0.31)
  )

df <- data.frame(
  "Year"         = c(1)
  ,"Age"             = c(1)
  ,"Singles"        = c(1)
  ,"Doubles"      = c(1)
  ,"Triples"         = c(1)
  ,"HomeRuns"        = c(1)
  ,"xBA"             = c(1)
  ,"xSLG"            = c(1)
  ,"xWOBA"          = c(1)
  ,"xOBP"            = c(1)
  ,"xIso"            = c(1)
  ,"xWOBACON"        = c(1)
  ,"AvgExitVelo"     = c(1)
  ,"AvgLaunchAngle"  = c(1)
  ,"BarreledRate"   = c(1)
  ,"HardHitPct"     = c(1)
  ,"ZswingPct"       = c(1)
  ,"InZonePct"       = c(1)
  ,"LineDrivePct"    = c(1)
  ,"HomeToFirstTime"= c(1)
  ,"TotalHits"= c(1)
  ,"PositionGroup"  = c("Infield")
  ,"AgeCategorical"  = c("Between 26-30")
  ,"TotalHits"= c(1)
  ,"Position" = c("C")
)
#   
#   ,"PositionGroupInfield" = c(1)
#   ,"PositionGroupOutfield" = c(1)
#   ,"PositionGroupDesignated Hitter" = c(1)
#   ,"Position1B"                       = c(1)
#   ,"Position2B"                     = c(1)
#   ,"Position3B"                       = c(1)
#   ,"PositionSS"                     = c(1)
#   ,"PositionLF"                       = c(1)
#   ,"PositionCF"                     = c(1)
#   ,"PositionRF"                     = c(1)
#   ,"PositionDH"                     = c(1)
#   ,"AgeCategoricalBetween 26-30"    = c(1)
#   ,"AgeCategoricalBetween 31-35"    = c(1)
#   ,"AgeCategorical36 or older"         = c(1)
# )


prediction <- predict(model_MLR, df)

print(paste("Predicted # of Home Runs: ", prediction))

testVector <- c("Object1", "Object2", "Object3")
testVector[1]
testVector[2]
testVector[3]
testVector[4]
