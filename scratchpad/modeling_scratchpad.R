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
