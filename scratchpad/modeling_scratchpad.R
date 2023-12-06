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
logistic_model_1 <- train(Diabetes_binary ~ HighBP + HighChol + BMI + PhysActivity + 
                            Fruits + Veggies + Sex + Age + Smoker + Stroke, 
                          data = train, method = "glm", family = "binomial", 
                          trControl = ctrl, metric = "logLoss")
