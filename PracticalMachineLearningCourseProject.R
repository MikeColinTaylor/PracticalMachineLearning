

setwd("G:/Coursera/PracticalMachineLearning")

options(stringsAsFactors = FALSE)

library(caret)

set.seed(10)


#download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", destfile = "data/training.csv")
#download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", destfile = "data/testing.csv")


train <- read.csv("data/training.csv")
test <- read.csv("data/testing.csv")

#drop record numbers, user and time variables
train <- train[ , -(1:7)]
test <- test[ , -(1:7)]
train$classe <- factor(train$classe)

#convert all variables to numeric
#set missings equal to the mean
for (i in 1:(ncol(train) - 1)) {
  train[ , i] <- as.numeric(train[ , i])
  trainMean <- mean(train[ , i], na.rm = TRUE)
  train[is.na(train[ , i]) , i] <- trainMean
  test[is.na(test[ , i]) , i] <- trainMean
}

#Remove variables with near zero variance
nzv <- nearZeroVar(train, saveMetrics = TRUE)

train <- train[ , !nzv$nzv]
test <- test[ , !nzv$nzv]



#Keep 5000 back to cross validate
validationRows <- sample(nrow(train), 5000)

validation <- train[validationRows, ]
trainFinal <- train[-validationRows, ]

#Run train on small subset to find best parameters and save on processing time
gbmModelOne <- train(classe ~ ., data = trainFinal[sample(nrow(trainFinal), 1000) , ], method = "gbm")
print(gbmModelOne)

#Create grid with best parameters
grid <- data.frame(n.trees = 150, interaction.depth = 3, shrinkage = 0.1, n.minobsinnode = 10)

#Use gradient boosting machines to predict the classe variable
gbmModelFinal <- train(classe ~ ., data = trainFinal[ , ], method = "gbm", tuneGrid = grid)

print(gbmModelFinal)
#Out of bag accuracy estimate is 0.957 
#Therefore the out of sample error should be around 4.3%.


#Cross validate the error estimate
validationPredictions <- predict(gbmModelFinal, newdata = validation)

confusionMatrix(validationPredictions, validation$classe)

#Cross validated accuracy is higher, which is good!


#Output test set predictions
finalPredictions <- predict(gbmModelFinal, newdata = test)

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("output/problem_id_", i, ".txt")
    write.table(x[i], file=filename, quote=FALSE, row.names=FALSE, col.names=FALSE)
  }
}
  
pml_write_files(finalPredictions)
