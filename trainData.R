# Download File
download.file("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv",
              destfile = "pml-training.csv")

# Load Data
data <- read.csv("pml-training.csv", header = TRUE)

# Create Training and Cross-Validation Sets
set.seed(34)

library(caret)
inTrain <- createDataPartition(y = data$classe, p = 0.75, list = FALSE)

training <- data[inTrain,]
xval <- data[-inTrain,]

blankOrNA <- c(12:36, 50:59, 69:83, 87:101, 103:112, 125:139, 141:150)

trainClean <- training[, -blankOrNA] # Remove columns with blank or NA values
trainClean <- trainClean[, -c(1:7)] # Remove columns with metadata

xvalClean <- xval[, -blankOrNA] # Remove columns with blank or NA values
xvalClean <- xvalClean[, -c(1:7)] # Remove columns with metadata

# Multinomial Logistic Regression

library(nnet)

multiFit <- multinom(classe ~ ., data = trainClean)
confusionMatrix(xval$classe, predict(multiFit, newdata = xvalClean))

corMat <- cor(trainClean[sapply(trainClean, is.numeric)])
diag(corMat) <- 0
which(abs(corMat) > 0.8, arr.ind = T)

highCor <- c(3, 4, 8, 9, 10, 11, 19, 24, 25, 31, 33, 34, 36, 45) 

## Remove highly correlated features
trainClean2 <- trainClean[, -highCor]
xvalClean2 <- xvalClean[, -highCor]

multiFit2 <- multinom(classe ~ ., data = trainClean2)
confusionMatrix(xval$classe, predict(multiFit2, newdata = xvalClean2))

# Support Vector Machine

library(e1071)

SVMfit <- svm(classe ~ ., data = trainClean)

confusionMatrix(xval$classe, predict(SVMfit, newdata = xvalClean))

tuneSVM <- tune(svm, classe ~ ., data = trainClean,
                validation.x = xval,
                ranges = list(gamma = 2^(-6:6), cost = 2^(-3:3)),
                tunecontrol = tune.control(sampling = "fix"))

SVMfit <- svm(classe ~ ., data = trainClean,
              gamma = tuneSVM$best.model$gamma, 
              cost = tuneSVM$best.model$cost)

confusionMatrix(xval$classe, predict(SVMfit, newdata = xvalClean))