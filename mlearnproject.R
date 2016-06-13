trainurl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testurl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

traindata <- read.csv(trainurl, header = TRUE, sep = ",", na.strings = c("NA", "#DIV/0!", ""))
testdata <- read.csv(testurl, header = TRUE, sep = ",", na.strings = c("NA", "#DIV/0!", ""))

traindata <- traindata[, colSums(is.na(traindata)) == 0]
testdata <- testdata[, colSums(is.na(testdata)) == 0]

nzv <- nearZeroVar(traindata)
traindata <- traindata[, -nzv]
testdata <- testdata[, -nzv]

traindata <- traindata[, -(1:5)]
testdata <- testdata[, -(1:5)]

set.seed(123)
inTrain <- createDataPartition(y = traindata$classe, p = 0.6, list = FALSE)
trained <- traindata[inTrain,]
nottrained <- traindata[-inTrain,]

fold <- trainControl(method = "cv", 5)
trainrf <- train(classe ~ ., data = trained, method = "rf", trControl = fold)
trainrf

trainprd <- predict(trainrf, trained)
confusionMatrix(trainprd, trained$classe)

ntrainprd <- predict(trainrf, nottrained)
confusionMatrix(ntrainprd, nottrained$classe)
oserror <- 1 - as.numeric(confusionMatrix(ntrainprd, nottrained$classe)$overall[1])
oserror

testprd <- predict(trainrf, testdata)
testprd