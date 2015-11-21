#libraries needed
library(caret)
library(randomForest)

# define your workspace here
workspace <- "."
setwd(workspace)

# function provided by project definition page
# function paste0's parameter altered to include output directory 
pml_write_files = function(x){
    n = length(x)
    for(i in 1:n){
        filename = paste0("output/","problem_id_",i,".txt")
        write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
    }
}

# verify if the data directory was created
if (!dir.exists("data")){
    dir.create("data")
}

#verify if the output directoy was created
if (!dir.exists("output")){
    dir.create("output")
}

# verify if the training data set was already downloaded
if(!file.exists("data/training.csv")){
    trainingFileURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
    download.file(trainingFileURL, destfile = "data/training.csv",
                  method = "curl", quiet = TRUE)
}

# verify if the test data set was already downloaded
if(!file.exists("data/test.csv")){
    testFileURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
    download.file(testFileURL, destfile = "data/test.csv",
                  method = "curl", quiet = TRUE)
}

# loading training data
originalTraining <-
    read.csv(file = "data/training.csv", stringsAsFactors = FALSE,
             na.strings = c("NA", "", " "))
originalTraining$classe <- factor(originalTraining$classe)

# removing descriptive and identification column
reducedTraining <- originalTraining[,-(1:7)]

# identifying and removing columns made primarily of NA's values
finalTraining <-
    reducedTraining[,which(apply(
        reducedTraining, 2, function (x) {sum(is.na(x))}) == 0)]

# creating training and test sets from the finalTraining in order to
# create the best decision model
isTrain <- createDataPartition(finalTraining$classe, p=.7, list = FALSE)
trainBase <- finalTraining[isTrain,]
trainTest <- finalTraining[-isTrain,]

# executing train operation to retrieve the best model
mRF <- randomForest(formula = classe ~ ., data = trainBase)

trainTestPredict <- predict(mRF, trainTest)
confusionMatrix(trainTest$classe, trainTestPredict)

# loading and reducing test data
testData <-
    read.csv(file = "data/test.csv", stringsAsFactors = FALSE,
             na.strings = c("NA", "", " "))
testData <- testData[,-(1:7)]
testData <-
    testData[,which(apply(
        testData, 2, function (x) {sum(is.na(x))}) == 0)]

# predicting values
testPredict <- predict(mRF, testData)

# writing output files
pml_write_files(testPredict)


