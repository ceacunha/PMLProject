#libraries needed
library(caret)
library(randomForest)

# define your workspace here
workspace <- "."
setwd(workspace)

# function extracted from course's project definition page
pml_write_files <- function(x){
    n = length(x)
    for(i in 1:n){
        # function paste0's input parameter altered from original to include output directory
        filename = paste0("output/","problem_id_",i,".txt")
        write.table(x[i], file=filename, quote=FALSE, row.names=FALSE,
                    col.names=FALSE)
    }
}

# function defined to return the density of NA values within a set of values
columnThresholdNA <- function(x){
    l <- length(x)
    tNA <- sum(is.na(x))
    result <- tNA / l
    result
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
# uploading training data
originalTraining <-
    read.csv(file = "data/training.csv", stringsAsFactors = FALSE,
             na.strings = c("NA", ""))

# transforming chr-type classe column into factor-type data
originalTraining$classe <- factor(originalTraining$classe)

# removing first seven columns
finalTraining <- originalTraining[,-(1:7)]

# removing high density NA columns
finalTraining <-
    finalTraining[,which(apply(finalTraining, 2,
                               FUN = columnThresholdNA) < 0.95)]

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
             na.strings = c("NA", ""))
testData <- testData[,-(1:7)]
testData <-
    testData[,which(apply(
        testData, 2, function (x) {sum(is.na(x))}) == 0)]

# predicting values
testPredict <- predict(mRF, testData)

# writing output files
pml_write_files(testPredict)


