## Title: run_analysis.R
## Author: Tjeerd Luykx
## Date: July 21 2015
## Description: script for analysis of course project

library(plyr)
library(reshape2)

FileName <- "getdata-projectfiles-UCI HAR Dataset.zip"

# Downloading and unzipping dataset according to lecture description:
if (!file.exists(FileName)){
        fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip "
        download.file(fileURL, FileName, method="curl")
}  
if (!file.exists("UCI HAR Dataset")) { 
        unzip(FileName) 
}

# Merge the training and the test data sets to create one data set. 
TestData <- read.table(file = "UCI HAR Dataset/test/X_test.txt", sep = "")
TrainData <- read.table(file = "UCI HAR Dataset/train/X_train.txt", sep = "")
rCombinedData <- rbind(TestData,TrainData)

# Extract only the measurements on the mean and standard deviation for each measurement. 
MeanCombined <- as.data.frame(rowMeans(rCombinedData, na.rm = FALSE, dims = 1))
SdCombined <- as.data.frame(apply(rCombinedData, 1, sd))
TotCombined <- cbind(MeanCombined,SdCombined)
colnames(TotCombined) <- c("Mean","SD")

## Function to define the file path in directory
FilePath <- function(file) {
        paste("UCI HAR Dataset","/",file,sep="")
}

# Use descriptive activity names to name the activities in the data set
ActivityFile <- FilePath("activity_labels.txt")
ActivityLabels <- read.table(ActivityFile,stringsAsFactors=FALSE)
colnames(ActivityLabels) <- c("Activity ID","Activity Label")

# Appropriately label the data set with descriptive variable names. 

## Assign values from FilePath function
TestActsFile <- FilePath("test/y_test.txt")
TrainActsFile <- FilePath("train/y_train.txt")

## Define test and train activities into table and combine them by row
TestActs <- read.table(TestActsFile,stringsAsFactors=FALSE)
TrainActs <- read.table(TrainActsFile,stringsAsFactors=FALSE)
CombinedActs <- rbind(TestActs,TrainActs)
colnames(CombinedActs) <- "Activity ID"

## Merge and form actvities labels with combined test and train data set
Acts <- join(CombinedActs,ActivityLabels, by = "Activity ID")

## Adding the Acts column to the combined dataset
rCombinedData <- cbind(Activity=Activities[,"Activity Label"],rCombinedData)

## Write table of formated data set
write.table(rCombinedData, "tidy1.txt", row.names = FALSE, quote = FALSE)

# Create a second independent tidy data set with the average of each variable for each activity and each subject

## Assign values from FilePath function
SubjTestFile <- FilePath("test/subject_test.txt")
SubjTrainFile <- FilePath("train/subject_train.txt")

## Define test and train and combine them by row
TestSubj <- read.table(SubjTestFile,stringsAsFactors=FALSE)
TrainingSubj <- read.table(SubjTrainFile,stringsAsFactors=FALSE)
TotSubj <- rbind(TestSubj,TrainingSubj)
colnames(TotSubj) <- "Subject"
rCombinedData <- cbind(TotSubj,rCombinedData)

## Reformat rCombinedData table in order
rCombinedData <- rCombinedData[order(rCombinedData$Subject,rCombinedData$Activity),]

## Converting rCombinedData object into a molten data frame
rCombinedData2 <- melt(rCombinedData,id.vars= c("Subject","Activity"))

## Reformat dataset to aggregate on subject and activity using the mean function
CastedDataSet <- dcast(rCombinedData2, Subject + Activity ~ variable, fun.aggregate = mean)

## Write table of formated data set
write.table(CastedDataSet, "tidy2.txt", row.names = FALSE, quote = FALSE)

###################
