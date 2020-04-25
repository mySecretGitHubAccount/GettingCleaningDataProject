library('dplyr')
library('plyr')

## Get file from the Web & and exctract it into a data frame using "read.table"
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
if(!file.exists("data.zip")){
    download.file(url, "data.zip")
}
unzip("data.zip")

# 'README.txt'
# 'features_info.txt': Shows information about the variables used on the feature vector.
# 'features.txt': List of all features.
# 'activity_labels.txt': Links the class labels with their activity name.
# 'train/X_train.txt': Training set.
# 'train/y_train.txt': Training labels.
# 'test/X_test.txt': Test set.
# 'test/y_test.txt': Test labels.

# Prepare dataFrame name row
# 4. Appropriately labels the data set with descriptive variable names.
namesData <- read.table("UCI HAR Dataset/features.txt", header=FALSE ,sep="")
namesData$V2 <- gsub("[()]|-|,","",namesData$V2)
namesDataVec <- as.vector(make.unique(namesData$V2))

# 1. Merges the training and the test sets to create one data set.
trainingSet <- read.table("UCI HAR Dataset/train/X_train.txt", header=FALSE ,sep="")
colnames(trainingSet) <- namesDataVec
str(trainingSet)

testSet <- read.table("UCI HAR Dataset/test/X_test.txt", header=FALSE ,sep="")
colnames(testSet) <- namesDataVec
str(testSet)

tranTestSet<-rbind(trainingSet,testSet)
write.table(x = tranTestSet[100,], file = "tidyData.txt",  sep = ",", col.names=T, append=T)
str(tranTestSet)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
logicalVec <- (grepl("mean|std",namesDataVec))
tranTestMeanStdSet <- tranTestSet[logicalVec,]
write.table(x = tranTestMeanStdSet[100,], file = "tidyData.txt",  sep = ",", col.names=T, append=T)
str(tranTestMeanStdSet)

# 3. Uses descriptive activity names to name the activities in the data set
activityData    <- read.table("UCI HAR Dataset/activity_labels.txt", header=FALSE ,sep="")

trainYSet       <- read.table("UCI HAR Dataset/train/y_train.txt", header=FALSE ,sep="")
activity        <- sapply( trainYSet$V1, function(x) activityData$V2[x] )
activityDf      <- data.frame(matrix(unlist(activity), nrow=length(activity), byrow=T))
colnames(activityDf) <- "ActivityType"
trainingSet     <-cbind(trainingSet,activityDf)

testYSet        <- read.table("UCI HAR Dataset/test/y_test.txt", header=FALSE ,sep="")
activity        <- sapply( testYSet$V1, function(x) activityData$V2[x] )
activityDf      <- data.frame(matrix(unlist(activity), nrow=length(activity), byrow=T))
colnames(activityDf) <- "ActivityType"
testSet         <-cbind(testSet,activityDf)



# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
testSubjectSet  <- read.table("UCI HAR Dataset/test/subject_test.txt", header=FALSE ,sep="")
trainSubjectSet <- read.table("UCI HAR Dataset/train/subject_train.txt", header=FALSE ,sep="")
colnames(testSubjectSet) <- "Subject"
colnames(trainSubjectSet) <- "Subject"
testSet         <-cbind(testSet,testSubjectSet)
trainingSet     <-cbind(trainingSet,trainSubjectSet)

testSet     %>% group_by(ActivityType, Subject) %>%  summarise_if(is.numeric, mean, na.rm = TRUE)
trainingSet %>% group_by(ActivityType, Subject) %>%  summarise_if(is.numeric, mean, na.rm = TRUE)
write.table(x = testSet[100,], file = "tidyData.txt",  sep = ",", col.names=T, append=T)
write.table(x = trainingSet[100,], file = "tidyData.txt",  sep = ",", col.names=T, append=T)





