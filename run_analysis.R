
library(reshape2)

download.data = function() {
    "Checks for data directory and creates one if it doesn't exist"
    if (!file.exists("data")) {
        message("Creating data directory")
        dir.create("data")
    }
    if (!file.exists("data/UCI HAR Dataset")) {
        # download the data
        fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
        zipfile="data/UCI_HAR_data.zip"
        message("Downloading data")
        download.file(fileURL, destfile=zipfile, method="curl")
        unzip(zipfile, exdir="data")
    }
}

if (!file.exists('data')){
 download.data()
}


# loading data
# Note that Linux directory escaping is used 

activityLabels = read.table('data/UCI\ HAR\ Dataset/activity_labels.txt',header=FALSE);
features     = read.table('data/UCI\ HAR\ Dataset/features.txt',header=FALSE);

subjectTraining = read.table('data/UCI\ HAR\ Dataset/train/subject_train.txt',header=FALSE);
xTraining    = read.table('data/UCI\ HAR\ Dataset/train/X_train.txt',header=FALSE);
yTraining    = read.table('data/UCI\ HAR\ Dataset/train/y_train.txt',header=FALSE);

subjectTesting = read.table('data/UCI\ HAR\ Dataset/test/subject_test.txt',header=FALSE);
xTesting       = read.table('data/UCI\ HAR\ Dataset/test/X_test.txt',header=FALSE);
yTesting       = read.table('data/UCI\ HAR\ Dataset/test/y_test.txt',header=FALSE);

# assigning names to tables 

names(subjectTraining) <- "subjectID"
names(subjectTesting) <- "subjectID"
names(xTraining) <- features$V2
names(xTesting) <- features$V2
names(yTraining) <- "activityId"
names(yTesting) <- "activityId"
names(activityLabels)  <-  c('activityId','activityType');


# combined training data set
trainingCombined = cbind(yTraining,subjectTraining,xTraining);

# combined testing data set 
testingCombined = cbind(yTesting,subjectTesting,xTesting);

# combine training and testing
finalData = rbind(trainingCombined,testingCombined);


#get columns containing "mean()" or "std()"
meanAndStdCols <- grepl("mean\\(\\)", names(finalData)) |
    grepl("std\\(\\)", names(finalData))

# keep the subjectID and activity columns
meanAndStdCols[1:2] <- TRUE

# Subset finalData table based on the logicalVector to keep only desired columns
finalData <- finalData[, meanAndStdCols]


# 3. Use descriptive activity names to name the activities in the data set

# Merge the finalData set with the acitivityType table to include descriptive activity names
finalData = merge(finalData, activityLabels,by='activityId',all.x=TRUE); 

melted <- melt(finalData, id=c("subjectID","activityType"))
tidyData <- dcast(melted, subjectID+activityType ~ variable, mean)

write.table(tidyData, './tidyData.csv',row.names=FALSE);
