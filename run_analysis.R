
# You should you need to have the UCI HAR Dataset extracted for the analysis to run. 


# loading training data 
activityLabels = read.table('./activity_labels.txt',header=FALSE);
subjectTraining = read.table('./train/subject_train.txt',header=FALSE);
features     = read.table('./features.txt',header=FALSE);
xTraining    = read.table('./train/X_train.txt',header=FALSE);
yTraining    = read.table('./train/y_train.txt',header=FALSE);

# Assigning names, training data 
colnames(activityLabels)  = c('activityId','activityType');
colnames(subjectTraining)  = "subjectId";
colnames(xTraining)        = features[,2]; 
colnames(yTraining)        = "activityId";

# combined training data set
trainingCombined = cbind(yTraining,subjectTraining,xTraining);

# loading test data 
subjectTesting = read.table('./test/subject_test.txt',header=FALSE);
xTesting       = read.table('./test/X_test.txt',header=FALSE);
yTesting       = read.table('./test/y_test.txt',header=FALSE);

# assigning names, training data 
colnames(subjectTesting) = "subjectId";
colnames(xTesting)       = features[,2]; 
colnames(yTesting)       = "activityId";

# combined testing data set 
testingCombined = cbind(yTesting,subjectTesting,xTesting);

# combine training and testing
combinedFinal = rbind(trainingCombined,testingCombined);


colNames  = colnames(combinedFinal); 
filtering = (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames));
combinedFinal = combinedFinal[filtering==TRUE];
combinedFinal = merge(combinedFinal,activityLabels,by='activityId',all.x=TRUE);
colNames  = colnames(combinedFinal); 


# Renaming variables
for (i in 1:length(colNames)) 
{
  colNames[i] = gsub("\\()","",colNames[i])
  colNames[i] = gsub("-mean","Mean",colNames[i])
  colNames[i] = gsub("^(f)","Freq",colNames[i])
  colNames[i] = gsub("-std$","StdDev",colNames[i])
  colNames[i] = gsub("^(t)","Time",colNames[i])
  colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
  colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
};

# combinedFinal with better variable names 
colnames(combinedFinal) = colNames;

# Creating Tidy final 

WithoutActivity  = combinedFinal[,names(combinedFinal) != 'activityType'];
tidyFinal    = aggregate(WithoutActivity[,names(WithoutActivity) != c('activityId','subjectId')],by=list(activityId=WithoutActivity$activityId,subjectId = WithoutActivity$subjectId),mean);
tidyFinal    = merge(tidyFinal,activityType,by='activityId',all.x=TRUE);
write.table(tidyFinal, './tidyFinal.txt',row.names=TRUE,sep='\t');
