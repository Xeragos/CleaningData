
#Loading libraries
library(data.table)
library(dplyr)

#Reading the supporting metadata assuming the dataset is downloaded & extracted
fNames <- read.table("UCI HAR Dataset/features.txt")
aLabels <- read.table("UCI HAR Dataset/activity_labels.txt", header = FALSE)

#Formatting the train data
#Spitting into 3 -> sub, act and feat

#Reading the training data
subTrain <- read.table("UCI HAR Dataset/train/subject_train.txt", header = FALSE)
actTrain <- read.table("UCI HAR Dataset/train/y_train.txt", header = FALSE)
featTrain <- read.table("UCI HAR Dataset/train/X_train.txt", header = FALSE)

#Reading the test data
subTest <- read.table("UCI HAR Dataset/test/subject_test.txt", header = FALSE)
actTest <- read.table("UCI HAR Dataset/test/y_test.txt", header = FALSE)
featTest <- read.table("UCI HAR Dataset/test/X_test.txt", header = FALSE)


#Merging training and testing datasets to make one dataset
sub <- rbind(subTrain, subTest)
act <- rbind(actTrain, actTest)
feat <- rbind(featTrain, featTest)

#Name the column names from the features file in variable fNames
colnames(feat) <- t(fNames[2])

#Add act and subj as a column to features
colnames(act) <- "Activity"
colnames(sub) <- "Subject"
completeData <- cbind(feat,act,sub)


#Extracting only the measurements over mean and standard deviation for each measurement
columnsWithMeanSTD <- grep(".*Mean.*|.*Std.*", names(completeData), ignore.case=TRUE)

#Adding act and sub columns
requiredColumns <- c(columnsWithMeanSTD, 562, 563)

#Looking at the dimension and number of variables in completeData
dim(completeData)
extractedData <- completeData[,requiredColumns]

#Look at the dimension and number of variables in extractedData
dim(extractedData)


#Using descriptive activity names to name the activities in the data set

extractedData$Activity <- as.character(extractedData$Activity)
for (i in 1:6){
  extractedData$Activity[extractedData$Activity == i] <- as.character(aLabels[i,2])
}

#Set the activity variable in the data as a factor
extractedData$Activity <- as.factor(extractedData$Activity)


#Allocating proper labels the data set with descriptive variable names. 
#To check the variable names
names(extractedData)

#Acc can be replaced with Accelerometer
#Gyro can be replaced with Gyroscope
#BodyBody can be replaced with Body
#Mag can be replaced with Magnitude
#Character 'f' can be replaced with Frequency
#Character 't' can be replaced with Time

names(extractedData)<-gsub("Acc", "Accelerometer", names(extractedData))
names(extractedData)<-gsub("Gyro", "Gyroscope", names(extractedData))
names(extractedData)<-gsub("BodyBody", "Body", names(extractedData))
names(extractedData)<-gsub("Mag", "Magnitude", names(extractedData))
names(extractedData)<-gsub("^t", "Time", names(extractedData))
names(extractedData)<-gsub("^f", "Frequency", names(extractedData))
names(extractedData)<-gsub("tBody", "TimeBody", names(extractedData))
names(extractedData)<-gsub("-mean()", "Mean", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-std()", "STD", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-freq()", "Frequency", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("angle", "Angle", names(extractedData))
names(extractedData)<-gsub("gravity", "Gravity", names(extractedData))


#Checking the new variable names
names(extractedData)


#From the data set created, created a second, independent tidy data set with the
#average of each variable for each activity and each subject.
#Set the subject variable in the data as a factor
extractedData$Subject <- as.factor(extractedData$Subject)
extractedData <- data.table(extractedData)

#Create tidyData as a set with average for each activity and subject
tidyData <- aggregate(. ~Subject + Activity, extractedData, mean)

#Order tidayData according to subject and activity
tidyData <- tidyData[order(tidyData$Subject,tidyData$Activity),]

#Write tidyData into a text file
write.table(tidyData, file = "Tidy.txt", row.names = FALSE)