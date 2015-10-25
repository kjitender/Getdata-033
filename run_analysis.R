library(plyr)
library(data.table)
library(dplyr)


## Getting the data
## Assuming Samsung data is not there in folder.

if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./data/Dataset.zip",method="curl")
unzip(zipfile="./data/Dataset.zip",exdir="./data")

file_path <- file.path("./data" , "UCI HAR Dataset")
files<-list.files(path_rf, recursive=TRUE)

setwd(file_path)


## Reading the data

y_train <-read.table(file.path(file_path, "train", "Y_train.txt"),header = FALSE)
y_test <- read.table(file.path(file_path, "test" , "Y_test.txt" ),header = FALSE)

x_test  <- read.table(file.path(file_path, "test" , "X_test.txt" ),header = FALSE)
x_train <- read.table(file.path(file_path, "train", "X_train.txt"),header = FALSE)


subject_train <- read.table(file.path(file_path, "train", "subject_train.txt"),header = FALSE)
subject_test  <- read.table(file.path(file_path, "test" , "subject_test.txt"),header = FALSE)


features <- read.table(file.path(file_path, "features.txt"),head=FALSE)
activity_labels <- read.table(file.path(file_path, "activity_labels.txt"),head=FALSE)


colnames(activity_labels)<- c("V1","Activity")

## Merging same datasets

subject_data <- rbind(subject_train, subject_test) 

y_data<- rbind(y_train, y_test)  
x_data<- rbind(x_train, x_test)  

## Renaming columns in datasets. This will help in connecting all datasets

names(subject_data)<-c("subject")
names(y_data)<- c("activity")
names(x_data)<- features$V2


## Assignment 1: Merges the training and the test sets to create one data set.

All_data <- cbind(x_data,cbind(subject_data,y_data)) 



## Assignment 2:Extracts only the measurements on the mean and standard deviation for each measurement. 

Mean_cols <- grep("mean()", names(All_data), value = FALSE, fixed = TRUE)
Std_cols <- grep("std()", names(All_data), value = FALSE, fixed = TRUE)

Select_data<-All_data[c(Mean_cols,Std_cols)]

Select_data<-cbind(Select_data,All_data$subject,All_data$activity)


colnames(Select_data)[which(names(Select_data) == "All_data$subject")] <- "subject"

colnames(Select_data)[which(names(Select_data) == "All_data$activity")] <- "activity"



## Assignment 3: Uses descriptive activity names to name the activities in the data set

Select_data$activity <- as.character(Select_data$activity)

Select_data$activity[Select_data$activity == 1] <- "Walking"
Select_data$activity[Select_data$activity == 2] <- "Walking Upstairs"
Select_data$activity[Select_data$activity == 3] <- "Walking Downstairs"
Select_data$activity[Select_data$activity == 4] <- "Sitting"
Select_data$activity[Select_data$activity == 5] <- "Standing"
Select_data$activity[Select_data$activity == 6] <- "Laying"

Select_data$activity <- as.factor(Select_data$activity)


## Assignment 4:Appropriately labels the data set with descriptive variable names. 


names(Select_data)<-gsub("Acc", "Accelerometer", names(Select_data))
names(Select_data)<-gsub("BodyBody", "Body", names(Select_data))
names(Select_data)<-gsub("^f", "frequency", names(Select_data))
names(Select_data)<-gsub("Gyro", "Gyroscope", names(Select_data))
names(Select_data)<-gsub("Mag", "Magnitude", names(Select_data))
names(Select_data)<-gsub("^t", "time", names(Select_data))


## Assignment 5: From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.


Tidy_data<-aggregate(. ~subject + activity, Select_data, mean)
Tidy_data<-Tidy_data[order(Tidy_data$subject,Tidy_data$activity),]
write.table(Tidy_data, file = "tidy_data.txt",row.name=FALSE)