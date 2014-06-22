rm(list=ls())
source("helperFunctions.R")

#START: 1. Merges the training and the test sets to create one data set.
#load test files and bind to same dataframe
subject_test <- read.csv("UCI HAR Dataset/test/subject_test.txt", header=F, col.names="subjectID")
activity_test <- read.csv("UCI HAR Dataset/test/y_test.txt", header=F, col.names="activity")
features_test <- read.csv("UCI HAR Dataset/test/X_test.txt", header=F, sep="")
test_df <- cbind(subject_test, activity_test, features_test)

#load train files and bind to same dataframe
subject_train <- read.csv("UCI HAR Dataset/train/subject_train.txt", header=F, col.names="subjectID")
activity_train <- read.csv("UCI HAR Dataset/train/y_train.txt", header=F, col.names="activity")
features_train <- read.csv("UCI HAR Dataset/train/X_train.txt", header=F, sep="")
train_df <- cbind(subject_train, activity_train, features_train)

#concatenate the two data frames
df <- rbind(test_df, train_df)
#END 1

#START: 2. Extracts only the measurements on the mean and standard deviation for each measurement.
#get measurements names and clean them up
#then assign them to the name of the df numerical variables
features_names <- read.csv("UCI HAR Dataset/features.txt", header=F, sep="")
features_names$V2 <- gsub("-"," ",features_names$V2)
features_names$V2 <- gsub("\\(\\)","",features_names$V2)
features_names$V2 <- gsub("\\("," ",features_names$V2)
features_names$V2 <- gsub("\\)"," ",features_names$V2)
features_names$V2 <- gsub(","," ",features_names$V2)
names(df) <- c(names(df)[1:2], features_names$V2)

#subset only the measurements regarding mean and std plus the subjectID and activity
subset_feature_names <- c(1:2, sort(c(grep("[S|s]td", names(df)), grep("[M|m]ean", names(df)))))
df_tidy <- df[,subset_feature_names]
#END 2

#START: 3. Uses descriptive activity names to name the activities id in the data set
#transform activity id in readable variable values according to the guidelines of Lesson 1 week 4:
# -should be descriptive

#Then replace activity id with the activity value through merge and subset
activity_labels <- read.csv("UCI HAR Dataset/activity_labels.txt", header=F, col.names=c("id", "activity"), sep="")
activity_labels$activity <- gsub("_"," ",activity_labels$activity)
activity_labels$activity <- sapply(activity_labels$activity, capitalizeValue)
df_tidy <- merge(activity_labels, df_tidy, by.x="id", by.y="activity", all=F)
df_tidy <- df_tidy[,!(names(df_tidy) %in% "id")]
#END 3

#START: 4. Appropriately labels the data set with descriptive variable names.
#transform feature names in readable variable values according to the guidelines of Lesson 1 week 4:
#all lower case
#descriptive
#not duplicated
#not have underscores or dots or white spaces
names(df_tidy) <- sapply(names(df_tidy), capitalizeVariableName)
names(df_tidy) <- gsub("Acc","Acceleration",names(df_tidy))
names(df_tidy) <- gsub("Gyro","AngularVelocity",names(df_tidy))
names(df_tidy) <- sapply(names(df_tidy), appendTimeFrequency)
#write.table(df_tidy, "first_tidy_dataset.txt", row.names=F)
#END 4

#START: 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.
require(data.table)
dt <- data.table(df_tidy)
setkey(dt, subjectID, activity)
dt_tidy <- dt[,lapply(.SD, mean),by=list(subjectID, activity)]
write.table(dt_tidy, "second_tidy_dataset.txt", row.names=F)
#END 5