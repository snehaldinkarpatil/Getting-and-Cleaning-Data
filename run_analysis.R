## setting the working directory
setwd("~/UCI HAR Dataset")

## Reading the datasets
feature <- read.table("~/UCI HAR Dataset/features.txt", header = FALSE, sep = "", dec = ".")
activity <- read.table("~/UCI HAR Dataset/activity_labels.txt", header = FALSE, sep = "", dec = ".")
x_test <- read.table("~/UCI HAR Dataset/test/X_test.txt", header = FALSE, sep = "", dec = ".")
y_test <- read.table("~/UCI HAR Dataset/test/y_test.txt", header = FALSE, sep = "", dec = ".")
subject_test <- read.table("~/UCI HAR Dataset/test/subject_test.txt", header = FALSE, sep = "", dec = ".")
x_train <- read.table("~/UCI HAR Dataset/train/X_train.txt", header = FALSE, sep = "", dec = ".")
y_train <- read.table("~/UCI HAR Dataset/train/y_train.txt", header = FALSE, sep = "", dec = ".")
subject_train <- read.table("~/UCI HAR Dataset/train/subject_train.txt", header = FALSE, sep = "", dec = ".")

## Defining the column names of the datasets x_test and x_train
colnames(x_test) <- feature[ ,2]
colnames(x_train) <- feature[ ,2]

## Labelling the activities
library(dplyr)
y_test <- y_test %>% left_join(activity)
y_train <- y_train %>% left_join(activity)

## Defining the column names of y_test and y_train
names(y_test)[1] <- "activity"
names(y_test)[2] <- "activity_label"
names(y_train)[1] <- "activity"
names(y_train)[2] <- "activity_label"

## Defining the column names of subject_test and subject_train
names(subject_test)[1] <- "subject"
names(subject_train)[1] <- "subject"

## Merging the training and the test sets to create one data set.
test <- cbind(x_test, y_test, subject_test)
train <- cbind(x_train, y_train, subject_train)
my_data <- rbind(test, train)

## Extracting only the measurements on the mean and standard deviation for each measurement.
names(my_data)
my_data <- my_data[, !duplicated(colnames(my_data))]
my_data <- my_data %>% select("subject", "activity_label", contains("mean"), contains("std"))
names(my_data)

## Uses descriptive activity names to name the activities in the data set.
my_data$activity_label

## Appropriately labels the data set with descriptive variable names.
names(my_data)[2] <- "activity"
names(my_data)<-gsub("Acc", "Accelerometer", names(my_data))
names(my_data)<-gsub("Gyro", "Gyroscope", names(my_data))
names(my_data)<-gsub("BodyBody", "Body", names(my_data))
names(my_data)<-gsub("Mag", "Magnitude", names(my_data))
names(my_data)<-gsub("^t", "Time", names(my_data))
names(my_data)<-gsub("^f", "Frequency", names(my_data))
names(my_data)<-gsub("tBody", "TimeBody", names(my_data))
names(my_data)<-gsub("-mean()", "Mean", names(my_data), ignore.case = TRUE)
names(my_data)<-gsub("-std()", "STD", names(my_data), ignore.case = TRUE)
names(my_data)<-gsub("-freq()", "Frequency", names(my_data), ignore.case = TRUE)
names(my_data)<-gsub("angle", "Angle", names(my_data))
names(my_data)<-gsub("gravity", "Gravity", names(my_data))

## From the data set in step 4, creates a second, 
## independent tidy data set with the average of each variable for each activity and each subject.

Means  <- my_data %>% 
  group_by(activity, subject) %>%
  summarize_all("mean")

write.table(Means, "Means.txt", row.name=FALSE)







