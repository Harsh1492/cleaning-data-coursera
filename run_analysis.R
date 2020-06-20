# Steps Assignment
# Author: Geanderson Esteves

# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names.
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

if (!file.exists("dataset.zip")) {
    download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", "dataset.zip")
}

unzip("dataset.zip")
setwd("UCI HAR Dataset")

train_subject <- read.table("train/subject_train.txt")
test_subject <- read.table("test/subject_test.txt")
subject <- rbind(train_subject, test_subject)

activity_labels <- read.table("activity_labels.txt")
train_activity <- read.table("train/y_train.txt")
test_activity <- read.table("test/y_test.txt")
activity <- rbind(train_activity, test_activity)
activity[, 1] <- as.factor(gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", gsub("_", " ", tolower(activity_labels[match(activity[, 1], activity_labels[, 1]), 2])), perl = TRUE))

train_measurement <- read.table("train/X_train.txt")
test_measurement <- read.table("test/X_test.txt")
measurement <- rbind(train_measurement, test_measurement)

features <- read.table("features.txt", colClass = "character")

combined <- cbind(subject, activity, measurement)
names(combined) <- c("Subject", "Activity", features[, 2])

combined <- combined[, (names(combined) == "Subject" | names(combined) == "Activity" | grepl("mean\\(\\)", names(combined)) | grepl("std\\(\\)", names(combined)))]

names(combined) <- gsub("^t", "Time", names(combined))
names(combined) <- gsub("^f", "Frequency", names(combined))
names(combined) <- gsub("-", " ", names(combined))
names(combined) <- gsub("mean\\(\\)", "Mean", names(combined))
names(combined) <- gsub("std\\(\\)", "Standard Deviation", names(combined))
names(combined) <- gsub("Body", " Body", names(combined))
names(combined) <- gsub("Gyro", " Gyroscope", names(combined))
names(combined) <- gsub("Acc", " Accelerometer", names(combined))
names(combined) <- gsub("Mag", " Magnitude", names(combined))
names(combined) <- gsub("Jerk", " Jerk", names(combined))
names(combined) <- gsub("Gravity", " Gravity", names(combined))
names(combined) <- gsub("Body Body", "Body", names(combined))

tidy <- aggregate(combined[, -(1:2)], by = list(Subject = combined$Subject, Activity = combined$Activity), FUN = mean)

setwd("..")

#Write the tidy.txt
write.table(tidy, "tidy.txt", row.names = FALSE)