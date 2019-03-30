library(dplyr)

# read data
X_train <- read.table("train/X_train.txt")
X_test <- read.table("test/X_test.txt")
Y_train <- read.table("train/Y_train.txt")
Y_test <- read.table("test/Y_test.txt")
subject_train <- read.table("train/subject_train.txt")
subject_test <- read.table("test/subject_test.txt")
features <- read.table("features.txt")
activity_labels <- read.table("activity_labels.txt")

# Merges the training and the test sets to create one data set.
X_total <- rbind(X_train, X_test)
Y_total <- rbind(Y_train, Y_test)
subject_total <- rbind(subject_train, subject_test)

# Extracts only the measurements on the mean and standard deviation for each measurement
Meanstdfeatures <- features[grep("mean|std",features[,2]),]
X_total <- X_total[,Meanstdfeatures[,1]]

# Uses descriptive activity names to name the activities in the data set
colnames(Y_total) <- "labels"
Y_total$activity <- factor(Y_total$labels, labels = as.character(activity_labels[,2]))
activity <- Y_total$activity

# Appropriately labels the data set with descriptive variable names
colnames(X_total) <- features[Meanstdfeatures[,1],2]
colnames(subject_total) <- "subject"

# Rename variable names to make it clear
names(X_total) <- gsub("Acc", "Accelerometer", names(X_total), ignore.case = TRUE)
names(X_total) <- gsub("Freq()", "Frequency", names(X_total), ignore.case = TRUE)
names(X_total) <- gsub("BodyBody", "Body", names(X_total), ignore.case = TRUE)
names(X_total) <- gsub("Gyro", "Gyroscope", names(X_total), ignore.case = TRUE)
names(X_total) <- gsub("Mag", "Magnitude", names(X_total), ignore.case = TRUE)
names(X_total) <- gsub("^t", "Time", names(X_total), ignore.case = TRUE)
names(X_total) <- gsub("^f", "Frequency", names(X_total), ignore.case = TRUE)

# From the data set in step 4, creates a second, independent tidy data set with 
# the average of each variable for each activity and each subject.
all <- cbind(subject_total, Y_total, X_total)
all$activity <- factor(all$activity, levels = activity_labels[,1], labels = activity_labels[,2])
all$subject  <- is.factor(all$subject)
all_mean <- all %>% group_by(activity, subject) %>% summarize_all(funs(mean)) 

# Write table
write.table(all_mean, file = "Tidy_data.txt", row.names = FALSE)
