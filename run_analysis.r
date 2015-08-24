# GettingCleaningData

###############################################
#
# Libraries
#
###############################################
library(reshape2)

###############################################
#
# Load features data.
#
###############################################

setwd("UCI HAR Dataset")
features <- read.table("features.txt", header=FALSE)
features.colnames <- features$V2
xtrain <- read.table("train/X_train.txt", header=FALSE, col.names=features.colnames)
xtest <- read.table("test/X_test.txt", header=FALSE, col.names=features.colnames)
features <- rbind(xtrain, xtest)

###############################################
#
# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
#
###############################################
features <- features[, grep(".*\\.(mean|std)\\.\\..*", names(features), value=T)]

subjects.colnames <- c("subject")
subjecttrain <- read.table("train/subject_train.txt", header=FALSE, col.names=subjects.colnames)
subjecttest <- read.table("test/subject_test.txt", header=FALSE, col.names=subjects.colnames)

subjects <- rbind(subjecttrain, subjecttest)

activities.colnames <- c("activity")
activitytrain <- read.table("train/y_train.txt", header=FALSE, col.names=activities.colnames)
activitytest <- read.table("test/y_test.txt", header=FALSE, col.names=activities.colnames)

activities <- rbind(activitytrain, activitytest)


###############################################
#
# 2. Uses descriptive activity names to name the activities in the data set
#
###############################################
activity.labels <- read.table("activity_labels.txt", header=FALSE, col.names=c("activity", "activityName"))
activities <- merge(activities, activity.labels, by="activity", sort=F)
subject.activities <- cbind(subjects, data.frame(activity = activities$activityName))

###############################################
#
# 1. Merges the training and the test sets to create one data set.
#
###############################################
df <- cbind(features, subject.activities)

###############################################
#
# 4. Appropriately labels the data set with descriptive variable names. 
#
###############################################
colnames(df) <- tolower(str_replace_all(colnames(df), "([A-Z]{1})", ".\\1"))
colnames(df) <- str_replace_all(colnames(df), "[\\.]+", ".")
colnames(df) <- str_replace_all(colnames(df), "[\\.]+$", "") # extra dot at the end of the string

tidy <- df[0,]
tidy[1,] <- rep(NA, 68)

melted <- melt(df, id=c("subject","activity"))
 
# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

###############################################
#
# 5. From the data set in step 4, creates a second, independent tidy data set with 
#    the average of each variable for each activity and each subject.
#
###############################################
for(subj in unique(melted$subject)){
  means <- c()
  for(acti in unique(melted[melted$subject == subj,]$activity)){
    for(vari in unique(melted[melted$subject == subj,]$variable)){
      m <- mean(melted[melted$subject == subj & melted$activity == acti & melted$variable == vari,]$value)
      means <- append(means, as.numeric(m))
    }
    means <- append(means, subj)
    means <- append(means, acti)
    tidy <- rbind(tidy, means)
  }
}

# remove the first N/A row
tidy <- na.omit(tidy)

# re-orderding of columns: first subject, then activity, then the values
tidy <- tidy[,c(67,68, 1:66)]

write.table(tidy, file="tidymeans.txt", quote=FALSE)




