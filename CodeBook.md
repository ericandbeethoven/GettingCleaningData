---
title: "CodeBook"
output: html_document
---

This is an R Markdown document CodeBook that describes the R script, source data, and output in detail.

<b>Source Data</b>
The source data and original dataset description can be found at

(http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones).

<b>Variables</b>
There were 68 variables in the 'tiny' dataset.

```{r}
str(tiny)
```
1 subject
2 activity
3 tBodyAcc.mean...X
4 tBodyAcc.mean...Y
5 tBodyAcc.mean...Z
6 tBodyAcc.std...X
7 tBodyAcc.std...Y
8 tBodyAcc.std...Z
9 tGravityAcc.mean...X
10 tGravityAcc.mean...Y
11 tGravityAcc.mean...Z
12 tGravityAcc.std...X
13 tGravityAcc.std...Y
14 tGravityAcc.std...Z
15 tBodyAccJerk.mean...X
16 tBodyAccJerk.mean...Y
17 tBodyAccJerk.mean...Z
18 tBodyAccJerk.std...X
19 tBodyAccJerk.std...Y
20 tBodyAccJerk.std...Z
21 tBodyGyro.mean...X
22 tBodyGyro.mean...Y
23 tBodyGyro.mean...Z
24 tBodyGyro.std...X
25 tBodyGyro.std...Y
26 tBodyGyro.std...Z
27 tBodyGyroJerk.mean...X
28 tBodyGyroJerk.mean...Y
29 tBodyGyroJerk.mean...Z
30 tBodyGyroJerk.std...X
31 tBodyGyroJerk.std...Y
32 tBodyGyroJerk.std...Z
33 tBodyAccMag.mean..
34 tBodyAccMag.std..
35 tGravityAccMag.mean..
36 tGravityAccMag.std..
37 tBodyAccJerkMag.mean..
38 tBodyAccJerkMag.std..
39 tBodyGyroMag.mean..
40 tBodyGyroMag.std..
41 tBodyGyroJerkMag.mean..
42 tBodyGyroJerkMag.std..
43 fBodyAcc.mean...X
44 fBodyAcc.mean...Y
45 fBodyAcc.mean...Z
46 fBodyAcc.std...X
47 fBodyAcc.std...Y
48 fBodyAcc.std...Z
49 fBodyAccJerk.mean...X
50 fBodyAccJerk.mean...Y
51 fBodyAccJerk.mean...Z
52 fBodyAccJerk.std...X
53 fBodyAccJerk.std...Y
54 fBodyAccJerk.std...Z
55 fBodyGyro.mean...X
56 fBodyGyro.mean...Y
57 fBodyGyro.mean...Z
58 fBodyGyro.std...X
59 fBodyGyro.std...Y
60 fBodyGyro.std...Z
61 fBodyAccMag.mean..
62 fBodyAccMag.std..
63 fBodyBodyAccJerkMag.mean..
64 fBodyBodyAccJerkMag.std..
65 fBodyBodyGyroMag.mean..
66 fBodyBodyGyroMag.std..
67 fBodyBodyGyroJerkMag.mean..
68 fBodyBodyGyroJerkMag.std..

<b>run_analysis.R</b>

A. The initial steps are to load libraries and features data.

```{r}
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
```

B. Data transformation as per Project Requirements


```{r}

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

```

C. Create tiny data set.

```{r}

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

```

D. Write output CSV file of tiny data set.

```{r}

write.table(tidy, file="tidymeans.txt", quote=FALSE)
```
