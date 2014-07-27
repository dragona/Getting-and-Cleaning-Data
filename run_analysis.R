# The purpose of this project is to demonstrate your ability to collect, 
# work with, and clean a data set. The goal is to prepare tidy data that 
# can be used for later analysis. You will be graded by your peers on a 
# series of yes/no questions related to the project. You will be required
# to submit: 1) a tidy data set as described below, 2) a link to a Github 
# repository with your script for performing the analysis, and 3) a code book 
# that describes the variables, the data, and any transformations or work that 
# you performed to clean up the data called CodeBook.md. You should also include
# a README.md in the repo with your scripts. This repo explains how all of the 
# scripts work and how they are connected. 
# 
# One of the most exciting areas in all of data science right now is wearable 
# computing - see for example this article . Companies like Fitbit, Nike, 
# and Jawbone Up are racing to develop the most advanced algorithms to attract 
# new users. The data linked to from the course website represent data collected 
# from the accelerometers from the Samsung Galaxy S smartphone. A full description
# is available at the site where the data was obtained:
#     
# http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
# 
# Here are the data for the project:
#     
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
# 
# You should create one R script called run_analysis.R that does the following. 
# 
# 1- Merges the training and the test sets to create one data set.
# 2- Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3- Uses descriptive activity names to name the activities in the data set
# 4- Appropriately labels the data set with descriptive variable names. 
# 5- Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
# 
# Good luck!


        ###########################################################################
        

# -Download https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
# -Set the working directory to point at "UCI HAR Dataset", which is the default name of the extracted
#   folder inside the previously downloaded zip file. 


        ###########################################################################
        # 1- Merge the training and the test sets to create one data set.
        ###########################################################################
#Read all the training  files and save them in memory
        #read X_train.txt and save it in trainingData
trainingData <- read.table("./train/X_train.txt")
        #read y_train.txt and save it in traningLabel
trainingLabel <- read.table("./train/y_train.txt")
        #readsubject_tran.txt and save it in traningSubject 
trainingSubject <- read.table("./train/subject_train.txt")

#Read all the test  files and save them in memory
        #read X_test.txt and save it in testData
testData <- read.table("./test/X_test.txt")
        #read y_test.txt and save it in testLabel
testLabel <- read.table("./test/y_test.txt")
        #read subject_test.txt and save it in testSubject
testSubject <- read.table("./test/subject_test.txt")
        
#combine (merge) trainings and the tests to create data frames (data set)
        #merge traningData and testData and save in mergeData
mergedData <- rbind(trainingData, testData)
        #merge traningLabel and testLabel and save in mergeData
mergedLabel <- rbind(trainingLabel, testLabel)
        #merge traningSubject and testSubjet and save in megeSubject
mergedSubject <- rbind(trainingSubject, testSubject)
    

        
        ###########################################################################
        # 2- Extract only the measurements on the mean and standard deviation for each measurement. 
        ###########################################################################

        #read features.txt and save in features        
features <- read.table("features.txt")
        #Using pattern matching and replacement
        #extract  measurements on the mean and standard deviation
meanStdIndices <- grep("mean\\(\\)|std\\(\\)", features[, 2])
mergedData <- mergedData[, meanStdIndices]
        # remove "()"
names(mergedData) <- gsub("\\(\\)", "", features[meanStdIndices, 2]) 
        # use "M" instead of "m"
names(mergedData) <- gsub("mean", "Mean", names(mergedData)) 
        # use "S" instead of "s"
names(mergedData) <- gsub("std", "Std", names(mergedData)) 
        # remove "-" in column names
names(mergedData) <- gsub("-", "", names(mergedData)) 
        
        ###########################################################################
        # 3- Use descriptive activity names to name the activities in the data set
        ###########################################################################
        #read activity_labels.txt and save in activity
activity <- read.table("activity_labels.txt")
        
        #Convert to lowercase
activity[, 2]<- tolower(activity[, 2]);
        #Using gsub, to perform replacement/matching of the "_", in fact, here we remove them
activity[, 2] <- gsub("_", "", activity[, 2])
        #Use substr to Extract/replace substrings in a character vector.
        # we have walkingdownstairs and walkingupstairs, and we will replace then to be 
        # walkingDownstairs and walkingUpstairs to be more readable.
substr(activity[2, 2], 8, 8) <- toupper(substr(activity[2, 2], 8, 8))
substr(activity[3, 2], 8, 8) <- toupper(substr(activity[3, 2], 8, 8))
mergedLabel[, 1] <- activity[mergedLabel[, 1], 2]
        #set the name of the label to be "activity"
names(mergedLabel) <- "activity"
        
        ###########################################################################
        # 4- Appropriately label the data set with descriptive variable names. 
        ###########################################################################
        #set the name of the label for "mergedSubject" to be "subject"
names(mergedSubject) <- "subject"
merged_clean_data <- cbind(mergedSubject, mergedLabel, mergedData)
   
        ###########################################################################
        # 5- Create a second, independent tidy data set with the average of each variable for each activity and each subject. 
        ###########################################################################
subjectLen <- length(table(mergedSubject)) # 30
activityLen <- dim(activity)[1] # 6
columnLen <- dim(merged_clean_data)[2]
result <- matrix(NA, nrow=subjectLen*activityLen, ncol=columnLen)
result <- as.data.frame(result)
colnames(result) <- colnames(merged_clean_data)
row <- 1
for(i in 1:subjectLen) {
    for(j in 1:activityLen) {
        result[row, 1] <- sort(unique(mergedSubject)[, 1])[i]
        result[row, 2] <- activity[j, 2]
        bool1 <- i == merged_clean_data$subject
        bool2 <- activity[j, 2] == merged_clean_data$activity
        result[row, 3:columnLen] <- colMeans(merged_clean_data[bool1&bool2, 3:columnLen])
        row <- row + 1
    }
}
#save the file
write.table(result, "tidy_data.txt") 
  