# Project Assignment
# 1.	Merges the training and the test sets to create one data set.
# 2.	Extracts only the measurements on the mean and standard deviation for each measurement.
# 3.	Uses descriptive activity names to name the activities in the data set
# 4.	Appropriately labels the data set with descriptive variable names.
# 5.	From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.


install.packages("data.table")
install.packages("plyr")

library(data.table)
library(plyr)

# You should set the working directory in the folder that contains "UCI HAR Dataset" folder

Activity labels: the name of the six activities performed for every person
# Load the second column from the file activity_labels.txt 
activity_labels<- read.table("./UCI HAR Dataset/activity_labels.txt")[,2]

# Features: All the variables (477) measured by the smartphone
# Load the second column from the file features.txt
features<- read.table("./UCI HAR Dataset/features.txt")[,2]


# X_test: training test (Data measured in all the variables for all the subjects)
# y_test: training labels of the 2947 observations (activities performed in all the variables measured)
# subject_test: subject measured in every observation (row)
# Load X_test, y_test & subject_test
X_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt")
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")
X_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt")
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")

# Load the variable names in the training test dataset
names(X_test) = features
names(X_train) = features

# Create a new column with the correspondent activity label
y_test[,2] = activity_labels[y_test[,1]]
y_train[,2] = activity_labels[y_train[,1]]

# Bind data
test_data <- cbind(subject_test, y_test, X_test)
train_data <- cbind(subject_train, y_train, X_train)
data = rbind(test_data, train_data)

# Rename the variables V1="Subject" V2="Activity_ID", V3="Activity_Label"
colnames(data)[1:3] = c("Subject", "Activity_ID", "Activity_Label")

# Create a file with all the data binded (Merged Dataset)
write.table(data, file = "./data.txt")


# 2.	Extracts only the measurements on the mean and standard deviation for each measurement.

# Extract only the measurements on the mean and standard deviation for each measurement.

extract_data= data[,grepl("mean|std", colnames(data))]
extract_data= extract_data[,!grepl("meanFreq", colnames(extract_data))]

data1= data[,1:3]
colnames(data1)[1:3] = c("Subject", "Activity_ID", "Activity_Label")

dataS= cbind(data1,extract_data)

# Calculate the average of each variable for each activity and each subject
Data2<-aggregate(. ~Subject + Activity_Label, dataS, mean)
Data2<-Data2[order(Data2$Subject,Data2$Activity_ID),]

# Create a file with the tidy data
write.table(Data2, file = "tidydata.txt",row.name=FALSE)
