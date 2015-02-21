################################################################################
##
## Getting and Cleaning Data
## Programming assignment
##
## 1. Merges the training and the test sets to create one data set.
## 2. Extracts only the measurements on the mean and standard deviation for 
##    each measurement. 
## 3. Uses descriptive activity names to name the activities in the data set
## 4. Appropriately labels the data set with descriptive variable names. 
## 5. From the data set in step 4, creates a second, independent tidy data set 
##    with the average of each variable for each activity and each subject.
##
## The dataset includes the following files:
##    
##- 'README.txt'
##- 'features_info.txt': Shows information about the variables used on 
##   the feature vector.
##- 'features.txt': List of all features.
##- 'activity_labels.txt': Links the class labels with their activity name.
##
################################################################################
#
# Starting R in the main directory, reading the location and setting
# up to read the files. Data is downloaded to the "UCI HAR Dataset" folder.
#
currentDirectory <- getwd()
datasetHomeFolder <- "UCI HAR Dataset"
#
# This dataset contains the information on the variables used.
# It is a text file and are not used in the subsequent workflow.
#
feautures_info_name <- "features_info.txt"
features_info_file <- paste(currentDirectory,"/",datasetHomeFolder,"/",feautures_info_name,sep="")
features_info <- read.table(features_info_file, sep="\t", header=FALSE)
features_info
#
# This dataset contains a list of variables in the datasets.
#
features_name <- "features.txt"
features_file <- paste(currentDirectory,"/",datasetHomeFolder,"/",features_name,sep="")
features <- read.table(features_file, sep="\t", header=FALSE)
features
#
# This is the activity labels connected to the y_datasets
#
activity_labels_name <- "activity_labels.txt"
activity_labels_file <- paste(currentDirectory,"/",datasetHomeFolder,"/",activity_labels_name,sep="")
activity_labels <- read.table(activity_labels_file, sep="\t", header=FALSE)
activity_labels
#
# Read the datafiles from subdirectories. 
# We have 2 directories containing test and train datasets
#
testsetHomeFolder <- "test"
trainsetHomeFolder <- "train"
#
testDirectory <- paste(currentDirectory,"/",datasetHomeFolder,"/",testsetHomeFolder,sep="")
trainDirectory <- paste(currentDirectory,"/",datasetHomeFolder,"/",trainsetHomeFolder,sep="")
print(testDirectory)
print(trainDirectory)
#
# The following files are available for the train and test data.
# Their descriptions are equivalent. 
# 'train/subject_train.txt' and 'test/subject_train.txt': 
#   Each row identifies the subject who performed the activity for 
#   each window sample. Its range is from 1 to 30.
#
thisFiletest <- "subject_test.txt"
thisFiletrain <- "subject_train.txt"
testFileName <- paste(testDirectory,"/",thisFiletest,sep="")
trainFileName <- paste(trainDirectory,"/",thisFiletrain,sep="")
print(testFileName)
print(trainFileName)
subject_test <- read.table(testFileName, header=FALSE)
subject_train <- read.table(trainFileName, header=FALSE)
str(subject_test)
str(subject_train)
#
# The following files are available for the train and test data.
# Their descriptions are equivalent. 
#'train/y_train.txt' and 'test/y_train.txt': 
#   Each row contains the activity linked to the rows in the X_ tables.
#   Rows are linked to subject and y_test. There are 6 activities described
#   in the activity_labels file
#   
thisFiletest <- "y_test.txt"
thisFiletrain <- "y_train.txt"
testFileName <- paste(testDirectory,"/",thisFiletest,sep="")
trainFileName <- paste(trainDirectory,"/",thisFiletrain,sep="")
print(testFileName)
print(trainFileName)
y_test <- read.table(testFileName, header=FALSE)
y_train <-  read.table(trainFileName, header=FALSE)
str(y_test)
str(y_train)
#
# The following files are available for the train and test data.
# Their descriptions are equivalent. 
#'train/X_train.txt' and 'test/X_train.txt': 
#   Each row contains the summary measurements for 561 variables.
#   Rows are linked to subject and y_test
#   
thisFiletest <- "X_test.txt"
thisFiletrain <- "X_train.txt"
testFileName <- paste(testDirectory,"/",thisFiletest,sep="")
trainFileName <- paste(trainDirectory,"/",thisFiletrain,sep="")
print(testFileName)
print(trainFileName)
X_test <- read.table(testFileName, header=FALSE)
X_train <-  read.table(trainFileName, header=FALSE)
str(X_test)
str(X_train)
dim(X_test)
dim(X_train)
##
## Raw signals are in subfolders. Are not used for the assignment.
##
##- 'train/Inertial Signals/total_acc_x_train.txt': 
##     The acceleration signal from the smartphone accelerometer X axis 
##     in standard gravity units 'g'. Every row shows a 128 element vector. 
##     The same description applies for the 'total_acc_x_train.txt' 
##     and 'total_acc_z_train.txt' files for the Y and Z axis. 
##- 'train/Inertial Signals/body_acc_x_train.txt': 
##     The body acceleration signal obtained by subtracting the gravity 
##     from the total acceleration. 
##- 'train/Inertial Signals/body_gyro_x_train.txt': 
##   The angular velocity vector measured by the gyroscope 
##   for each window sample. The units are radians/second. 
#
# All data are read into R. Ready to merge
# Check dimensions. 
# All train sets should have same number of rows. The same is for test sets.
#
dim(subject_train)
dim(subject_test)
dim(y_train)
dim(y_test)
dim(X_train)
dim(X_test)
################################################################################
#
# 1. Merges the training and the test sets to create one data set.
#
# Merge the datasets, combining rows since both datasets have same format
# 
#
# Subject datasets
#
merged_subject <- rbind(subject_train,subject_test)
dim(merged_subject)
#
# Set column name
#
names(merged_subject)[1] <- paste("subject")
#
# y_ datasets
#
merged_Y <- rbind(y_train,y_test)
dim(merged_Y)
#
# set column name
#
names(merged_Y)[1] <- paste("activity")
#
# for the X_ dataset we want to extract certain columns
#
merged_X <- rbind(X_train,X_test)
dim(merged_X)
#
# Set the varable names in the X_ set using 
# the column names from the features set.
#
names(merged_X) <- features$V1
#
# Can check all column names
#
str(merged_subject)
str(merged_Y)
str(merged_X)

################################################################################
#
# 2. Extracts only the measurements on the mean and standard deviation for 
#    each measurement. 
# Check for mean and std in the header information in the merged X_ set.
# Should Keep true and remove false from final dataset
# This will reduce the number of variables from 561 to 66
# 
#
substring="-mean\\(\\)|-std\\(\\)"
final_merged_x <- merged_X[grepl(substring,names(merged_X))]
str(final_merged_x)
dim(final_merged_x)
head(final_merged_x)

################################################################################
#
# 3. Uses descriptive activity names to name the activities in the data set
#
str(merged_Y)
str(activity_labels)
activity_labels$rownumber <- row(activity_labels, as.factor = FALSE)
str(activity_labels)
print(activity_labels)
#
# Put labels into the merged_y dataset. 
# I keep the activity numbers along with text description.
#
merged_Y$txtactivity <- 
          activity_labels$V1[match(merged_Y$activity,activity_labels$rownumber)]
str(merged_Y)
head(merged_Y,n=30)
#
# Add the 3 datasets into combined dataset. 
# This now contains subject, activity and the selected X_ columns
#
combined_set <- cbind(merged_subject,merged_Y,final_merged_x)
str(combined_set)
head(combined_set)

################################################################################
#
# 4. Appropriately labels the data set with descriptive variable names. 
#
# Looking at the output from str() I decide to keep the labels as is now.
# An option would be to remove the number that are the first part of the 
# 66 mean and std columns. But keeping them shows what position they came from 
# in the original dataset.
#
str(combined_set)

################################################################################
#
## 5. From the data set in step 4, creates a second, independent tidy data set 
##    with the average of each variable for each activity and each subject.
#    
# Using dplyr package for last part of project
#
require(dplyr)
require(tidyr)
#
# Set the tidy set using tbl_df to speed up
#
tidy_set <- tbl_df(combined_set)
str(tidy_set)
dim(tidy_set)
#
# Use pipe functionality to do the summarising
# I put activity first. It is then easy to see
# the results for each subject for a given activity, comparing the subjects
# in a way. Who is best
#
grouped_tidy <- tidy_set %>%
    group_by(activity,subject) %>%
    summarise_each(funs(mean)) %>%
    print
#
# The text explanation for activity is now a number due to the sumarise
# function. Need to put back a text string as was done before.
# More research may have shown me another solution, but this works
#
grouped_tidy$txtactivity <- 
    activity_labels$V1[match(grouped_tidy$activity,activity_labels$rownumber)]
str(grouped_tidy)
#
# Check dimensions and head in the combined and aggregated set to see if it is OK
# There are 30 subjects and 6 activities. The maxomum rows in the summarized set
# are therefore 180
#
#table(merged_subject)
#table(activity_labels)
#
dim(combined_set)
dim(tidy_set)
dim(grouped_tidy)
head(combined_set,n=50)
head(grouped_tidy,n=50)
#
# Write the tidy dataset
#
write.table(grouped_tidy, file="tidy.txt", row.names=FALSE)
#
# Done
#
################################################################################
