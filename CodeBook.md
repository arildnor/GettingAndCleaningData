#"Getting and Cleaning Data" Course, Johns Hopkins
## Introduction
One of the most exciting areas in all of data science right now is wearable computing - see for example this article.

http://www.insideactivitytracking.com/data-science-activity-tracking-and-the-battle-for-the-worlds-top-sports-brand/

Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. 
The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. 
A full description is available at the site where the data was obtained: 

http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 

Here are the data for the project: 

https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

An R script, run_analysis.R is created to perform the following on the dataset above.

1.	Merges the training and the test sets to create one data set.
2.	Extracts only the measurements on the mean and standard deviation for each measurement. 
3.	Uses descriptive activity names to name the activities in the data set
4.	Appropriately labels the data set with descriptive variable names. 
5.	From the data set in step 4, creates a second, independent tidy data set with the average of 
    each variable for each activity and each subject.

Information from the full description homepage (link shown above) are: 

*Source:*

*Jorge L. Reyes-Ortiz(1,2), Davide Anguita(1), Alessandro Ghio(1), Luca Oneto(1) and Xavier Parra(2)*
*1 - Smartlab - Non-Linear Complex Systems Laboratory*
*DITEN - Università degli Studi di Genova, Genoa (I-16145), Italy.*
*2 - CETpD - Technical Research Centre for Dependency Care and Autonomous Living*
*Universitat Politècnica de Catalunya (BarcelonaTech). Vilanova i la Geltrú (08800), Spain*
*activityrecognition '@' smartlab.ws*

*Data Set Information:*

*The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years.* 
*Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) *
*wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, *
*we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. *
*The experiments have been video-recorded to label the data manually. *
*The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers *
*was selected for generating the training data and 30% the test data.*

*The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters *
*and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window).* 
*The sensor acceleration signal, which has gravitational and body motion components, *
*was separated using a Butterworth low-pass filter into body acceleration and gravity. *
*The gravitational force is assumed to have only low frequency components, *
*therefore a filter with 0.3 Hz cutoff frequency was used. From each window, *
*a vector of features was obtained by calculating variables from the time and frequency domain.*

*Attribute Information:*

*For each record in the dataset it is provided:*
*- Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration.*
*- Triaxial Angular velocity from the gyroscope.*
*- A 561-feature vector with time and frequency domain variables.*
*- Its activity label.*
*- An identifier of the subject who carried out the experiment.*

*Relevant Papers:

*Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on *
*Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop of *
*Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012*
*Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra, Jorge L. Reyes-Ortiz. Energy Efficient *
*Smartphone-Based Activity Recognition using Fixed-Point Arithmetic. Journal of Universal Computer *
*Science. Special Issue in Ambient Assisted Living: Home Care. Volume 19, Issue 9. May 2013*
*Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity *
*Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. 4th International *
*Workshop of Ambient Assited Living, IWAAL 2012, Vitoria-Gasteiz, Spain, December 3-5, 2012. Proceedings. *
*Lecture Notes in Computer Science 2012, pp 216-223.*
*Jorge Luis Reyes-Ortiz, Alessandro Ghio, Xavier Parra-Llanas, Davide Anguita, Joan Cabestany, *
*Andreu Català. Human Activity and Motion Disorder Recognition: Towards Smarter Interactive Cognitive *
*Environments. 21th European Symposium on Artificial Neural Networks, Computational Intelligence and *
*Machine Learning, ESANN 2013. Bruges, Belgium 24-26 April 2013.*

*Citation Request:
*Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. A Public Domain Dataset for *
*Human Activity Recognition Using Smartphones. 21th European Symposium on Artificial Neural Networks, *
*Computational Intelligence and Machine Learning, ESANN 2013. Bruges, Belgium 24-26 April 2013. *

## Data preparations
The zip file (link above) were downloaded and put in the same directory as the run_analysis.R script.
The data were unzipped and generated a folder structure as outlined.

"UCI HAR Dataset" ------test-----Inertial Signals

                  ------train----Inertial Signals

The text below is from the README.txt file in the downloaded data.

For each record it is provided:
======================================

- Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration.
- Triaxial Angular velocity from the gyroscope. 
- A 561-feature vector with time and frequency domain variables. 
- Its activity label. 
- An identifier of the subject who carried out the experiment.

The dataset includes the following files:
=========================================

- 'README.txt'
- 'features_info.txt': Shows information about the variables used on the feature vector.
- 'features.txt': List of all features.
- 'activity_labels.txt': Links the class labels with their activity name.
- 'train/X_train.txt': Training set.
- 'train/y_train.txt': Training labels.
- 'test/X_test.txt': Test set.
- 'test/y_test.txt': Test labels.

The following files are available for the train and test data. Their descriptions are equivalent. 

- 'train/subject_train.txt': Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 
- 'train/Inertial Signals/total_acc_x_train.txt': The acceleration signal from the smartphone accelerometer X axis in standard gravity units 'g'. 
   Every row shows a 128 element vector. The same description applies for the 'total_acc_x_train.txt' and 'total_acc_z_train.txt' files for the Y and Z axis. 
- 'train/Inertial Signals/body_acc_x_train.txt': The body acceleration signal obtained by subtracting the gravity from the total acceleration. 
- 'train/Inertial Signals/body_gyro_x_train.txt': The angular velocity vector measured by the gyroscope for each window sample. The units are radians/second. 

Notes: 
======
- Features are normalized and bounded within [-1,1].
- Each feature vector is a row on the text file.
For more information about this dataset contact: activityrecognition@smartlab.ws

#Variable Names and Format
The variable names for the features are documented in the features_info.txt file in the "UCI HAR Dataset" folder.
There are a total of 561 features. 
There are 30 subjects participating. They are given numbers from 1 to 30.
There are 6 activity codes:
1 WALKING
2 WALKING_UPSTAIRS
3 WALKING_DOWNSTAIRS
4 SITTING
5 STANDING
6 LAYING

##The transformations and comutations done by the run_analysis.R script.
Reading the datafiles from the directories.

Datasets read from main directory:
"features.txt"          This dataset contains a list of variables in the datasets.
"activity_labels.txt"   This is the activity labels connected to the y_datasets
Datasets read from the train and test subdirectories:
"subject_test.txt"      The subject number (1-30) linked to the test data
"subject_train.txt"     The subject number (1-30) linked to the train data
"y_test.txt"            Activity code linked to the test dataset
"y_train.txt"           Activity code linked to the train dataset
"X_test.txt"            Feature vector test data
"X_train.txt"           Feature vector train data

#Processing.
1. Merging subject_, y_ and X_ train and test datasets using rbind.
2. Columns in subject and activity are named
3. The names are set in the X_ merged set using the features set read.
4. We only want to keep mean() and std() features for further work.
I extract the relevant columns using the grepl command. This reduces the columns from 561 to 66.
5. All columns have got labels. One could discuss if editing names would be useful.
I decided to minimize editing and stick to the originals.
6. I have a combined dataset with subject, activity and kept features. Combined with cbind.
7. Then I used the dplyr package to group on activity and subject and calculate the mean of theese groups.
This gave a reduction from 10299 to 180 rows.
8. This dataset was then exported.

The exported dataset has the original names for vectors. There is an extra column with activity as text. 
I decided to keep the original numeric activity coding also.
