# Getting-and-Cleaning-Data


### Introduction

The purpose of this project is to demonstrate my ability to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis. 

This submission contains: 

1. a tidy data set as described below, 
2. a link to a Github repository with the script for performing the analysis, and 
3. a code book that describes the variables, the data, and any transformations or work that were performed to clean up the data called CodeBook.md. Also included is this README.md in the repo that explains how the script works

### Included Files

**run_analysis.R:** The R file that reads UCI HAR data and outputs a tidy data set. The script does the following:

1. Merges the training and the test sets to create one data set with names taken from *features.txt*
2. Extracts only the measurements on the mean and standard deviation for each measurement via regex
3. Uses descriptive activity names to name the activities in the data set taken from *activity_labels.txt*
4. Gathers the sensor measurements and separates by *doman, sensor, summary, and direction* of the signal value.
5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

**Tidy_Step5.txt:** The tidy data set with the average of each variable for each activity and each subject referenced in step 5 above.

**CodeBook.MD:** A codebook that describes the variables and summaries calculated

**README.MD:** This file that includes a description of repo contents
