rm(list = ls())

require(dplyr)
require(tidyr)

## reads the feature data to provide column labels
features <- read.table("./data/UCI HAR Dataset/features.txt")
features <- select(features, V2)

## adds subject and label headings as they will be joined to the datasets
features <- c("subject", "label", as.character(features$V2)) 

## reads activity lables
activities <- read.table("./data/UCI HAR Dataset/activity_labels.txt")
colnames(activities) <- c("label", "activity")

## reads the test and training data and bind it together
xtest <- read.table("./data/UCI HAR Dataset/test/X_test.txt")
ytest <- read.table("./data/UCI HAR Dataset/test/y_test.txt")
xsubjecttest <- read.table("./data/UCI HAR Dataset/test/subject_test.txt")
xtest <- cbind(xsubjecttest, ytest, xtest)
colnames(xtest) <- features
xtest <- left_join(xtest,activities, by = "label")

xtrain <- read.table("./data/UCI HAR Dataset/train/X_train.txt")
ytrain <- read.table("./data/UCI HAR Dataset/train/y_train.txt")
xsubjecttrain <- read.table("./data/UCI HAR Dataset/train/subject_train.txt")
xtrain <- cbind(xsubjecttrain, ytrain, xtrain)
colnames(xtrain) <- features
xtrain <- left_join(xtrain,activities, by = "label")

## joins the test and training sets into a single large dataframe
df <- rbind(xtest, xtrain)

## selects only the subject & activity columns and all columns that contain mean 
##  or std
tidy1 <- df[,grep("subject|activity|mean\\(\\)|std\\(\\)",names(df))]
names(tidy1) <- gsub("\\(|\\)", "", names(tidy1))

## gathers measurements into a tidy dataset and separates out measurement types
tidy1 <- gather(tidy1, measurement, value, 2:67)
tidy1 <- separate(tidy1, measurement, into = c("sensor", "summary",
                                               "direction"))


## creates a second tidy data set to display averages for each activity and each 
## subject
tidy2 <- tidy1 %>% group_by(activity, subject, sensor, summary, direction) %>% 
  summarize(mean = mean(value))

## output datasets
write.csv(tidy1, "Step4.csv")
write.csv(tidy2, "Step5 (Average Values).csv")