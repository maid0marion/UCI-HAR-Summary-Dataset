## This R package reads the test and training data tables for the 


# load R packages

library(dplyr)
library(tidyr)

# read the test dataset files

testX <- read.table("UCI HAR Dataset/test/X_test.txt")
testY <- read.table("UCI HAR Dataset/test/y_test.txt")
testS <- read.table("UCI HAR Dataset/test/subject_test.txt")

# read the training dataset files

trainX <- read.table("UCI HAR Dataset/train/X_train.txt")
trainY <- read.table("UCI HAR Dataset/train/y_train.txt")
trainS <- read.table("UCI HAR Dataset/train/subject_train.txt")
flabels <- read.table("UCI HAR Dataset/features.txt")
alabels <- read.table("UCI HAR Dataset/activity_labels.txt")

# add new column to indicate data group

trainS <- mutate(trainS, Class = "train")
testS <- mutate(testS, Class = "test")

# merge the train and test datasets for each file

xAll <- bind_rows(trainX, testX)
yAll <- bind_rows(trainY, testY)
sAll <- bind_rows(trainS, testS)

# assign descriptive column names to merged datasets

colnames(xAll) <- flabels$V2
colnames(yAll) <- c("Activity")
colnames(sAll) <- c("Subject", "Class")

# combine all dataset columns so they are within a single dataset

all <- bind_cols(xAll, yAll, sAll)
all_t <- tbl_df(all)
rm(all)

# ensure column names are unique

valid_column_names <- make.names(names=names(all_t), unique=TRUE, allow_ = TRUE)
names(all_t) <- valid_column_names

# Extract only mean or standard deviation variable values

all_t <- all_t %>%
  select(Class, Subject, Activity, contains(".mean."), contains(".std.")) %>%
  mutate(Activity = factor(Activity, labels = alabels$V2)) %>%
  gather(Measurement, Meas_val, -(Class:Activity)) %>%
  separate(Measurement, c("Feature_Variable", "Stat_Type", "Axis"))

# Create a second dataset that contains the average of each variable for each 
# activity and each subject.

tidy_dataset <- all_t %>%
  group_by(Subject, Activity, Feature_Variable, Stat_Type, Axis) %>%
  summarize(Avg_val = mean(Meas_val)) %>%
  spread(Stat_Type, Avg_val) %>%
  rename("Mean" = mean, "Std" = std)

# Write output of summary dataset to a text file

write.table(tidy_dataset, "UCI-HAR-Summary-Dataset.txt", row.names=FALSE)
