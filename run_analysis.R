## Script: run.analysis.R
## Course: Getting and Cleaning Data
## Author: Ben Collins

## This script has been created in order to fulfill the requirements of the
## Getting and Cleaning Data course project, which are as follows:
##
## 1. Merges the training and the test sets to create one data set.
##
## 2. Extracts only the measurements on the mean and standard deviation for each
## measurement.
##
## 3. Uses descriptive activity names to name the activities in the data set.
##
## 4. Appropriately labels the data set with descriptive variable names.
##
## 5. From the data set in step 4, creates a second, independent tidy data set
## with the average of each variable for each activity and each subject.

## NOTE: This script uses a mixture of base R and tidyverse functions.

library(tidyverse)

##____________________________________________________________________________
## Start of script
##____________________________________________________________________________

## Download the data to the working directory.

data_url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

data_file <- "UCI HAR Dataset.zip"

download.file(data_url, data_file, method = "curl", quiet = TRUE)

unzip(data_file, setTimes = TRUE)

## Data has now been downloaded and released from the zip folder.

## start to load and merge the data sets.
## First load the names of features.

features <- read.table(file = "UCI HAR Dataset/features.txt",
                      col.names = c("column_number", "feature_name"),
                      colClasses = c("integer", "character"))

## Use the mutate function to add columns which indicate if the features are
## needed (for this project, as stated in the second requirement, we need the
## mean and standard deviations measurements) and to make syntactically valid
## names for the features.

features <- features %>%
        ## Add indicators for the mean and standard deviation.
        mutate(is_needed = str_detect(feature_name, "-(mean|std)\\(\\)")) %>%
        ## Add syntactically valid names for the features.
        mutate(feature_column = make.names(feature_name, unique = TRUE)) %>%
        ## Swap the . for _ in the new feature name.
        mutate(feature_column = str_replace_all(feature_column, "\\.+", "_"))

## Next load the activity labels.

activity_labels <- read.table(file = "UCI HAR Dataset/activity_labels.txt",
                              col.names = c("id", "activity"),
                              colClasses = c("integer", "factor"))

## Change activity labels to lower case.

activity_labels$activity <- tolower(activity_labels2$activity)

## Now we can create a function which will read in the test and training data
## files from their relevant directories and return a data frame.

read_from_directory <- function(directory) {
        
directory_path <- file.path("UCI HAR Dataset", directory)
## Set up a warning if the incorrect directory has been inserted in to
## the function.
if(!dir.exists(directory_path)){
        stop("Directory not found")
}

## Read in the data set based on the selected directory and rename the columns
## to match the features, which follows the fourth requirement.

data_set <- read.table(file = file.path(directory_path, str_c("X_", directory, ".txt")),
                       col.names = features$feature_column)

## Choose the columns that are required for this project based on our features
## data frame.
        
data_set <- data_set %>% select(one_of(features %>%
                                                filter(is_needed) %>%
                                                pull(feature_column)))

## Read in the activity.
        
activity <- read.table(file = file.path(directory_path, str_c("y_", directory, ".txt")),
                       col.names = "id",
                       colClasses = "numeric")
        
## Add the activity labels.

activity <- left_join(activity, activity_labels, by = "id")

## Add the description of the activities to the start of the data set, as stated
## in the third requirement.
        
data_set <- cbind(activity = activity[,2], data_set)

## Read in the subject.
        
subject <- read.table(file = file.path(directory_path, str_c("subject_", directory, ".txt")),
                      col.names = "subject",
                      colClasses = "integer")

## Add the subject to the start of the data set.
        
data_set <- cbind(subject, data_set)

## Get the function to output the data set with an additional column to
## identify if the data is from the test or the training
        
data_set %>%
        mutate(data_set = directory) %>%
        select(data_set, everything())
}

## The function to read the data files has now been completed.
## We can now load both the training and test data and then merge them
## together.

training_data <- read_from_directory("train")

test_data <- read_from_directory("test")

data <- rbind(training_data, test_data)

## We have no successfully loaded and merged the data.

## To fulfill the fifth requirment we need to create another independent,
## tidy data set. We can use the pivot_longer function to reduce the number of
## columns in the data and to summarise the average of each variable for each
## activity and each subject.

tidy_data <- pivot_longer(data,
                           cols = -c(data_set, subject, activity),
                           names_to = c("sensor", "measure", "dimension"),
                           names_sep = "_")

## Change the characters to factors to aid rearrangement.

tidy_data <- mutate_if(tidy_data, is.character, as_factor)

## Rearrange the data to make it easier to read.

tidy_data <- arrange(tidy_data, data_set, subject, activity, sensor, measure, dimension)

## Save the data in the working directory.

write.table(tidy_data, file = "tidy_data.txt", row.names = FALSE)

## Tidy the data up further for submission, including defining the average
## column.

submit_data <- tidy_data %>%
        group_by(subject, activity, sensor, measure, dimension) %>%
        summarise(mean = mean(value))

## Save the submission data in the working directory.

write.table(submit_data, file = "submit_data.txt", row.names = FALSE)

##____________________________________________________________________________
## End of script
##____________________________________________________________________________