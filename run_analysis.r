#The Samsung data is located here: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
#The data should be unzipped and located in your working directory.

#Import the activity labels and features text files
activityLabels <- read.table("activity_labels.txt")
activityLabels[,2] <- as.character(activityLabels[,2])

features <- read.table("features.txt")
features[,2] <- as.character(features[,2])

#Create a vector that contains the column index for the features referring to mean or standard deviation
#Identify mean and standard deviation fields regardless of capitalization
featuresMeanStd <- grep(".*[Mm][Ee][Aa][Nn].*|.*[Ss][Tt][Dd].*", features[,2])

#Import the training and test data sets while subsetting to import only the columns of interest
xTrain <- read.table("train/X_train.txt")[featuresMeanStd] #Subsets with vector
yTrain <- read.table("train/Y_train.txt")
subjectTrain <- read.table("train/subject_train.txt")

xTest <- read.table("test/X_test.txt")[featuresMeanStd] #Subsets with vector
yTest <- read.table("test/Y_test.txt")
subjectTest <- read.table("test/subject_test.txt")

#Merge the training and test sets to create one data set
train <- cbind(subjectTrain, yTrain, xTrain)
test <- cbind(subjectTest, yTest, xTest)
data <- rbind(train, test)

#Create descriptive activity names to name the activities in the data set
featuresMeanStd.names <- features[featuresMeanStd, 2] 
featuresMeanStd.names <- gsub('-std', 'standardeviation', featuresMeanStd.names)
featuresMeanStd.names <- gsub("^f", "frequencydomain", featuresMeanStd.names)
featuresMeanStd.names <- gsub("^t", "timedomain", featuresMeanStd.names)
featuresMeanStd.names <- gsub("Acc", "acceleration", featuresMeanStd.names)
featuresMeanStd.names <- gsub("Mag", "magnitude", featuresMeanStd.names)
featuresMeanStd.names <- gsub("BodyBody", "body", featuresMeanStd.names)
featuresMeanStd.names <- gsub('[-()]', '', featuresMeanStd.names)
#Recommended as best practice to make names same case therefore force to lower case
featuresMeanStd.names <- tolower(featuresMeanStd.names)

#Label the data set with descriptive variable names
colnames(data) <- c("subject", "activity", featuresMeanStd.names)

data$activity <- factor(data$activity, levels = activityLabels[,1], labels = activityLabels[,2])
data$subject <- as.factor(data$subject)

#Create an independent tidy data set with the average of each variable for each activity and each subject
#Mean calculation excludes the subject and activity column as these columns are not applicable
tidy <- aggregate(data[,3:ncol(data)], by=list(activity = data$activity, subject=data$subject), mean)

#Write out tidy data set to working directory
write.table(tidy, "tidy.txt", row.names = FALSE)