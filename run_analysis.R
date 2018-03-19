library(data.table)
library(dplyr)
library(tidyr)

# Read the data
train_subjects<- read.table("train/subject_train.txt")
test_subjects<- read.table("test/subject_test.txt")
trainx_data <- read.table("train/X_train.txt")
trainy_data <- read.table("train/Y_train.txt")
testx_data <- read.table("test/X_test.txt")
testy_data <- read.table("test/Y_test.txt")
features <- read.table("features.txt")
setnames(features, names(features), c("featurenum", "featurename"))
activity_labels <- read.table("activity_labels.txt")
setnames(activity_labels, names(activity_labels), c("activity_num", "activity_name"))


# 1. merges the data
data_subject <- rbind(train_subjects, test_subjects)
setnames(data_subject, "V1", "subject")
mergey_data <- rbind(trainy_data,testy_data)
setnames(mergey_data, "V1", "activity_num")


mergex_data <- rbind(trainx_data,testx_data)
colnames(mergex_data) = features$featurename

data_subj_act<- cbind(data_subject, mergey_data)
mergex_data <- cbind(data_subj_act,mergex_data)



# 2. Extracting mean and standard deviation measurements
mean_std <- grep("mean\\(\\)|std\\(\\)",features$featurename, value=TRUE)
mean_std <- union(c("subject","activity_num"), mean_std)
datatable<- subset(mergex_data,select = mean_std) 

# 3. Descriptive activity names
datatable <- merge(activity_labels, datatable , by="activity_num", all.x=TRUE)
datatable$activity_name <- as.character(datatable$activity_name)


# 4. Descriptive variable names

names(datatable)<-gsub("std()", "SD", names(datatable))
names(datatable)<-gsub("mean()", "MEAN", names(datatable))
names(datatable)<-gsub("^t", "time", names(datatable))
names(datatable)<-gsub("^f", "frequency", names(datatable))
names(datatable)<-gsub("Acc", "Accelerometer", names(datatable))
names(datatable)<-gsub("Gyro", "Gyroscope", names(datatable))
names(datatable)<-gsub("Mag", "Magnitude", names(datatable))
names(datatable)<-gsub("BodyBody", "Body", names(datatable))

# 5. Average of each variable
data_aggr<- aggregate(. ~ subject - activity_name, data = datatable, mean)
datatable<- tbl_df(arrange(data_aggr,subject,activity_name))
write.table(datatable, "tidydata.txt", row.name=FALSE)




