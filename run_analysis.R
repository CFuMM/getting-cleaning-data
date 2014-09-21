
#setwd("\\file04\redirect\ths\my documents\Coursea\getting and cleaing data")
library(dplyr)

#load the data in both train and test data, labels and subjects
trainData <- read.table("./getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/train/X_train.txt")
trainLabel <- read.table("./getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/train/Y_train.txt")
trainSub <- read.table("./getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/train/subject_train.txt")
testData <- read.table("./getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/test/X_test.txt")
testLabel <- read.table("./getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/test/Y_test.txt")
testSub <- read.table("./getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/test/subject_test.txt")

#check that the data sets are joinable in dimentions
dim(trainData)
dim(testData)
dim(trainLabel)
dim(testLabel)
dim(trainSub)
dim(testSub)

#Joingin the data after sum check
DataJoin <- rbind(trainData, testData)
LabelJoin <- rbind(trainLabel, testLabel)
SubJoin <- rbind(trainSub, testSub)

#new check of sums
dim(DataJoin)
dim(LabelJoin)
dim(SubJoin)

#getting the mean and str info from file
features <- read.table("./getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/features.txt")

#extracting the filter where str and mean are in features collom 2
meanstrlist <- grep("mean\\(\\)|std\\(\\)",features[, 2])

#first filter the data on the new list
DataJoin <- DataJoin[, meanstrlist]
dim(DataJoin)

#cleaning the dataset
names(DataJoin) <- gsub("\\(\\)", "", features[meanstrlist, 2]) # koyrur alt vekk
names(DataJoin) <- gsub("-", "", names(DataJoin))
names(DataJoin) <- gsub("std", "Std", names(DataJoin)) 
names(DataJoin) <- gsub("mean", "Mean", names(DataJoin))

#getting the aktivity

Aktivity <- read.table("./getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/activity_labels.txt")

#to lower case
Aktivity[, 2] <- tolower(Aktivity[, 2])
#remove "_"
Aktivity[, 2] <- gsub("_", "", Aktivity[, 2])

AktivityLabel <- Aktivity[LabelJoin[, 1], 2]
LabelJoin[, 1] <- AktivityLabel

#Calling the names() before adding them togher
names(LabelJoin) <- "activity"
names(SubJoin) <- "subject"

#getting cleen data and writing the new file
CleenData <- cbind(SubJoin, LabelJoin, DataJoin)
write.table(CleenData, "data_set_1_merged.txt", row.name=FALSE)


#And the second set of data 
SubLength <- length(table(SubJoin)) # 30
ActLength <- dim(Aktivity)[1] # 6
ColLength <- dim(CleenData)[2]
output <- matrix(NA, nrow=SubLength*ActLength, ncol=ColLength) 
output <- as.data.frame(result)
colnames(output) <- colnames(CleenData)
Nr <- 1
for(i in 1:SubLength) {
    for(j in 1:ActLength) {
        output[Nr, 1] <- sort(unique(SubJoin)[, 1])[i]
        output[Nr, 2] <- Aktivity[j, 2]
        bool1 <- i == CleenData$subject
        bool2 <- Aktivity[j, 2] == CleenData$activity
        output[Nr, 3:ColLength] <- colMeans(CleenData[bool1&bool2, 3:ColLength])
        Nr <- Nr + 1
    }
}
head(output)
write.table(output, "data_set_2_cleen.txt")

