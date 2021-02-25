#run_analysis.R week 4 project
library(dplyr) 
rm(list = ls()) 

#Load the feature/activity labels
Fnames <- readLines("features.txt")

#Get the means and stds only - I am ignoring all mean frequencies
meanstds <- grep("mean\\(|std\\(",Fnames)
indvec <- -rep(1,561) 
indvec[meanstds] = 1 

#Making tiddy names
Mynames <- tolower(Fnames[meanstds])
secondElement <- function(x){x[2]}
Mynames <- sapply(strsplit(Mynames, " "), secondElement)
#Get rid of the parenthesis
Mynames <- sub("\\(\\)", "", Mynames)
Mynames <- gsub("`","", Mynames)

#Read test and train data
testfile = "./test/X_test.txt"
Numlines = length(readLines(testfile))
testdata <- read.fwf(testfile, widths = rep(16,561)*indvec, sep = "", n = Numlines)
trainfile = "./train/X_train.txt"
Numlines = length(readLines(trainfile))
traindata <- read.fwf(trainfile, widths = rep(16,561)*indvec, sep = "", n = Numlines) 

#Load the index labels for each activity and the real labels
testlabelfile <-"./test/y_test.txt"
testlabels <- data.frame(labels = readLines(testlabelfile))
trainlabelfile <-"./train/y_train.txt"
trainlabels <- data.frame(labels =readLines(trainlabelfile))
mergedlabels <- rbind(testlabels, trainlabels)
activities <- readLines("activity_labels.txt")
activities <- data.frame(id = factor(1:6), activity = sapply(strsplit(activities, " "), secondElement)) 
mergedActivities <- left_join(mergedlabels,activities, c("labels" ="id"))

#Merge test and train data in one matrix
MergedData <- rbind(testdata,traindata)
rm(list = c("testdata","traindata"))
#Ad the column names for the dataset
colnames(MergedData) <- Mynames

#The Participant indices per test and train data
testPart <- as.numeric(readLines("./test/subject_test.txt"))
trainPart <- as.numeric(readLines("./train/subject_train.txt"))
SubID <- data.frame(participant = c(testPart, trainPart))

#Merged all three frames
database <- cbind(SubID, mergedActivities, MergedData)
database$labels <- NULL

aux <- split(database, as.factor(database$participant))
#aux2 <- aggregate(onepart[,-(1:2)], list(onepart$activity), mean)
meanbyparticipant <-do.call(rbind,lapply(aux, function(chunk) aggregate(chunk[,-2], list(chunk$activity), mean)))
colnames(meanbyparticipant)[1] <- "activities"

#Write both datasets in two csv files
write.csv(database,"mergeddata.csv", row.names = FALSE)
write.csv(meanbyparticipant, "meanbyparticipant.csv",row.names=FALSE)
write.table(meanbyparticipant, "meanbyparticipant.txt",row.names=FALSE)

