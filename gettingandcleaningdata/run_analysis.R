#Step 1: "Merges the training and the test sets to create one data set."

readData <- function(datasetname) {
	subject = read.table(paste0(datasetname,"/subject_",datasetname,".txt"), col.names=c("subject_id"))
	subject$ID <- as.numeric(rownames(subject))
	activity = read.table(paste0(datasetname,"/y_",datasetname,".txt"), col.names=c("activity_id"))
	activity$ID <- as.numeric(rownames(activity))
	X = read.table(paste0(datasetname,"/X_",datasetname,".txt"))
	X$ID <- as.numeric(rownames(X))

	data <- merge(subject, activity, all=TRUE)
	data <- merge(data, X, all=TRUE)
	data
}

trainData <- readData("train")
testData <- readData("test")
mergedData <- rbind(trainData, testData)

#Step 2: "Extracts only the measurements on the mean and standard deviation for each measurement."
features = read.table("features.txt", col.names=c("feature_id", "feature_label"),) 
selected_features <- features[grepl("mean\\(\\)", features$feature_label) | grepl("std\\(\\)", features$feature_label), ]
meanStdData <- mergedData[, c(c(1, 2, 3), selected_features$feature_id + 3) ]

#Step 3: "Uses descriptive activity names to name the activities in the data set."
activity_labels = read.table("activity_labels.txt", col.names=c("activity_id", "activity_label"),) #
labledData = merge(meanStdData, activity_labels)

#Step 4: "Appropriately labels the data set with descriptive activity names." 
selected_features$feature_label = gsub("\\(\\)", "", selected_features$feature_label)
selected_features$feature_label = gsub("-", ".", selected_features$feature_label)
for (i in 1:length(selected_features$feature_label)) {
    colnames(labledData)[i + 3] <- selected_features$feature_label[i]
}

#Step 5: "Creates a second, independent tidy data set with the average of each variable for each activity and each subject."
drops <- c("ID","activity_label")
labledData <- labledData[,!(names(labledData) %in% drops)]
aggdata <-aggregate(labledData, by=list(subject = labledData$subject_id, activity = labledData$activity_id), FUN=mean, na.rm=TRUE)
drops <- c("subject","activity")
aggdata <- aggdata[,!(names(aggdata) %in% drops)]
aggdata = merge(activity_labels, aggdata)
write.csv(file="tidy.txt", x=aggdata)
