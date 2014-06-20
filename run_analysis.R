run_analysis <- function() {
  
  # Read input files
  training <- read.table('UCI HAR Dataset/train/X_train.txt')
  trainingSubj <- read.table('UCI HAR Dataset/train/subject_train.txt')
  trainingLbl <- read.table('UCI HAR Dataset/train/Y_train.txt')
  test <- read.table('UCI HAR Dataset/test/X_test.txt')
  testSubj <- read.table('UCI HAR Dataset/test/subject_test.txt')
  testLbl <- read.table('UCI HAR Dataset/test/Y_test.txt')
  featureTbl <- read.table('UCI HAR Dataset/features.txt')  
  activityTbl <- read.table('UCI HAR Dataset/activity_labels.txt')
  
  # Lookup descriptive activity label from numeric activity value for entire training set
  i <- 1
  trainingTbl <- matrix(nrow=length(trainingLbl[,1]), ncol=2)
  for(trainLbl in trainingLbl[,1]) {
    trainingTbl[i,1] = trainLbl
    trainingTbl[i,2] = as.character(activityTbl[activityTbl$V1==trainLbl,2])    
    i = i + 1
  }
  
  # Lookup descriptive activity label from numeric activity values for entire test set
  i <- 1
  testTbl <- matrix(nrow=length(testLbl[,1]), ncol=2)
  for(testLbl in testLbl[,1]) {
    testTbl[i,1] = testLbl
    testTbl[i,2] = as.character(activityTbl[activityTbl$V1==testLbl,2])    
    i = i + 1
  }  
  
  # Initialize column names vector
  colNames <- c('subject', 'activityLabel', as.character(featureTbl[,2]))
  
  # Add subject and activityLabel columns to training and test sets
  training <- cbind(trainingSubj, trainingTbl[,2], training)
  test <- cbind(testSubj, testTbl[,2], test)
  
  # Label data set with descriptive variable names  
  names(training) <- colNames
  names(test) <- colNames
  
  # Merge training and test set  
  merge <- rbind(test, training)

  # Write column numbers matching string *std* to vector stdlist and *mean* to meanlist
  i <- 1
  stdpattern <- "*std*"
  stdlist <- numeric()
  meanpattern <- "*-mean()*"
  meanlist <- numeric()
  for(feature in colNames) {    
    m <- regexpr(stdpattern, feature)
    if (length(regmatches(stdpattern, m)) > 0) {
      stdlist <- c(stdlist, i)
    }
    m <- regexpr(meanpattern, feature)
    if (length(regmatches(meanpattern, m)) > 0) {
      meanlist <- c(meanlist, i)
    }
    i <- i + 1
  }

  # Extract only mean and stddev for each measurement, adding first two columns (subject and activity)
  meanstdmeas <- merge[,c(1:2, meanlist, stdlist)]
  
# Create second, independent data set with the average of each variable for each activity and subject
# Aggregate the set by subject and activityLabel and then get mean
  groupset <- aggregate(meanstdmeas[,3:length(meanstdmeas)], 
                        by=list(meanstdmeas$subject, meanstdmeas$activityLabel), 
                        FUN="mean")
  names(groupset)[1:2] <- c('subject', 'activity')

  write.table(groupset, 'tidy.txt', row.names=FALSE, sep="\t")
}