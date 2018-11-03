#install needed packages
#install.packages("zip")
#install.packages("reshape2")

## call the packages
require(zip)
require(data.table)
require(reshape2)



#Downloading the  Zip file
        fileName<-"UCI HAR Dataset"
        if(!file.exists(paste0(fileName,".zip"))){
                url<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
                print(paste0("Downloading file from the web..."))
                download.file(url, paste0(fileName,".zip"), method = "curl")
        }else{
                print(paste("The file", fileName, "exists in the working directory."))
                print(paste("Fetching", fileName, " from the working directory..."))
        }
        
        
        oldwd<-getwd()
        
        ##Unizp file working directory
        unzip(paste0(fileName,".zip"))
        
        ##Get into the unzipped file
        nwd<-paste0(oldwd,"/",fileName)
        setwd(nwd)
        
        ##Reading activity and features files
        print("Reading activity_labels and features files...")
        activity_labels <- fread("activity_labels.txt", col.names = c("labels", "activity"))
        features <- fread("features.txt", col.names = c("id", "featureNames"))
        
        #Set variables to extract only the measurements on the mean and standard deviation 
        #for each measurement. 
        wfeatures <- grep("(mean|std)\\(\\)", features[, featureNames])
        measures <- features[wfeatures, featureNames]
        measures <- gsub('[()]', '', measures)
        
        #Set the directories that need to be visited
        testDir<- paste0(getwd(),"/test") 
        trainDir<-paste0(getwd(),"/train") 
        
        #Get into "test" directory in the file and read the .txt files under scope and
        #extract only the measurements on the mean and standard deviation
        setwd(testDir)
        print("Reading 'test' files...")
        # Load test datasets
        subjectTest <- fread("subject_test.txt", col.names = c("subjectNumber"))
        Xtest <- fread("X_test.txt")[, wfeatures, with = FALSE]
        data.table::setnames(Xtest, colnames(Xtest), measures)
        ytest <- fread("y_test.txt", col.names = c("activity"))
        test <- cbind(subjectTest, ytest, Xtest)
        
        #Get into "train" directory in the file and read the .txt files under scope and
        #extract only the measurements on the mean and standard deviation
        setwd(trainDir)
        print("Reading 'train' files...")
        Xtrain <- fread("X_train.txt")[, wfeatures, with = FALSE]
        data.table::setnames(Xtrain, colnames(Xtrain), measures)
        ytrain<- fread("y_train.txt", col.names = c("activity"))
        subjectTrain <- fread("subject_train.txt", col.names = c("subjectNumber"))
        train <- cbind(subjectTrain, ytrain, Xtrain)

        #Return to root/initial working directory
        setwd(oldwd)
        
        #Merge the training and test datasets to create one data set.
        print("Merging 'test' and 'train' datasets...")
        mergedDataSet <- rbind(test,train)

        # Convert 'labels' to activityName.
        mergedDataSet[["activity"]] <- factor(mergedDataSet[, activity], levels = activity_labels[["labels"]], labels = activity_labels[["activity"]])
        mergedDataSet[["subjectNumber"]] <- as.factor(mergedDataSet[, subjectNumber])
        mergedDataSet <- reshape2::melt(data = mergedDataSet, id = c("subjectNumber", "activity"))
        mergedDataSet <- reshape2::dcast(data = mergedDataSet, subjectNumber + activity ~ variable, fun.aggregate = mean)
        
        #Write the resulting dataset into a .txt file
        print("Writing tidy dataset into MytidyDataSet.txt...")
        data.table::fwrite(x = mergedDataSet, file = "MytidyDataSet.txt", quote = FALSE) 
        print("Analysis Completed.")
        



