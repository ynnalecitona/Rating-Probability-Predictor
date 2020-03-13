# Assume data has 3 columns: userID, itemID, rating
# Ensure that sample contains all userIDs and itemIDs

# If the targetList contains all items in the checkList, returns True
does_contain_all <- function(targetList, checkList)
{
        numElementsLacked <- sum(!(checkList %in% targetList))
        return(numElementsLacked == 0)
}

# Set verbose to TRUE if want to see how many resampling trials the function needed
sampling_data <- function(dataIn, sampleSize, verbose = FALSE)
{
        if (sampleSize > nrow(dataIn)) {
                print("Error: Sample size greater than data size")
                return(-1)
        }
        uniqueUsersTotal <- unique(dataIn[,1])
        uniqueItemsTotal <- unique(dataIn[,2])
        samplingIdx <- sample(1:nrow(dataIn), sampleSize)
        sampleData <- dataIn[samplingIdx,]
        i <- 1
        while (!does_contain_all(sampleData[,1], uniqueUsersTotal)
            || !does_contain_all(sampleData[,2], uniqueItemsTotal)) {
                samplingIdx <- sample(1:nrow(dataIn), sampleSize)
                sampleData <- dataIn[samplingIdx,]
                i <- i + 1
        }
        if (verbose) {
                print("Trials needed: ")
                print(i)
        }
        return(sampleData)
}

# Train, validation, and test set division restriction:
#       Train set has to contain all userID and itemID in the overal dataset
#       Validation set and test set don't have the same restriction
# ratio is a list of 3 percentages, indicating how to divide dataIn into the 3 sets
#       ratio = [trainSetPercentage, validationSetPercentage, testSetPercentage]
create_experiment_sets <- function(dataIn, ratio)
{
        if (sum(ratio) != 1) {
                print("Invalid ratios")
                return(-1)
        }

        trainSetSize <- floor(nrow(dataIn) * ratio[1])
        validationSetSize <- floor(nrow(dataIn) * ratio[2])
        testSetSize <- nrow(dataIn) - trainSetSize - validationSetSize

        trainSet <- sampling_data(dataIn, trainSetSize)
        print("Train set sampling done")

        sampledRows <- as.numeric(rownames(trainSet))
        # We can do this since row index == row name for all rows
        leftovers <- dataIn[-sampledRows,]
        # Rename row names so we can reuse the trick above for getting the test set
        rownames(leftovers) <- 1:nrow(leftovers)

        samplingIdx <- sample(1:nrow(leftovers), validationSetSize)
        validationSet <- leftovers[samplingIdx,]
        print("Validation set sampling done")

        sampledRows <- as.numeric(rownames(validationSet))
        testSet <- leftovers[-sampledRows,]
        print("Test set sampling done")

        experiment_sets <- list(trainSet = trainSet, validationSet = validationSet, testSet = testSet)
        class(experiment_sets) <- "expSets"
        return(experiment_sets)
}

save_experiment_sets <- function(expSets)
{
        trainSet <- expSets$trainSet
        validationSet <- expSets$validationSet
        testSet <- expSets$testSet
        write.csv(trainSet, "train.data", row.names = FALSE)
        write.csv(validationSet, "validation.data", row.names = FALSE)
        write.csv(testSet, "test.data", row.names = FALSE)
}

#-----------------------How to create the experiment sets-----------------------
#library(lme4)
#ie <- InstEval
#ie <- ie[,c(1,2,7)] # Only care about columns depicting userID, itemID, and rating
#ratio <- c(0.7,0.2,0.1)
#e <- create_experiment_sets(ie, ratio)
#save_experiment_sets(e)
