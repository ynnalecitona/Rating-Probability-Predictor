library(recosystem)
library(rectools)
library(partykit)
library(data.table)

ratingProbsFit <- function(dataIn,maxRating,predMethod,embedMeans,specialArgs){
  # Check for errors where using or not using embeded means with an incompatible method.
  # Will stop execution because of invalid inputs.
  if( embedMeans && predMethod == "NMF" ) {
    stop("Do not need to embed means for NMF.\n")
  }
  if( !embedMeans && predMethod == "CART" ) {
    stop("Mandatory for CART to embed means.\n")
  }

  # If using embeded means replace the data with embeded data.
  if( embedMeans ) dataIn <- embedDataMeans(dataIn)

  # Convert the last col ( which should be a rating integer > 0 and < maxRating ) into a dummy variable with maxRating columns
<<<<<<< Updated upstream
  dataIn[,3] <- ratingToDummy(dataIn[,3], maxRatings)

=======
  dataIn <- ratingToDummy(dataIn, maxRating)
  
>>>>>>> Stashed changes
  # Call the proper predition method.
  if( predMethod == "logit" ) return(Logit(dataIn,maxRating,embedMeans,specialArgs))
  if( predMethod == "NMF" ) return(NMFTrain(dataIn,maxRating,specialArgs))
  if( predMethod == "kNN" ) return(KNN(dataIn,maxRating,embedMeans,specialArgs))
  if( predMethod == "CART" ) return(CART(dataIn,maxRating,specialArgs))
}

predict <- function(probsFitOut,newXs) {
  if( probsFitOut$predMethod == "logit" ) return(Logit(dataIn,maxRating,embedMeans,specialArgs))
  if( probsFitOut$predMethod == "NMF" ) return(NMFPredict(probsFitOut,newXs))
  if( probsFitOut$predMethod == "kNN" ) return(KNN(dataIn,maxRating,embedMeans,specialArgs))
  if( probsFitOut$predMethod == "CART" ) return(CART(dataIn,maxRating,specialArgs))
}


Logit <- function(dataIn,maxRating,embedMeans,specialArgs) {
  # no changes with embedMeans for logit, despite data type changes
  # prediction probabilities approach: pg. 36 of book, 3.3.6.3
}

NMFTrain <- function(dataIn,maxRating,specialArgs) {
  # does not need embedMeans
  rank <- specialArgs$rank

  models <- vector('list', maxRating)

  # Over all the output columns
  for( i in 1:maxRating ) {
  	# Factor in the user and item columns to get the current rating column
  	nRatingCol <- i + 2
  	
  	reco <- Reco()
  	training <- data_memory(dataIn[,1], dataIn[,2], dataIn[,nRatingCol], index1 = TRUE)
  	reco$train(training, out_model = "train.txt", opt = list(dim = rank, nmf=TRUE))	
  	
  	result <- reco$output(out_P = out_memory(), out_Q =  out_memory())
  	
  	models[[i]] <- result$P %*% t(result$Q)
  }
  
  outProbFit <- vector('list', 3)
  names(outProbFit) <- c('method', 'models')
  outProbFit$predMethod <- 'NMF'
  outProbFit$z <- specialArgs$z
  outProbFit$models <- models

  return(outProbFit)
}

NMFPredict <- function(probsFitOut,newData) {
	nNewData <- nrow(newData)
	
	models <- probsFitOut$models
	nModels <- length(models)

	preds <- matrix(nrow = nNewData, ncol = nModels)
	# For each new datum that we are given
	for(i in 1:nNewData) {
		# For each model of a different rating
		for(j in 1:nModels) {
			newDatum <- newData[i,]
			preds[i,j] <- models[[j]][newDatum[[1]],newDatum[[2]]]
		}
		preds[i,] <- softmax(preds[i,])
	}
	# rows returned are the predictions of each rating for a new datum
	return(preds)
}

KNN <- function(dataIn,maxRating,embedMeans,specialArgs) {
  # embedMeans: look at entire database, find all the users who've rated this item
  # get costDistance
  # should be on page 80-ish of book
  # open-ended: defining how 2 users are similar

}

CART <- function(dataIn,maxRating,embedMeans,specialArgs) {
  # has to use embedMeans
}

#### HELPER FUNCTIONS ####
ratingToDummy <- function(data, maxRatings) {

  # Extract the ratings column from the data and trim it
  numCols <- ncol(data)
  ratings <- data[,numCols]
  data <- data[,-c(numCols)]

  # Add the new columns to the matrix

  for(i in 1:maxRatings) {
    # Create the name for the dummy variable column in format "r + rating number"
    name <- paste("r",toString(i), sep = "")

    # Each value in the new column represents a boolean that is true if the user gave an item the rating "i"
    data[,numCols - 1 + i] <- as.integer(ratings == i)

    # Add the name to the newly created column
    names(data)[numCols - 1 + i] <- name
  }

  return(data)
}

embedDataMeans <- function(dataIn) {
  # mean ratings for a user
  ud <- formUserData(dataIn)
  mean_users <- sapply(ud, function(oneusr) mean(oneusr$ratings))

  # mean ratings for an item
  # switch userid and itemid so that
  udReversed <- formUserData(dataIn[,c('itemID', 'userID', 'rating')])
  mean_items <- sapply(udReversed, function(oneitm) mean(oneitm$ratings))

  # create new columns that holds the user_mean and item_mean
  # assuming that the user named the column headings as userID and itemID
  dataIn$user_mean <- mean_users[dataIn$userID]
  dataIn$item_mean <- mean_items[dataIn$itemID]

  # To remove duplicated data
  # mappings_users <- dataIn[, c('userID', 'user_mean')]
  # mappings_users <- unique(mappings_users)

  # mappings_items <- dataIn[, c('itemID', 'item_mean')]
  # mappings_items <- unique(mappings_items)
  # mappings_items <- mappings_items[order(mappings_items$itemID),]

  # Can't merge the two, columns don't match

  # return a data frame that maps userid with user mean and
  # itemid with item mean
  mappings <- as.data.frame(dataIn[, c('userID', 'user_mean', 'itemID', 'item_mean')])
  return(mappings)
}

dataToMatrix <- function(dataIn) {
  # turns data frame into data.table which is more enhanced than data.frame
  dt <- as.data.table(dataIn)

  # creates the table entries, and fill empty ones with NAs
  userID <- names(dataIn)[1]
  itemID <- names(dataIn)[2]
  valueName <- names(dataIn)[3]
  formula <- paste(c(userID, itemID), collapse = '~')
  table <- dcast(dt, formula, fill = NA, value.var=valueName)

  # The first column of table will be the userID, which we don't need in our matrix
  mat <- as.matrix(table[,-1])

  # Assign the row names of the new matrix with our userIDs
  rNames <- as.list(table[,1])
  rNames <- rNames[[names(rNames)]]
  row.names(mat) <- rNames
  return(mat)
}

measurePerformance <- function(predProbs, truthValues) {
  nTruths <- length(truthValues)
  maxRating <- ncol(predProbs)

  avgPredProbs <- apply(predProbs, 2, mean)

  truthTotals <- rep(0, maxRating)

  for ( i in 1:nTruths ) { 
    rating <- truthValues[i]
    truthTotals[rating] <- truthTotals[rating] + 1
  }

  truthProbs <- truthTotals/nTruths

  print("The average distribution we compute is:")
  print(avgPredProbs)
  print("the true distribution was:")
  print(truthProbs)
  print("The absolute percetage error is:")
  print(abs(avgPredProbs - truthProbs))
  print('The mean absolute percetage error is:')
  print(mean(abs(avgPredProbs - truthProbs)))
  return(mean(abs(avgPredProbs - truthProbs)))
}

softmax <- function(values, z = 1) {
	temp <- values * z
	expValues <- sapply(temp, exp)
	return( expValues/sum(expValues) )
}
