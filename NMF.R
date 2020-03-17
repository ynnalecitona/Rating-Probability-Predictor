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
  dataIn <- ratingToDummy(dataIn, maxRating)

  # Call the proper predition method.
  if( predMethod == "logit" ) return(Logit(dataIn,maxRating,embedMeans,specialArgs))
  if( predMethod == "NMF" ) return(NMFTrain(dataIn,maxRating,specialArgs))
  if( predMethod == "kNN" ) return(KNN(dataIn,maxRating,embedMeans,specialArgs))
  if( predMethod == "CART" ) return(CART(dataIn,maxRating,specialArgs))
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
  	fn <- "train.txt"
	#Check its existence
	if (file.exists(fn)) 
	  #Delete file if it exists
	  file.remove(fn)
  	reco$train(training, out_model = fn, opt = list(dim = rank, nmf=TRUE))	
  	
  	result <- reco$output(out_P = out_memory(), out_Q =  out_memory())
  	models[[i]] <- result
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
	print(nNewData)
	for(i in 1:nNewData) {
		print(i)
		# For each model of a different rating
		for(j in 1:nModels) {
			newDatum <- newData[i,]
			preds[i,j] <- models[[j]]$P[newDatum[[1]],] %*% models[[j]]$Q[newDatum[[2]],]
		}
		preds[i,] <- softmax(preds[i,])
	}
	# rows returned are the predictions of each rating for a new datum
	return(preds)
}



#### HELPER FUNCTIONS ####
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


softmax <- function(values, z = 1) {
	temp <- values * z
	expValues <- sapply(temp, exp)
	return( expValues/sum(expValues) )
}
