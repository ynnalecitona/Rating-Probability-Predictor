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
	total_time <- Sys.time()
	# The list object to be outputted containing 3 values the prediction method and the list of trained models
	outProbFit <- vector('list', 3)
	names(outProbFit) <- c('predMethod', 'models', 'z')
	outProbFit$predMethod <- 'NMF'
	outProbFit$models <- vector('list', maxRating)
	givenRank <- FALSE

	# If the user does provide a rank then use that rank instead of tuning for a rank.
	if( 'rank' %in% names(specialArgs) ) {
		givenRank <- TRUE
	}

	# If the user wishes to increase the number of threads used to tune then they can be modified here.
	# The default is 1.
	nthread <- 1
	if ( 'nthread' %in% names(specialArgs) ) {
		nthread <- specialArgs$nthread
	}

	index1 <- specialArgs$index1

	# Over all the output columns
	for( i in 1:maxRating ) {
		# Factor in the user and item columns to get the current rating column
		nRatingCol <- i + 2
		
		reco <- Reco()

		# Init the training data for the current rating column
		training <- data_memory(dataIn[,1], dataIn[,2], dataIn[,nRatingCol], index1 = index1)
		if( givenRank ){
			reco$train(training, out_model = tempfile(), opt = list(dim = rank, nmf=TRUE, nthread = nthread))
		}
		else {
			print("Begining to tune the Reco Model")
			print("Rating:")
			print(i)
			start_time <- Sys.time()
			opts = list(dim = c(25,50,100,200), nmf = TRUE, costp_l1 = 0, costp_l2 = 0, costq_l1 = 0, costq_l2 = 0, lrate = 0.1, nmf = TRUE, nthread = nthread, progress = TRUE, verbose = TRUE)
			tuned <- reco$tune(training, opts = opts)
			end_time <- Sys.time()

			fn <- paste(as.character(i),"tuning.data")
			saveRDS(tuned, file=fn)

			print('Finished tuning the Reco Model')
			print("Rating:")
			print(i)
			print("Current tume time:")
    		print(end_time - start_time)
    		print("Total time:")
    		print(end_time - total_time)

			reco$train(training, opts = tuned$min)
		}
			  	
		result <- reco$output(out_P = out_memory(), out_Q =  out_memory())
		outProbFit$models[[i]] <- result
	}

	return(outProbFit)
}

NMFPredict <- function(probsFitOut,newData) {
	nNewData <- nrow(newData)
	
	models <- probsFitOut$models
	nModels <- length(models)

	# Predictions with a column for each possible rating and a row for each new data input.
	preds <- matrix(nrow = nNewData, ncol = nModels)
	# For each new datum that we are given
	for(i in 1:nNewData) {
		# TODO remove print statement
		print(i)
		# For each model of a different rating
		for(j in 1:nModels) {
			newDatum <- newData[i,]
			# The prediction is equal to the dot product of the usersID row in P and the itemID col in Q 
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
