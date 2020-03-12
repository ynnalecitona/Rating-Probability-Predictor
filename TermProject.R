library(recosystem)
library(regtools)
library(partykit)

ratingProbsFit <- function(dataIn,maxRating,predMethod,embedMeans,specialArgs){
  # Check for errors where using or not using embeded means with an incompatible method. Will return NaN if incompatable.
  if( embedMeans and predMethod == "NMF" ) return NaN
  if( !embedMeans and predMethod == "CART" ) return NaN
  
  # If using embeded means replace the data with embeded data.
  if( embedMeans ) dataIn <- embedDataMeans(dataIn)
  
  
  dataIn <- ratingToDummy(dataIn, maxRatings)
  
  # Call the proper predition method.
  if( predMethod == "logit" ) return Logit(dataIn,maxRating,specialArgs)
  if( predMethod == "NMF" ) return NMF(dataIn,maxRating,specialArgs)
  if( predMethod == "kNN" ) return KNN(dataIn,maxRating,embedMeans,specialArgs)
  if( predMethod == "CART" ) return CART(dataIn,maxRating,specialArgs)
}

predict <- function(probsFitOut,newXs) {
  
}

embedDataMeans <- function(dataIn) {
  # http://heather.cs.ucdavis.edu/~matloff/189G/Exams/W20Quiz5Answers.txt
  library(rectools)
  ud <- formUserData(dataIn)
  ### Complete ###
}

Logit <- function(dataIn,maxRating,embedMeans,specialArgs) {
  # no changes with embedMeans for logit, despite data type changes
  # prediction probabilities approach: pg. 36 of book, 3.3.6.3
}

NMFTrain <- function(dataIn,maxRating,specialArgs) {
  # does not need embedMeans
  rank <- specialArgs$rank
  
  models <- vector('list', maxRating)
  
  for( i in 1:maxRating ) {
  	reco <- Reco()
  	training <- data_memory(dataIn[,1], dataIn[,2], dataIn[,3], index1 = TRUE)
  	reco$train(training, out_model = "train.txt", opt = list(dim = rank, nmf=TRUE))	
  	
  	result <- reco$output(out_P = out_memory(), out_Q =  out_memory())
  	
  	models[[i]] <- result$p %*% result$q
  }
  
  return(models)
}

NMFPredict <- function(probsFitOut,newData) {
	nNewData <- nrow(newData)
	
	models <- probsFitOut$models
	nModels <- length(models)
	
	preds <- matrix(nrow = nNewData, ncol = nModels)
	# For each new datum that we are given
	for i in 1:nNewData {
		# For each model of a different rating
		for j in 1:nModels {
			newDatum <- newData[j]
			preds[i,j] <- models[[i]][newDatum[1],newDatum[2]]
		}
	}
	
	# rows returned are the predictions of each rating for a new datum
	return(preds)
}

KNN <-- function(dataIn,maxRating,embedMeans,specialArgs) {
  # embedMeans: look at entire database, find all the users who've rated this item
  # get costDistance
  # should be on page 80-ish of book
  # open-ended: defining how 2 users are similar
  
}

CART <- function(dataIn,maxRating,embedMeans,specialArgs) {
  # has to use embedMeans
}

ratingToDummy <- function(data, maxRatings) {
  
  # Extract the ratings column from the data and trim it
  numCols <- ncol(data)
  ratings <- data[,numCols]
  data <- data[,-c(numCols)] 
  
  # Add the new columns to the matrix
  
  for(i in 0:maxRatings) {
    # Create the name for the dummy variable column in format "r + rating number"
    name <- paste("r",toString(i), sep = "")
    
    # Each value in the new column represents a boolean that is true if the user gave an item the rating "i" 
    data[,numCols + i] <- as.integer(ratings == i)
    
    # Add the name to the newly created column
    names(data)[numCols + i] <- name  
  }
  
  return(data)  
}
