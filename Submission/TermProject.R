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
  dataInNoDummy <- dataIn
  dataIn <- ratingToDummy(dataIn, maxRating)

  if( predMethod == "logit" ) return(Logit(dataIn,maxRating))
  # Call the proper predition method.
  if( predMethod == "logit" && embedMeans ) {
    dataIn <- embedDataMeans(dataIn)
    return(Logit(dataIn,maxRating))
  }

  if( predMethod == "NMF" ) return(NMFTrain(dataIn,maxRating,specialArgs))

  if( predMethod == "kNN" ) return(KNN_setup(dataInNoDummy,maxRating,specialArgs))

  if( predMethod == "CART" ) return(CART(dataIn,maxRating,specialArgs))
}

predict <- function(probsFitOut,newXs) {
  if( probsFitOut$predMethod == "logit" ) return(LogitPredict(probsFitOut,newXs))
  if( probsFitOut$predMethod == "NMF" ) return(NMFPredict(probsFitOut,newXs))
  if( probsFitOut$predMethod == "kNN" ) return(KNN_predict(probsFitOut, newXs))
  if( probsFitOut$predMethod == "CART" ) return(CARTPredict(probsFitOut,newXs))
}

############################ LOGIT SECTION ############################

Logit <- function(dataIn, maxRating) {

  # Parameter Description
  # - dataIn: Data set with userID, itemID, Additional Predictors, Dummy Ratings
  # - maxRating: Maximum possible numerical rating that an item can be given

  # Verify that the dataIn dataframe is valid
  if(names(dataIn)[1] != "userID" | names(dataIn)[2] != "itemID") {
    write("logit: dataIn missing userID or itemID", stderr())
    return(status = -1)
  }

  # Declare the return object that will hold the linear models
  dataOut <- list(predMethod = "logit", maxRating = maxRating)

  # One vs. All approach to generate a binomial linear model for each possible rating
  for(i in 1:maxRating) {

    # Compute the index of the response variable
    index <- (ncol(dataIn) - maxRating + i)

    # Create the string for the formula
    responseName <- paste("r",toString(i), sep = "")
    form <- paste(responseName, "~ userID + itemID")

    # Create the logit model
    glmout <- glm(formula = form, data = dataIn, family = binomial, maxit = 50)

    # Add the glmout objects to the return object
    columnName <- paste("p",toString(i), sep = "")
    dataOut <- append(dataOut, list(glmout))
    names(dataOut)[2 + i] <- columnName
  }

  class(dataOut) <- "logit"

  return(dataOut)
}

LogitPredict <- function(probsFitOut, newXs) {

  # Generate the prediction probability for the first layer
  maxRatings <- probsFitOut[[2]]
  dataOut <- matrix(0, nrow = nrow(newXs), ncol = maxRatings)
  for(i in 1:maxRatings) {

    predictions <- predict.glm(probsFitOut[[2+i]], newXs, type = 'response')
    dataOut[,i] <- predictions
    colnames(dataOut)[i] <- paste("p",toString(i), sep = "")
  }

  return(dataOut)
}

############################ NMF SECTION ############################

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
			print(paste("Begining to tune the Reco Model for Rating:", i))
			opts = list(dim = c(25,50,100,200,500,1000,2000), nmf = TRUE, costp_l1 = 0, costp_l2 = 0, costq_l1 = 0, costq_l2 = 0, lrate = 0.1, nmf = TRUE, nthread = nthread, progress = TRUE, verbose = TRUE)
			tuned <- reco$tune(training, opts = opts)

			fn <- paste(as.character(i),"SongYEs.data")
			saveRDS(tuned, file=fn)

			print(paste("Finished tuning the Reco Model for Rating:", i))
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

############################ KNN SECTION ############################

# specialArgs contains k (i.e. number of nearest neighbours)
KNN_setup <- function(dataIn, maxRating, specialArgs)
{
        trainData <- dataIn
        if ((.Machine$integer.max / length(unique(dataIn[,1]))) > length(unique(dataIn[,2]))) {
                dataSetSize <- "small"
        } else {
                dataSetSize <- "big"
        }
        maxRating <- maxRating
        KNNData <- list(trainData = dataIn, mode = dataSetSize, predMethod = "kNN",
                        k = specialArgs, maxRating = maxRating)
        class(KNNData) <- "recProbs"
        return(KNNData)
}

KNN_predict <- function(probsFitOut, newXs)
{
        if (probsFitOut$mode == "small")
                find_kNN_small(probsFitOut$trainData,
                               probsFitOut$k,
                               probsFitOut$maxRating,
                               newXs)
        else
                find_kNN_big(probsFitOut$trainData,
                             probsFitOut$k,
                             probsFitOut$maxRating,
                             newXs)
}

#-----------------------------kNN parameters checking-----------------------------

is_valid_values <- function(k, maxRating)
{
        valid <- TRUE
        if (k <= 0) {
                print("Error: k can't be less than or equal to 0.")
                valid <- FALSE
        }
        if (maxRating < 1) {
                print("Error: maxRating can't be less than 1.")
                valid <- FALSE
        }
        return(valid)
}

#-----------------------------kNN similarity funcs-----------------------------

# Similarity value: the bigger == the more similar

get_vlen <- function(v) sqrt(v %*% v)

#----------In-house similarity----------

inhouseSim <- function(potentialUser, targetUser)
{
        commItemsIdx <- !is.na(potentialUser) & !is.na(targetUser)
        if (sum(commItemsIdx) == 0) return(NaN)

        xvec <- potentialUser[commItemsIdx]
        yvec <- targetUser[commItemsIdx]
        yLen <- get_vlen(yvec)
        dotProd <- (xvec %*% yvec)
        pCos <- dotProd / yLen
        p_tCos <- ((xvec - yvec) %*% yvec) / yLen
        cosine <- dotProd / (get_vlen(xvec) * yLen)
        (pCos - p_tCos) * cosine
}

#-------------------------------kNN calculations-------------------------------

# Find users who have rated the target item.
find_rated_users <- function(dataIn, targetItemIdx)
{
        return(dataIn[which(!is.na(dataIn[,targetItemIdx])),])
}

# Calculate each rated user's similarity to the target user.
# Here is where to choose which similarity function to use.
# NOTE: Make sure to change the similarity function used in both the if and the else
#       statements if you're planning of using only one similarity function
calc_sims <- function(ratedUsers, targetUser, numRatedUsers)
{
        if (numRatedUsers == 1) {
                simVec <- inhouseSim(ratedUsers, targetUser)
        } else {
                simVec <- vector(mode = "numeric", length = nrow(ratedUsers))
                for (i in 1:numRatedUsers)
                        simVec[i] <- inhouseSim(ratedUsers[i,], targetUser)
        }
        return(simVec)
}

calc_rating_probs <- function(targetUser, targetItemIdx, ratedUsers,
                              simVec, k, maxRating)
{
        # Each element in simVec corresponds to the same element in ratedUsers
        sortOrder <- sort(simVec, decreasing = TRUE, na.last = TRUE, index.return = TRUE)
        if (length(sortOrder$ix) == 0) {
                # No nearest neighbours found
                # Return probability vector of all zeros.
                return(vector(mode = "numeric", length = maxRating))
        } else {
                if (length(sortOrder$ix) == 1) {
                        # Only 1 neighbor
                        nearestNeighbours <- ratedUsers
                        numNN <- 1
                        kNN <- nearestNeighbours[targetItemIdx]
                } else {
                        # If don't have k nearest neighbours,
                        #       use as many as there are
                        nearestNeighbours <- ratedUsers[sortOrder$ix,]
                        numNN <- min(k, nrow(nearestNeighbours))
                        kNN <- nearestNeighbours[1:numNN,targetItemIdx]
                }
                # Vector of each rating's frequency among k nearest neighbours
                ratings <- vector(mode = "numeric", length = maxRating)
                for (rating in 1:maxRating)
                        ratings[rating] = sum(kNN == rating) / numNN
                return(ratings)
        }
}

save_mat <- function(ratingPredMat, i, fileName)
{
        print("Saving matrix")
        name <- paste(fileName,".mat", i, sep="")
        write.csv(ratingPredMat, name, row.names = FALSE)
        # Only keep latest 5 iterations
        oldFileName <- paste(fileName,".mat", i - 5*10000, sep="")
        if (file.exists(oldFileName))
                unlink(oldFileName)
}

# find_kNN's assumptions:
#       1. newXs doesn't contain any new users or items
#       2. ratings have consecutive integer values, ranging from 1 to maxRating.

# This version of kNN is for cases where dataIn can be entirely converted into a matrix.
#       i.e. (# unique users * # unique items) < .Machine$integer.max
find_kNN_small <- function(dataIn, k, maxRating, newXs,
                           fileName = NULL, verbose = FALSE, progressSaving = FALSE)
{
        if(is_valid_values(k, maxRating) == FALSE)
                return(-1)

        dataMat <- dataToMatrix(dataIn)
        ratingPredMat <- matrix(nrow = nrow(newXs), ncol = maxRating)
        for (i in 1:nrow(newXs)) {
                if (verbose)
                        print(i)
                targetUserIdx <- which(rownames(dataMat) == newXs[i,1])
                targetItemIdx <- which(colnames(dataMat) == newXs[i,2])

                if (!is.na(dataMat[targetUserIdx, targetItemIdx])) {
                        # Known rating
                        ratings <- vector(mode = "integer", length = maxRating)
                        ratings[dataMat[targetUserIdx, targetItemIdx]] = 1
                        ratingPredMat[i,] <- ratings
                } else {
                        ratedUsers <- find_rated_users(dataMat, targetItemIdx)
                        targetUser <- dataMat[targetUserIdx,]
                        numRatedUsers <- sum(!is.na(dataMat[,targetItemIdx]))
                        simVec <- calc_sims(ratedUsers, targetUser, numRatedUsers)
                        ratingPredMat[i,] <- calc_rating_probs(targetUser,
                                                               targetItemIdx,
                                                               ratedUsers,
                                                               simVec,
                                                               k,
                                                               maxRating)
                }
                if (progressSaving && i %% 10000 == 0)
                        save_mat(ratingPredMat, i, fileName)
        }
        return(ratingPredMat)
}

find_kNN_big <- function(dataIn, k, maxRating, newXs,
                         fileName = NULL, verbose = FALSE, progressSaving = FALSE)
{
        if(is_valid_values(k, maxRating) == FALSE)
                return(-1)
        # byrow = TRUE allows us to replace a matrix's row with a vector
        ratingPredMat <- matrix(nrow = nrow(newXs), ncol = maxRating, byrow = TRUE)
        for (i in 1:nrow(newXs)) {
                if (verbose)
                        print(i)
                foundRating <- dataIn[,1] == newXs[i,1] & dataIn[,2] == newXs[i,2]

                # Assume no repeated rating
                if (sum(foundRating) > 0) {
                        # Known rating
                        ratings <- vector(mode = "integer", length = maxRating)
                        ratings[dataIn[foundRating,3]] = 1
                        ratingPredMat[i,] <- ratings
                } else {
                        # Get users who have rated item newXs[i,2]
                        ratedUsersIDs <- dataIn[which(dataIn[,2] == newXs[i,2]),1]
                        targetUserID <- newXs[i,1]

                        ratedUsersIdx <- which(dataIn[,1] %in% ratedUsersIDs)
                        targetUserIdx <- which(dataIn[,1] %in% targetUserID)

                        dataExtract <- dataToMatrix(dataIn[c(ratedUsersIdx,targetUserIdx),])

                        targetUserIdx <- which(rownames(dataExtract) == newXs[i,1])
                        targetItemIdx <- which(colnames(dataExtract) == newXs[i,2])

                        ratedUsers <- dataExtract[-targetUserIdx,]
                        targetUser <- dataExtract[targetUserIdx,]
                        numRatedUsers <- length(ratedUsersIDs)
                        simVec <- calc_sims(ratedUsers, targetUser, numRatedUsers)
                        ratingPredMat[i,] <- calc_rating_probs(targetUser,
                                                               targetItemIdx,
                                                               ratedUsers,
                                                               simVec,
                                                               k,
                                                               maxRating)
                }
                if (progressSaving && i %% 10000 == 0)
                        save_mat(ratingPredMat, i, fileName)
        }
        return(ratingPredMat)
}

############################ CART SECTION ############################

CART <- function(dataIn,maxRating,specialArgs) {
  recProbs <- list(predMethod="CART", maxRating=maxRating)
  class(recProbs) <- "recProbs"

  # map user ID to user's mean rating, item ID to item's mean rating
  mappings <- embedDataMeans(dataIn)
  recProbs$mappings <- mappings
  dataIn$userID <- mappings$user_mean
  dataIn$itemID <- mappings$item_mean

  args <- specialArgs
  args$formula <- as.formula('rating ~ .')
  args$data <- dataIn

  ctout <- do.call(ctree, args)
  recProbs$party <- ctout
  recProbs
}

CARTPredict <- function(probsFitOut,newXs) {
  # transform newXs
  lookUp <- function(onekey, keys, vals) vals[which(as.character(keys) == as.character(onekey))[1]]
  newXs$userID <- sapply(newXs$userID, lookUp, probsFitOut$mappings$userID, probsFitOut$mappings$user_mean)
  newXs$itemID <- sapply(newXs$itemID, lookUp, probsFitOut$mappings$itemID, probsFitOut$mappings$item_mean)

  ratings <- predict(probsFitOut$party, newXs, type="prob")

  computeProbs <- function(ecdfFunc, maxRating) probs <- sapply(1:maxRating, function(rating) ecdfFunc(rating + 0.5) - ecdfFunc(rating - 0.5))
  ratingProbs <- sapply(ratings, computeProbs, probsFitOut$maxRating)
  t(ratingProbs)
}

############################ HELPER FUNCTIONS ############################
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

ratingToDummy <- function(data, maxRatings) {

  # Verify that maxRating is valid
  if(!(maxRatings > 0)) {
    write("ratingToDummy: maxRating is an invalid number", stderr())
    return(status = -1)
  }

  # Extract the ratings from the data
  ratings <- data[,3]
  data <- data[,-3]

  # Add the dummy variable ratings to the end of the data
  for(i in 1:maxRatings) {

    # Create the name for the new column in the format "r + rating number"
    name <- paste("r", toString(i), sep = "")

    # Each value in the column represents a bool that is true if the user gave
    # the item the rating "i"
    data[,(ncol(data) + 1)] <- as.integer(ratings == i)

    # Add the name to newly created column
    names(data)[ncol(data)] <- name
  }

  return(data)
}

softmax <- function(values, z = 1) {
  temp <- values * z
  expValues <- sapply(temp, exp)
  return( expValues/sum(expValues) )
}
