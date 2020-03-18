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
  
  dataIn <- ratingToDummy(dataIn, maxRating)

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
  if( probsFitOut$predMethod == "CART" ) return(CARTPredict(probsFitOut,newXs))
}

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

#### HELPER FUNCTIONS ####
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
