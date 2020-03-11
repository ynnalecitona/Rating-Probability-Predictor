library(recosystem)
library(rectools)
library(partykit)

ratingProbsFit <- function(dataIn,maxRating,predMethod,embedMeans,specialArgs){
  # Check for errors where using or not using embeded means with an incompatible method. Will return NaN if incompatable.
  if( embedMeans and predMethod == "NMF" ) {
    stop("Do not need to embed means for NMF.\n")
  }
  if( !embedMeans and predMethod == "CART" ) {
    stop("Mandatory for CART to embed means.\n")
  }

  # If using embeded means replace the data with embeded data.
  if( embedMeans ) dataIn <- embedDataMeans(dataIn)


  dataIn[,3] <- ratingToDummy(dataIn[,3], maxRatings)

  # Call the proper predition method.
  if( predMethod == "logit" ) return Logit(dataIn,maxRating,specialArgs)
  if( predMethod == "NMF" ) return NMF(dataIn,maxRating,specialArgs)
  if( predMethod == "kNN" ) return KNN(dataIn,maxRating,embedMeans,specialArgs)
  if( predMethod == "CART" ) return CART(dataIn,maxRating,specialArgs)
}

predict <- function(probsFitOut,newXs) {

}

embedDataMeans <- function(dataIn) {
  # mean ratings for a user
  ud <- formUserData(dataIn)
  mean_users <- sapply(ud, function(oneusr) mean(oneusr$ratings))

  # mean ratings for an item
  # switch userid and itemid so that
  ud1 <- formUserData(dataIn[,c('itemID', 'userID', 'rating')])
  mean_items <- sapply(ud1, function(oneitm) mean(oneitm$ratings))

  # create new columns that holds the user_mean and item_mean
  # assuming that the user named the column headings as userID and itemID
  dataIn$user_mean <- mean_users[dataIn$userID]
  dataIn$item_mean <- mean_items[dataIn$itemID]

  # return a data frame that maps userid with user mean and
  # itemid with item mean
  mappings <- as.data.frame(dataIn[, c('userID', 'user_mean', 'itemID', 'item_mean')])
  return(mappings)
}

Logit <- function(dataIn,maxRating,embedMeans,specialArgs) {
  # no changes with embedMeans for logit, despite data type changes
  # prediction probabilities approach: pg. 36 of book, 3.3.6.3
}

NMF <- function(dataIn,maxRating,specialArgs) {
  # does not need embedMeans

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
