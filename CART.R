library(recosystem)
library(rectools)
library(partykit)
library(data.table)

measureAccuracy <- function(ratingProbs,actualVals,maxRating) {
  comparisonMatrix <- matrix(0, nrow=maxRating, ncol=2)
  for (i in 1:maxRating) {
    comparisonMatrix[i,] <- c(mean(ratingProbs[,i]), sum(actualVals == i) / length(actualVals))
  }
  comparisonMatrix
}

lookUp <- function(onekey, keys, vals) {
  vals[which(as.character(keys) == as.character(onekey))[1]]
}

CARTPredictv2 <- function(probsFitOut,newXs) {
  # transform newXs
  newXs$userID <- sapply(newXs$userID, lookUp, probsFitOut$mappings$userID, probsFitOut$mappings$user_mean)
  newXs$itemID <- sapply(newXs$itemID, lookUp, probsFitOut$mappings$itemID, probsFitOut$mappings$item_mean)
  
  ratings <- lapply(probsFitOut$parties, function(party) round(predict(party, newXs)))
  ratingsDF <- data.frame(matrix(unlist(ratings), nrow=length(probsFitOut$parties), byrow=T))
  ratingProbs <- matrix(0, nrow=nrow(newXs), ncol=probsFitOut$maxRating)
  for (i in 1:nrow(ratingProbs)) {
    ratingTbl <- table(ratingsDF[,i])
    ratingProbs[i,as.numeric(names(ratingTbl))] <- ratingTbl / sum(ratingTbl)
  }
  ratingProbs
}

computeProbs <- function(ecdfFunc, maxRating) {
  probs <- sapply(1:maxRating, function(rating) ecdfFunc(rating + 0.5) - ecdfFunc(rating - 0.5))
}

CARTPredict <- function(probsFitOut,newXs) {
  # transform newXs
  newXs$userID <- sapply(newXs$userID, lookUp, probsFitOut$mappings$userID, probsFitOut$mappings$user_mean)
  newXs$itemID <- sapply(newXs$itemID, lookUp, probsFitOut$mappings$itemID, probsFitOut$mappings$item_mean)

  ratings <- predict(probsFitOut$party, newXs, type="prob")
  ratingProbs <- sapply(ratings, computeProbs, probsFitOut$maxRating)
  t(ratingProbs)
}

CART <- function(dataIn,maxRating,embedMeans,specialArgs) {
  # has to use embedMeans
  recProbs <- list(method="CART", maxRating=maxRating)
  class(recProbs) <- "recProbs"

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

CARTv2 <- function(dataIn,maxRating,embedMeans,specialArgs) {
  # has to use embedMeans
  recProbs <- list(method="CART", maxRating=maxRating, parties=list())
  class(recProbs) <- "recProbs"

  mappings <- embedDataMeans(dataIn)
  recProbs$mappings <- mappings
  dataIn$userID <- mappings$user_mean
  dataIn$itemID <- mappings$item_mean

  args <- specialArgs
  args$formula <- as.formula('rating ~ .')

  ntrees <- 100
  for (i in 1:ntrees) {
    idxs <- sample(1:nrow(dataIn), 0.9*nrow(dataIn))
    args$data <- dataIn[idxs,]
    ctout <- do.call(ctree, args)
    recProbs$parties[[i]] <- ctout
  }
  recProbs
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
