require(data.table)

cosineSim <- function(trainData)

# trainData should be in a data.table form
# specialArgs contains k (i.e. number of nearest neighbours)
KNN <- function(trainData, maxRating, embedMeans, specialArgs)
{
        #embedMeans()
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

# Since rbfk similarity assigns higher values for more similar pair of vector
# (value range: [0,1]), to match with our similarity criteria, we subtract said value
# from 1.
#----------Radial Basis Function Kernel similarity----------

rbfkSim <- function(potentialUser, targetUser)
{
        commItemsIdx <- !is.na(potentialUser) & !is.na(targetUser)
        if (sum(commItemsIdx) == 0) return(NaN)

        xvec <- potentialUser[commItemsIdx]
        yvec <- targetUser[commItemsIdx]
        sigma <- 1
        exp(-1 * (get_vlen(xvec - yvec) / (2 * (sigma ^ 2))))
}

#----------Correlation similarity----------

correlSim <- function(potentialUser, targetUser)
{
        commItemsIdx <- !is.na(potentialUser) & !is.na(targetUser)
        if (sum(commItemsIdx) == 0) return(NaN)

        xvec <- potentialUser[commItemsIdx]
        yvec <- targetUser[commItemsIdx]
        xMean <- mean(xvec)
        yMean <- mean(yvec)
        xSd <- sd(xvec)
        ySd <- sd(yvec)
        mean((xvec * yvec) - (xMean * yMean)) / (xSd * ySd)
}

#----------Chebychev similarity----------

chebychevSim <- function(potentialUser, targetUser)
{
        commItemsIdx <- !is.na(potentialUser) & !is.na(targetUser)
        if (sum(commItemsIdx) == 0) return(NaN)

        xvec <- potentialUser[commItemsIdx]
        yvec <- targetUser[commItemsIdx]
        # Once power by infinity, the max element will overtake all other ones.
        1 / max(abs(xvec - yvec))
}

#----------Euclidean similarity----------

euclideanSim <- function(potentialUser, targetUser)
{
        commItemsIdx <- !is.na(potentialUser) & !is.na(targetUser)
        if (sum(commItemsIdx) == 0) return(NaN)

        xvec <- potentialUser[commItemsIdx]
        yvec <- targetUser[commItemsIdx]
        1 / get_vlen(xvec - yvec)
}

#----------Manhattan similarity----------

manhattanSim <- function(potentialUser, targetUser)
{
        commItemsIdx <- !is.na(potentialUser) & !is.na(targetUser)
        if (sum(commItemsIdx) == 0) return(NaN)

        xvec <- potentialUser[commItemsIdx]
        yvec <- targetUser[commItemsIdx]
        1 / sum(abs(xvec - yvec))
}

#----------Cosine similarity----------

# Since more similar pair of vectors have higher cosine values (max = 1), to
# match our similarity criteria, we actually calculate the angle between
# the vectors, which explains the acos().
# Problem: c(1) & c(5) has a cosine val of 0 even though they are very different
cosineSim <- function(potentialUser, targetUser)
{
        # 1 & to compare each element of both vectors
        commItemsIdx <- !is.na(potentialUser) & !is.na(targetUser)
        if (sum(commItemsIdx) == 0) return(NaN)

        xvec <- potentialUser[commItemsIdx]
        yvec <- targetUser[commItemsIdx]
        ((xvec %*% yvec) / (get_vlen(xvec) * get_vlen(yvec)))
}

#-----------------------------kNN calculations-----------------------------

# Find users who have rated the target item
find_rated_users <- function(dataIn, targetItemIdx)
{
        return(dataIn[which(!is.na(dataIn[,targetItemIdx])),])
}

# Calculate each rated user's similarity to the target user.
# Here is where to choose which similarity function to use.
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

calc_rating_probs <- function(targetUser, targetItemIdx, ratedUsers, simVec,
                              k, maxRating)
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
                        ratings[rating] = length(kNN[kNN == rating]) / numNN
                return(ratings)
        }
}

# Assumptions:
#       1. newXs doesn't contain any new users or items
#       2. ratings have consecutive integer values, ranging from 1 to maxRating.
find_kNN <- function(dataIn, k, maxRating, newXs)
{
        if(is_valid_values(k, maxRating) == FALSE)
                return(-1)

        # byrow = TRUE allows us to replace a matrix's row with a vector
        ratingPredMat <- matrix(nrow = nrow(newXs), ncol = maxRating, byrow = TRUE)
        for (i in 1:nrow(newXs)) {
                targetUserIdx <- which(rownames(dataIn) == newXs[i,1])
                targetItemIdx <- which(colnames(dataIn) == newXs[i,2])

                if (!is.na(dataIn[targetUserIdx, targetItemIdx])) {
                        # Known rating
                        ratings <- vector(mode = "integer", length = maxRating)
                        ratings[dataIn[targetUserIdx, targetItemIdx]] = 1
                        ratingPredMat[i,] <- ratings
                } else {
                        ratedUsers <- find_rated_users(dataIn, targetItemIdx)
                        targetUser <- dataIn[targetUserIdx,]
                        simVec <- calc_sims(ratedUsers, targetUser)
                        ratingPredMat[i,] <- calc_rating_probs(targetUser,
                                                               targetItemIdx,
                                                               ratedUsers,
                                                               simVec,
                                                               k,
                                                               maxRating)
                }
        }
        return(ratingPredMat)
}

# Find users who have rated the target item
find_rated_users2 <- function(dataIn, targetItem)
{

        return(dataIn[which(!is.na(dataIn[,targetItemIdx])),])
}

make_user_vector <- function(dataIn, userID)
{
        userIdx <- which(dataIn[,1] == userID)
        #print(dataIn[userIdx,])
        userVec <- dataIn[userIdx,3]
        itemVec <- dataIn[userIdx,2]
        sortOrder <- sort(itemVec, na.last = NA, index.return = TRUE)
        itemVec <- itemVec[sortOrder$ix]
        userVec <- userVec[sortOrder$ix]
        names(userVec) <- itemVec
        #print(userVec)
        return(userVec)
}

# Calculate each rated user's similarity to the target user
# Here is where to choose which similarity func to use
calc_sims2 <- function(dataIn, ratedUsersIDs, targetUserID)
{
        targetUser <- make_user_vector(dataIn, targetUserID)
        simVec <- vector(mode = "numeric", length = length(ratedUsersIDs))
        for (i in 1:length(ratedUsersIDs)) {
                ratedUser <- make_user_vector(dataIn, ratedUsersIDs[i])
                simVec[i] <- inhouseSim2(ratedUser, targetUser)
        }
        sortOrder <- sort(ratedUsersIDs, na.last = NA, index.return = TRUE)
        #print(ratedUsersIDs[sortOrder$ix])
        #print(simVec[sortOrder$ix])
        return(simVec)
}

calc_rating_probs2 <- function(targetItemRatings, targetItemIdx, ratedUsersIDs, simVec, k, maxRating)
{
        #print(ratedUsersIDs)
        #print(simVec)
        sortOrder <- sort(simVec, na.last = TRUE, index.return = TRUE)
        #print(ratedUsersIDs[sortOrder$ix])
        #print(sortOrder)
        if (length(sortOrder$ix) == 0) {
                # No nearest neighbours found
                # Return probability vector of all zeros.
                return(vector(mode = "numeric", length = maxRating))
        } else {
                nearestNeighboursIDs <- ratedUsersIDs[sortOrder$ix]
                #print(nearestNeighboursIDs)
                #print(simVec[sortOrder$ix])
                if (length(sortOrder$ix) == 1) {
                        # Since nearestNeighbours is a matrix, it's implicitly
                        # converted to a vector when there's only 1 entry
                        # => need a different syntax for indexing
                        numNN <- 1
                        kNN <- nearestNeighboursIDs
                } else {
                        # If don't have k nearest neighbours,
                        # use as many as there are
                        numNN <- min(k, length(nearestNeighboursIDs))
                        kNN <- nearestNeighboursIDs[1:numNN]
                }
                kNNRatings <- targetItemRatings[which(targetItemRatings[,1] %in% kNN),3]
                #print(kNNRatings)
                ratings <- vector(mode = "numeric", length = maxRating)
                for (rating in 1:maxRating)
                        ratings[rating] = length(kNNRatings[kNNRatings == rating]) / numNN
                return(ratings)
        }
}

find_kNN2 <- function(dataIn, k, maxRating, newXs)
{
        if(is_valid_values(k, maxRating) == FALSE)
                return(-1)
        # byrow = TRUE allows us to replace a matrix's row with a vector
        ratingPredMat <- matrix(nrow = nrow(newXs), ncol = maxRating, byrow = TRUE)
        for (i in 1:nrow(newXs)) {
                targetUserIdx <- which(dataIn[,1] == newXs[i,1])
                targetItemIdx <- which(dataIn[,2] == newXs[i,2])
                foundRating <- intersect(targetUserIdx, targetItemIdx)

                if (length(foundRating) > 0) {
                        # Known rating
                        ratings <- vector(mode = "integer", length = maxRating)
                        ratings[dataIn[foundRating,3]] = 1
                        ratingPredMat[i,] <- ratings
                } else {
                        ratedUsersIDs <- dataIn[targetItemIdx,1]
                        targetUserID <- dataIn[targetUserIdx[1],1]
                        simVec <- calc_sims2(dataIn, ratedUsersIDs, targetUserID)
                        ratingPredMat[i,] <- calc_rating_probs2(dataIn[targetItemIdx,],
                                                                targetItemIdx,
                                                               ratedUsersIDs,
                                                               simVec,
                                                               k,
                                                               maxRating)
                }
        }
        return(ratingPredMat)
}

#-----------------------------kNN test-----------------------------

get_accuracy <- function(ratingPredMat, maxRating, newXs, correctRatings)
{
        ratingPredMax <- matrix(nrow = nrow(newXs), ncol = maxRating, byrow = TRUE)
        for (i in 1:nrow(newXs)) {
                max <- max(ratingPredMat[i,])
                ratingPredMax[i,] <- ratingPredMat[i,] == max
        }
        numCorrect <- 0
        for (j in 1:length(correctRatings)) {
                if (ratingPredMax[j,correctRatings[j]] == TRUE)
                        numCorrect <- numCorrect + 1
        }
        return(numCorrect / nrow(newXs))
}

test_kNN <- function(dataIn, k, maxRating, newXs, correctRatings)
{
        if (nrow(newXs) != length(correctRatings)) {
                print("Different lengths of prediction entries and target ratings")
                return(-1)
        } else {
                ratingPredMat <- find_kNN(dataIn, k, maxRating, newXs)
                #print(ratingPredMat)
                ratingPredMeans <- colMeans(ratingPredMat)
                actualMeans <- vector(mode = "numeric", length = maxRating)
                for (rating in 1:maxRating)
                        actualMeans[rating] <- length(correctRatings[correctRatings == rating]) / length(correctRatings)

                # Print average proportion of each rating in both the
                # predicted ratings and the actual ratings, as well as the
                # mean absolute prediction error between them.
                print("Average proportion of ratings:")
                print("     For predicted ratings:")
                print(ratingPredMeans)
                print("     For actual ratings:")
                print(actualMeans)
                print("MAPE value between the above 2 proportions:")
                print(mean(abs(ratingPredMeans - actualMeans)))

                # Print accuracy percentage
                ratingPredMax <- matrix(nrow = nrow(newXs), ncol = maxRating, byrow = TRUE)
                for (i in 1:nrow(newXs)) {
                        max <- max(ratingPredMat[i,])
                        ratingPredMax[i,] <- ratingPredMat[i,] == max
                }
                numCorrect <- 0
                for (j in 1:length(correctRatings)) {
                        if (ratingPredMax[j,correctRatings[j]] == TRUE)
                                numCorrect <- numCorrect + 1
                }
                print("Percentage of accurate predictions:")
                print(get_accuracy(ratingPredMat, maxRating, newXs, correctRatings))
                probResults <- rbind(ratingPredMeans, actualMeans)
                #write.csv(probResults, "kNN_test.result", row.names = FALSE)
                #write.csv(ratingPredMat, "kNN_test.result", row.names = FALSE)
        }
}

test_kNN2 <- function(dataIn, k, maxRating, newXs, correctRatings)
{
        if (nrow(newXs) != length(correctRatings)) {
                print("Different lengths of prediction entries and target ratings")
                return(-1)
        } else {
                ratingPredMat <- find_kNN2(dataIn, k, maxRating, newXs)
                #print(ratingPredMat)
                ratingPredMeans <- colMeans(ratingPredMat)
                actualMeans <- vector(mode = "numeric", length = maxRating)
                for (rating in 1:maxRating)
                        actualMeans[rating] <- length(correctRatings[correctRatings == rating]) / length(correctRatings)

                # Print average proportion of each rating in both the
                # predicted ratings and the actual ratings, as well as the
                # mean absolute prediction error between them.
                print("Average proportion of ratings:")
                print("     For predicted ratings:")
                print(ratingPredMeans)
                print("     For actual ratings:")
                print(actualMeans)
                print("MAPE value between the above 2 proportions:")
                print(mean(abs(ratingPredMeans - actualMeans)))

                # Print accuracy percentage
                ratingPredMax <- matrix(nrow = nrow(newXs), ncol = maxRating, byrow = TRUE)
                for (i in 1:nrow(newXs)) {
                        max <- max(ratingPredMat[i,])
                        ratingPredMax[i,] <- ratingPredMat[i,] == max
                }
                numCorrect <- 0
                for (j in 1:length(correctRatings)) {
                        if (ratingPredMax[j,correctRatings[j]] == TRUE)
                                numCorrect <- numCorrect + 1
                }
                print("Percentage of accurate predictions:")
                print(get_accuracy(ratingPredMat, maxRating, newXs, correctRatings))
                probResults <- rbind(ratingPredMeans, actualMeans)
                write.csv(probResults, "kNN_k5_validation.result", row.names = FALSE)
                write.csv(ratingPredMat, "kNN_k5_validation.mat", row.names = FALSE)
        }
}

dataToMatrixtmp <- function(dataIn) {
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
