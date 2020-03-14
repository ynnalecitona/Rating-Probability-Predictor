cosineSim <- function(trainData)

# trainData should be in a data.table form
# specialArgs contains k (i.e. number of nearest neighbours)
KNN <- function(trainData, maxRating, embedMeans, specialArgs)
{
        #embedMeans()
}

get_vlen <- function(v) sqrt(v %*% v)

#--------------------Radial Basis Function Kernel similarity--------------------

rbfkSim <- function(potentialUser, targetUser)
{
        commItemsIdx <- !is.na(potentialUser) & !is.na(targetUser)
        if (sum(commItemsIdx) == 0) return(NaN)

        xvec <- potentialUser[commItemsIdx]
        yvec <- targetUser[commItemsIdx]
        sigma <- 1
        exp(-1 * (get_vlen(xvec - yvec) / (2 * (sigma ^ 2))))
}

#-----------------------------Correlation similarity-----------------------------

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

#-----------------------------Dot Product similarity-----------------------------

dotProdSim <- function(potentialUser, targetUser)
{
        commItemsIdx <- !is.na(potentialUser) & !is.na(targetUser)
        if (sum(commItemsIdx) == 0) return(NaN)

        xvec <- potentialUser[commItemsIdx]
        yvec <- targetUser[commItemsIdx]
        (xvec %*% yvec)
}

#-------------------------------Jaccard similarity-------------------------------

jaccardSim <- function(potentialUser, targetUser)
{
        # Get non-NA elements
        xvec <- potentialUser[!is.na(potentialUser)]
        yvec <- targetUser[!is.na(targetUser)]
        length(intersect(xvec, yvec)) / length(union(xvec, yvec))
}

#-------------------------------Chebychev similarity-------------------------------

chebychevSim <- function(potentialUser, targetUser)
{
        commItemsIdx <- !is.na(potentialUser) & !is.na(targetUser)
        if (sum(commItemsIdx) == 0) return(NaN)

        xvec <- potentialUser[commItemsIdx]
        yvec <- targetUser[commItemsIdx]
        # Once power by infinity, the max element will overtake all other ones.
        max(xvec - yvec)
}

#-------------------------------Euclidean similarity-------------------------------

euclideanSim <- function(potentialUser, targetUser)
{
        commItemsIdx <- !is.na(potentialUser) & !is.na(targetUser)
        if (sum(commItemsIdx) == 0) return(NaN)

        xvec <- potentialUser[commItemsIdx]
        yvec <- targetUser[commItemsIdx]
        get_vlen(xvec - yvec)
}

#-------------------------------Manhattan similarity-------------------------------

manhattanSim <- function(potentialUser, targetUser)
{
        commItemsIdx <- !is.na(potentialUser) & !is.na(targetUser)
        if (sum(commItemsIdx) == 0) return(NaN)

        xvec <- potentialUser[commItemsIdx]
        yvec <- targetUser[commItemsIdx]
        sum(xvec - yvec)
}

#-------------------------------Cosine similarity-------------------------------

cosineSim <- function(potentialUser, targetUser)
{
        # 1 & to compare each element of both vectors
        commItemsIdx <- !is.na(potentialUser) & !is.na(targetUser)
        if (sum(commItemsIdx) == 0) return(NaN)

        xvec <- potentialUser[commItemsIdx]
        yvec <- targetUser[commItemsIdx]
        (xvec %*% yvec) / (get_vlen(xvec) * get_vlen(yvec))
}

# Assumptions:
#       1. newXs doesn't contain any new users or items
#       2. ratings have consecutive integer values, ranging from 1 to maxRating.
find_kNN <- function(dataIn, k, maxRating, newXs)
{
        # byrow = TRUE allows us to replace a matrix's row with a vector
        ratingPredMat <- matrix(nrow = nrow(newXs), ncol = maxRating, byrow = TRUE)
        for (i in 1:nrow(newXs)) {
                oneUser <- newXs[i,]
                targetUserID <- oneUser$userID
                targetItemID <- oneUser$itemID
                #print(c(targetUserID, targetItemID))

                targetUserIdx <- which(rownames(dataIn) == targetUserID)
                targetItemIdx <- which(colnames(dataIn) == targetItemID)
                #print(c(targetUserIdx, targetItemIdx))

                if (!is.na(dataIn[targetUserIdx, targetItemIdx])) {
                        ratings <- vector(mode = "integer", length = maxRating)
                        ratings[dataIn[targetUserIdx, targetItemIdx]] = 1
                        ratingPredMat[i,] = ratings
                } else {
                        potentialUsersIdx <- which(!is.na(dataIn[,targetItemIdx]))
                        potentialUsers <- dataIn[potentialUsersIdx,]
                        targetUser <- dataIn[targetUserIdx,]
                        simVec <- vector(mode = "numeric", length = nrow(potentialUsers))
                        for (j in 1:nrow(potentialUsers))
                                simVec[j] <- cosineSim(potentialUsers[j,], targetUser)

                        # Might consider having na.last = TRUE
                        sortOrder <- sort(simVec, decreasing = FALSE, na.last = NA, index.return = TRUE)
                        if (length(sortOrder$ix) == 0) {
                                ratingPredMat[i,] <- vector(mode = "numeric", length = maxRating)
                        } else {
                                nearestNeighbours <- potentialUsers[sortOrder$ix,]
                                # If don't have k nearest neighbours, use as many as there are

                                if (length(sortOrder$ix) == 1) {
                                        kNN <- nearestNeighbours[targetItemIdx]
                                } else {
                                        numNN <- min(k, nrow(nearestNeighbours))
                                        kNN <- nearestNeighbours[1:numNN,targetItemIdx]
                                }
                                # print(kNN)
                                ratings <- vector(mode = "numeric", length = maxRating)
                                for (rating in 1:maxRating)
                                        ratings[rating] = length(kNN[kNN == rating]) / numNN
                                ratingPredMat[i,] <- ratings
                        }
                        #find similarity of each potential user to targetUser
                        #reorder potential user list in descending order, based on similarity
                        #get first k potential users
                        #create rating percentages
                }
        }
        return(ratingPredMat)
}

test_kNN <- function(dataIn, k, maxRating, newXs, correctRatings)
{
        if (nrow(newXs) != length(correctRatings)) {
                print("Different lengths of prediction entries and target ratings")
                return(-1)
        } else {
                ratingPredMat <- find_kNN(dataIn, k, maxRating, newXs)
                ratingPredMeans <- colMeans(ratingPredMat)
                actualMeans <- vector(mode = "numeric", length = maxRating)
                for (rating in 1:maxRating) {
                        actualMeans[rating] <- length(correctRatings[correctRatings == rating]) / length(correctRatings)
                }
                print(ratingPredMeans)
                print(actualMeans)
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
