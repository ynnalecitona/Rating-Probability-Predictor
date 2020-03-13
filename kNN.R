cosineSim <- function(trainData)

# trainData should be in a data.table form
# specialArgs contains k (i.e. number of nearest neighbours)
KNN <- function(trainData, maxRating, embedMeans, specialArgs)
{

        #nearestNeighbours <- cosineSim(trainData, specialArgs)
        #manhattanSim()
        #euclideanSim()
        #chebychevSim()
        #jaccardSim()
        #rbfkSim()
        #dotProdSim()
        #correlSim()
        #embedMeans()
}

get_vlen <- function(v) sqrt(v %*% v)

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

                targetUserIdx <- which(rownames(dataIn) == targetUserID)
                targetItemIdx <- which(colnames(dataIn) == targetItemID)

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

                        sortOrder <- sort(simVec, decreasing = TRUE, na.last = TRUE, index.return = TRUE)
                        nearestNeighbours <- potentialUsers[sortOrder$ix,]
                        # TODO: Check if potentialUsers < k
                        kNN <- nearestNeighbours[1:k,targetItemIdx]

                        ratings <- vector(mode = "numeric", length = maxRating)
                        for (rating in 1:maxRating)
                                ratings[rating] = length(kNN[kNN == rating]) / k

                        print(ratingPredMat[i,])
                        ratingPredMat[i,] <- ratings
                        #find similarity of each potential user to targetUser
                        #reorder potential user list in descending order, based on similarity
                        #get first k potential users
                        #create rating percentages
                }
        }
        return(ratingPredMat)
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
