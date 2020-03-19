library('caret')

source('NMF.R')

crossValidate <- function(trn, vld, tst,maxRating, predMethod, embedMeans, index1) {
	# Create 10 folds of the training/test data
	numFolds <- 1
	# folds <- vector('list',length = numFolds)
	# trn <- vector('list',length = numFolds)
	# tst <- vector('list',length = numFolds)

	# vldIdx <- createUsableFold(data, 10000)
	# vld <- data[vldIdx,]
	# trnTst <- data[-vldIdx,]
	# folds[[i]] <- createUsableFold(trnTst,10000)
	# # Create the 10 training/test sets by holding one fold out for test in each set.
	# for ( i in 1:numFolds ) {
	# 	trn[[i]] <- trnTst[-folds[[i]],]
	# 	tst[[i]] <- trnTst[folds[[i]],]
	# }

	ranks <- c(1,10,25,50,75,100,125,150,175,200,250,500)
	performance <- matrix(nrow = length(ranks), ncol = numFolds)
	# This is where you loop over your hyper parameters
	for( i in 1:length(ranks)){
		# Loop over the folds
		for (j in 1:numFolds) {
			out <- ratingProbsFit(trn, maxRating, predMethod, embedMeans, list(rank = ranks[i], index1 = index1))
			preds <- NMFPredict(out , vld)
			performance[i,j] <- measurePerformance(preds, vld[,3])
		}
	}
	#Find the best performing rank
	avgPerformance <- apply(performance, 1, mean)
	bestRank <- ranks[which.max(avgPerformance)]
	print("The best performing rank was:")
	print(bestRank)

	#Find the true performance using the full Training and Test data and the Validation data to measure performance
	trnVld <- rbind(trn,vld)
	out <- ratingProbsFit(trnVld, maxRating, predMethod, embedMeans, list(rank = bestRank))
	preds <- predict(out, tst)
	truePerformance <- measurePerformance(preds, tst[,3])
	print("The true performance was:")
	print(truePerformance)
	return(performance)
}

# createUsableFold <- function(data, size) {
# 	count <- 0
# 	while( TRUE ) {
# 		count <- count + 1 
# 		print(count)
# 		fold <- sample(1:nrow(data), size)
# 		trn <- data[-fold,]
# 		tst <- data[fold,]
# 		# See if there are any values in the test set that isnt in the training
# 		diffUsr <- setdiff(tst[,1], trn[,1])
# 		diffItm <- setdiff(tst[,2], trn[,2])
# 		if ( length(diffUsr) == 0 && length(diffItm) == 0 ) {
# 			browser()
# 			return(fold)
# 		}
# 	}
# }