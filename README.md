# ecs189g termproject
 ECS 189G Term Project - Winter 2020
 
# Overview

All quarter, we've been predicting item ratings, with output like "Our predicted rating for user 88 and movie 220 is 3.72." But it would be nice to report probabilities instead, making statements like, "For user 88 and movie 220, the probabilities of ratings 1, 2, 3, 4 and 5 are 0.102, 0.468, 0.057, 0.111 and 0.262, respectively." We'll do that here!

# Details

You will write a function with call form ratingProbsFit(dataIn,maxRating,predMethod,embedMeans,specialArgs) where the arguments are as follows:

dataIn: Data frame, with first 2 columns being R factors for user ID and item ID, and then a numerical rating. The 3 columns must be named 'userID', 'itemID' and 'rating'.
maxRating: Top rating. Scores are assumed to range from 1, consecutively through this value.
predMethod: One of 'logit', 'NMF', 'kNN' and 'CART'.
embedMeans: If TRUE, replace user and item IDs by means, as in our revised book. Not valid if predMethod is 'NMF', but mandatory if it is 'CART'.
specialArgs: An R list, with named elements, specifying method-specific arguments to be used, if any, e.g. rank for NMF.
The return value will be of class recProbs, S3. It will contain whatever information is needed for predict.recProbs().

You will write a function predict.recProbs()with call form
predict(probsFitOut,newXs)
where the arguments are as follows:
probsFitOut: Output of ratingProbsFit().
newXs: A data frame of new (user,item) combinations to be predicted. No new users or items will be allowed. Columns must be named 'userID' and 'itemID'.
Note that this will be a case of R's generic functions.

The return value will be a matrix, with maxRating columns and nrow(newXs) rows.
Other than the above specs, this project is open-ended. For any given predMethod, there are various possible ways you might estimate the rating probabilities.
Note, though, that the fundamental relation that will probably guide your thinking is E(IA) = P(A), where IA is a random variable equal to 1 or 0, depending on whether the event A occurs or not. In sample terms, the average of a dummy variable is the proportion of time it is equal to 1.
You will compare the accuracies of the various types of predMethod on two datasets, which you will choose from this list:
Czech Dating Agency
Harvard/MIT MOOCs (maybe take grade as the "rating")
InstEval
Song List
Stanford Network Datasets treat an edge as s "Like" (warning: very large)
Your report must explain in detail your design decisions, and how you implemented them.

