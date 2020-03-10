ratingProbsFit <- function(dataIn,maxRating,predMethod,embedMeans,specialArgs){
	# Check for errors where using or not using embeded means with an incompatible method.
	if( embedMeans and predMethod == "NMF" ) return NaN
	if( !embedMeans and predMethod == "CART") return NaN

	# If using embeded means replace the data with embeded data.
	if( embedMeans ) dataIn <- embedDataMeans(dataIn)

	# Call the proper predition method.
	if( predMethod == "logit" ) return Logit(dataIn,maxRating,embedMeans,specialArgs)
	if( predMethod == "NMF" ) return NMF(dataIn,maxRating,specialArgs)
	if( predMethod == "kNN" ) return KNN(dataIn,maxRating,embedMeans,specialArgs)
	if( predMethod == "CART" ) return CART(dataIn,maxRating,embedMeans,specialArgs)
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
