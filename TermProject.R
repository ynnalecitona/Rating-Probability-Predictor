ratingProbsFit <- function(dataIn,maxRating,predMethod,embedMeans,specialArgs){
	# Check for errors where using or not using embeded means with an incompatible method.
	if( embedMeans and predMethod == "NMF" ) return NaN
	if( !embedMeans and predMethod == "CART") return NaN
	
	# If using embeded means replace the data with embeded data.
	if( embedMeans ) dataIn <- embedDataMeans(dataIn)
	
	# Call the proper predition method.
	if( predMethod == "logit" ) return Logit(dataIn,maxRating,predMethod,embedMeans,specialArgs)
	if( predMethod == "NMF" ) return NMF(dataIn,maxRating,predMethod,embedMeans,specialArgs)
	if( predMethod == "kNN" ) return KNN(dataIn,maxRating,predMethod,embedMeans,specialArgs)
	if( predMethod == "CART" ) return CART(dataIn,maxRating,predMethod,embedMeans,specialArgs)
}

predict <- function(probsFitOut,newXs) {
	
}

embedDataMeans <- function(dataIn) {
	
}

Logit <- function(dataIn,maxRating,predMethod,embedMeans,specialArgs) {
	
}

NMF <- function(dataIn,maxRating,predMethod,embedMeans,specialArgs) {
	
}

KNN <-- function(dataIn,maxRating,predMethod,embedMeans,specialArgs) {
	
}

CART <- function(dataIn,maxRating,predMethod,embedMeans,specialArgs) {
	
}