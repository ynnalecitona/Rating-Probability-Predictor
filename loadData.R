loadData <- function(filename) {
	data <- read.csv(filename)
	colnames(data) <- c('userID', 'itemID', 'rating')
	return(data) 
}