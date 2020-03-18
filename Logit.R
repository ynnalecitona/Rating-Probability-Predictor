library(lme4)

# Appends the linearly dependent rating dummy matrix
ratingToDummy <- function(data, maxRatings) {
  
  # Verify that maxRating is valid
  if(!(maxRatings > 0)) {
    write("ratingToDummy: maxRating is an invalid number", stderr())
    return(status = -1)
  }
  
  # Extract the ratings from the data
  ratings <- data[,3]
  data <- data[,-3]
  
  # Add the dummy variable ratings to the end of the data
  for(i in 1:maxRatings) {
    
    # Create the name for the new column in the format "r + rating number"
    name <- paste("r", toString(i), sep = "")
    
    # Each value in the column represents a bool that is true if the user gave
    # the item the rating "i"
    data[,(ncol(data) + 1)] <- as.integer(ratings == i)
    
    # Add the name to newly created column
    names(data)[ncol(data)] <- name	
  }
  
  return(data)	
}

# Uses a binomial linear model approach to generate probabilities of possible ratings
Logit <- function(dataIn, maxRating) {
  
  # Parameter Description
  # - dataIn: Data set with userID, itemID, Additional Predictors, Dummy Ratings
  # - maxRating: Maximum possible numerical rating that an item can be given
  
  # Verify that the dataIn dataframe is valid
  if(names(dataIn)[1] != "userID" | names(dataIn)[2] != "itemID") {
    write("logit: dataIn missing userID or itemID", stderr())
    return(status = -1)
  }
  
  # Declare the return object that will hold the linear models
  dataOut <- list(predMethod = "logit", maxRating = maxRating)
  
  # One vs. All approach to generate a binomial linear model for each possible rating
  for(i in 1:maxRating) {

    # Compute the index of the response variable
    index <- (ncol(dataIn) - maxRating + i)
    
    # Create the string for the formula
    responseName <- paste("r",toString(i), sep = "")
    form <- paste(responseName, "~ userID + itemID")
    
    # Create the logit model
    glmout <- glm(formula = form, data = dataIn, family = binomial, maxit = 50)
    
    # Add the glmout objects to the return object
    columnName <- paste("p",toString(i), sep = "")
    dataOut <- append(dataOut, list(glmout))
    names(dataOut)[2 + i] <- columnName
  }
  
  class(dataOut) <- "logit"
  
  return(dataOut)
}

prediction <- function(probsFitOut, newXs) {
  
  # Generate the prediction probability for the first layer
  maxRatings <- probsFitOut[[2]]
  dataOut <- matrix(0, nrow = nrow(newXs), ncol = maxRatings)
  for(i in 1:maxRatings) {
    
    predictions <- predict.glm(probsFitOut[[2+i]], newXs, type = 'response')
    dataOut[,i] <- predictions
    colnames(dataOut)[i] <- paste("p",toString(i), sep = "")
  }
  
  return(dataOut)
}