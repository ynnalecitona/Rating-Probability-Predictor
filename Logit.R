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
Logit <- function(dataIn, maxRating, specialArgs) {
  
  # Parameter Description
  # - dataIn: Data set with userID, itemID, Additional Predictors, Dummy Ratings
  # - maxRating: Maximum possible numerical rating that an item can be given
  # - specialArgs: Additional predictor to be used in the generation of the model
  
  # Verify that the dataIn dataframe is valid
  if(names(dataIn)[1] != "userID" | names(dataIn)[2] != "itemID") {
    write("logit: dataIn missing userID or itemID", stderr())
    return(status = -1)
  }
  
  # Verify that specialArgs are valid and format the specialArgs if necessary
  if(length(specialArgs) != 0) {
    i = 1
    validArgRange <- 3:(ncol(dataIn) - maxRating)
    for(element in specialArgs) {
      if(is.integer(element) & !(element %in% validArgRange)) {
        write("logit: specialArg in invalid range", stderr())
        return(status = -1)
      }
      if(is.character(element) & (!(element %in% names(dataIn)) | element == "userID" | element == "itemID" | element %in% names(dataIn)[(ncol(dataIn) - maxRating):(ncol(dataIn))])) {
        write("logit: specialArg is not an additional predictor", stderr())
        return(status = -1)
      }
      if(!(is.character(element) | is.integer(element))) {
        write("logit: specialArg is an invalid type", stderr())
        return(status = -1)
      }
      if(is.character(element)) {
        
        # Replace the string representation of the specialArg with its respective column index representation
        specialArgs[i] <- as.integer(match(element, names(dataIn)))
      }
      i = i + 1
    }

    # Trim the unsued predictors from the dataframe		
    dataIn <- dataIn[,c(1,2,as.integer(specialArgs),(ncol(dataIn) - maxRating):(ncol(dataIn)))]
  }

  # Declare the return object that will hold the linear models
  dataOut <- list(predMethod = "logit", maxRating = maxRating)
  
  # One vs. All approach to generate a binomial linear model for each possible rating
  for(i in 1:maxRating) {
    
    print("REACHED CHECKPOINT START OF FOR LOOP. ITER:")
    print(i)
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
  print(maxRatings)
  print(typeof(maxRatings))
  dataOut <- matrix(0, nrow = nrow(newXs), ncol = maxRatings)
  for(i in 1:maxRatings) {
    
      print("REACHED CHECKPOINT IN FOR LOOP 1 of PREDICTION. ITER: ")
      print(i)
      
      predictions <- predict.glm(probsFitOut[[2+i]], newXs, type = 'response')
      dataOut[,i] <- predictions
      names(dataOut)[i] <- paste("p",toString(i), sep = "")
  }
  
  return(dataOut)
}



testInstEval2 <- function() {
  
  data(InstEval)
  maxRating <- 5
  dataIn <- ratingToDummy(InstEval[1:10000,c(1,2,7)], maxRating)
  names(dataIn)[1] <- "userID"
  names(dataIn)[2] <- "itemID"
  dataIn$userID <- as.factor(dataIn$userID)
  dataIn$itemID <- as.factor(dataIn$itemID)
  dataIn <- na.omit(dataIn)
  
  train <- sample(1:nrow(dataIn), floor(0.95 * nrow(dataIn)), replace = FALSE)
  test <- setdiff(1:nrow(dataIn), train)

  n <- floor(0.95 * nrow(dataIn))
  cond <- FALSE
  
  numTries <- 0
  
  while(!cond) {
    
    numTries <- numTries + 1  
    trainIndx <- sample(nrow(dataIn), n)
    testIndx <- setdiff(1:nrow(dataIn), trainIndx)
    cond <- TRUE
    for(factor in names(dataIn)[1:2]) {
      print(factor)
      trainLevels <- dataIn[trainIndx, factor]
      testLevels <- dataIn[testIndx, factor]
      
      if(!all(testLevels %in% trainLevels)) {
        cat('Factor level: ', factor, ' violated constraint, retrying.\n')
        cond <- FALSE
      }
    }
  }
  print(numTries)
  
  print(head(dataIn[trainIndx,]))
  print(head(dataIn[testIndx,]))
  
  write.table(dataIn[trainIndx,], file = "TrimmedTrain.data")
  write.table(dataIn[testIndx,], file = "TrimmedTest.data")

    # Generate the probability predictions
  
  print("REACHED CHECKPOINT 1")
  
  #glmout <- Logit(dataIn[trainIndx,], maxRating, c())
  
  print("REACHED CHECKPOINT AFTER GLMOUT")
  
  #p <- prediction(glmout, dataIn[testIndx,])
  
  print("REACHED CHECKPOINT AFTER PREDICT")
  
  # Save the output to a file
  #write.table(p, file = "LogitPredictions.data")
  
}

testInstEval <- function() {
  
  # Read in the train and testing data set for InstEval
  train <- read.csv("train.data")
  test <- read.csv("test.data")
  
  # Format the data
  maxRating <- 5
  names(train) <- c('userID', 'itemID', 'rating')
  names(test) <- c('userID', 'itemID', 'rating')
  
  train$userID <- factor(train$userID)
  train$itemID <- factor(train$itemID)
  test$userID <- factor(test$userID)
  test$itemID <- factor(test$itemID)
  
  test <- test[,c(1,2)]
  train <- ratingToDummy(train, maxRating)
  
  # Generate the probability predictions
  
  print("REACHED CHECKPOINT 1")
  
  glmout <- Logit(train, maxRating, c())
  
  print("REACHED CHECKPOINT AFTER GLMOUT")
  
  p <- prediction(glmout, test)
  
  print("REACHED CHECKPOINT AFTER PREDICT")
  
  # Save the output to a file
  write.table(p, file = "LogitPredictions.data")
  
  
}

testInstEval3 <- function() {
  
  train <- read.table("TrimmedTrain.data")
  test <- read.table("TrimmedTest.data")
  print(head(train))
  print(head(test))
  
  train$userID <- factor(train$userID)
  train$itemID <- factor(train$itemID)
  test$userID <- factor(test$userID)
  test$itemID <- factor(test$itemID)
  maxRating <- 5
  test <- test[,c(1,2)]
  
  print("REACHED CHECKPOINT 1")
  
  glmout <- Logit(train, maxRating, c())
  
  print("REACHED CHECKPOINT AFTER GLMOUT")
  
  p <- prediction(glmout, test)
  
  print("REACHED CHECKPOINT AFTER PREDICT")
  
  # Save the output to a file
  write.table(p, file = "LogitPredictions.data")  
}

#testInstEval3()
p <- read.table("LogitPredictions.data")
print(sum(p))
print(nrow(p))