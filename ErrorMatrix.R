ErrorMatrix <- function(trueData,imputedData,imputedMask)
{
  variableNumber <- ncol(trueData)
  
  obsNumber <- nrow(trueData)
  
  # matrix for errors
  
  output <- matrix(data = 0,nrow = length(errorTypes), ncol = variableNumber+1)
  
  rownames(output) <- errorTypes
  
  colnames(output) <- c(noquote(paste("X", 1:variableNumber, sep="")),"mean")
  
  # main loop
  
  for (i in 1:variableNumber) {
    
    # which rows are with some NAs?
    
    rowsWithNA <- which(imputedMask[,i])
    
    # let's calculate errors
    
    output["MAE",i] <- mean(abs(trueData[rowsWithNA,i]-imputedData[rowsWithNA,i]))
    
    output["WMAE",i] <- mean(abs((trueData[rowsWithNA,i]-imputedData[rowsWithNA,i])/
                                         ChangeDenominator(trueData[rowsWithNA,i])))
    
    output["MSE",i] <- mean((trueData[rowsWithNA,i]-imputedData[rowsWithNA,i])^2)
    
    output["WMSE",i] <- mean(((trueData[rowsWithNA,i]-imputedData[rowsWithNA,i])/
                                      ChangeDenominator(trueData[rowsWithNA,i]))^2)
    
    output["NMRSE",i] <- sqrt(mean((trueData[rowsWithNA,i]-imputedData[rowsWithNA,i])^2)/(max(trueData[,i])-min(trueData[,i])))
    
  }
  
  # calculate means for errors
  
  output[,variableNumber+1] <- apply(output[,-(variableNumber+1)],1,mean)
  
  
  return(output)
  
  
}











# auxiliary function if the denominator is equal to zero

ChangeDenominator <- function(inputVector)
{
  whereToChange <- (inputVector==0)
  
  output <- inputVector
  
  output[whereToChange] <- 1
  
  return(output)
  
}
