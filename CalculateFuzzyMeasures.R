# calculate all fuzzy distances for the pairs of fuzzy numbers

CalculateFuzzyMeasures <- function(fuzzyMatrix1,fuzzyMatrix2,imputedMask)
{
  # create vector for output
  
  output <- rep(0,length(distanceNames))
  
  names(output) <- distanceNames
  
  if(ncol(fuzzyMatrix1)==3)
  {
    
    trapezoidal=FALSE
    
  } else {
    
    trapezoidal=TRUE
    
  }
  
  # choose only imputed values
  
  rowsNumbersImputed <- which(apply(imputedMask,1,any))
  
  # cat("rowsNumbersImputed: ", rowsNumbersImputed, "\n")
  
  fuzzyMatrix1Imp <- fuzzyMatrix1[rowsNumbersImputed,]
  
  # print(fuzzyMatrix1Imp)
  
  fuzzyMatrix2Imp <- fuzzyMatrix2[rowsNumbersImputed,]
  
  # print(fuzzyMatrix2Imp)
  
  for (i in 1:nrow(fuzzyMatrix1Imp)) {
    
    # cat("i: ", i , "\n")
    
    output["Euclidean"] <-  output["Euclidean"] + EuclideanMetric(fuzzyMatrix1Imp[i,],fuzzyMatrix2Imp[i,],trapezoidal = trapezoidal)
    
    output["AHD"] <-  output["AHD"] + MeasureAHD(fuzzyMatrix1Imp[i,],fuzzyMatrix2Imp[i,],trapezoidal = trapezoidal)
    
    output["HSD"] <-  output["HSD"] + MeasureHSD(fuzzyMatrix1Imp[i,],fuzzyMatrix2Imp[i,],trapezoidal = trapezoidal)
    
    # print(output)
    
  }
  
  output <- output / nrow(fuzzyMatrix1Imp)
  
  return(output)
  
  
}
