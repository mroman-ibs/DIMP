StatisticalMeasures <- function(trueData,imputedData,imputedMask)
{
  variableNumber <- ncol(trueData)
  
  obsNumber <- nrow(trueData)
  
  # matrix for measures
  
  output <- matrix(data = 0,nrow = length(measuresTypes), ncol = variableNumber)
  
  rownames(output) <- measuresTypes
  
  colnames(output) <- noquote(paste("X", 1:variableNumber, sep=""))
  
  # mail loop
  
  for (i in 1:variableNumber) {
    
    # which rows are with some NAs?
    
    rowsWithNA <- which(imputedMask[,i])
    
    
    # measures only for NA/imputed data
    
    output["TrueMean",i] <- mean(trueData[rowsWithNA,i])
    
    output["ImpMean",i] <- mean(imputedData[rowsWithNA,i])
    
    output["TrueSD",i] <- sd(trueData[rowsWithNA,i])
    
    output["ImpSD",i] <- sd(imputedData[rowsWithNA,i])
    
  }
  
  # measures for all data
  
  output["GenMean",] <- apply(trueData,MARGIN = 2,FUN = mean)
  
  output["GenImpMean",] <- apply(imputedData,MARGIN = 2,FUN = mean)
  
  output["GenSD",] <- apply(trueData,MARGIN = 2,FUN = sd)
  
  output["GenImpSD",] <- apply(imputedData,MARGIN = 2,FUN = sd)
  
  # some differences
  
  output["AbsDiffTrueImpMean",] <- abs(output["TrueMean",]-output["ImpMean",])
  
  output["AbsDiffGenImpMean",] <- abs(output["GenMean",]-output["GenImpMean",])
  
  output["AbsDiffTrueImpSD",] <- abs(output["TrueSD",]-output["ImpSD",])
  
  output["AbsDiffGenImpSD",] <- abs(output["GenSD",]-output["GenImpSD",])
  
  
  return(output)
  
  
  
}
