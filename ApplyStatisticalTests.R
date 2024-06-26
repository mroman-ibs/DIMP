ApplyStatisticalTests <- function(trueData,imputedData,imputedMask,cutsNumber,K)
{
  # let's divide sample to subsamples
  
  rowsNumbersImputed <- which(apply(imputedMask,1,any))
  
  dataWithoutChanges <- trueData[-rowsNumbersImputed,]
  
  dataFromImputation <- imputedData[rowsNumbersImputed,]
  
  dataBeforeImputation <- trueData[rowsNumbersImputed,]
  
  # conversion to fuzzy numbers
  
  dataWithoutChangesFuzzy <- MatrixToFuzzyNumbers(dataWithoutChanges)
  
  dataFromImputationFuzzy <- MatrixToFuzzyNumbers(dataFromImputation)
  
  dataBeforeImputationFuzzy <- MatrixToFuzzyNumbers(dataBeforeImputation)
  
  # matrix for the results
  
  output <- matrix(0,nrow = length(setsNames),ncol = length(testsNames),dimnames = list(setsNames,testsNames))
  
  # now we apply statistical tests
  
  output["true","avs-anti"] <- AverageStatisticEpistemicTest(dataWithoutChangesFuzzy,dataBeforeImputationFuzzy,bootstrapMethod = "anti",cutsNumber = cutsNumber)
  
  output["true","ms-anti"] <- MultiStatisticEpistemicTest(dataWithoutChangesFuzzy,dataBeforeImputationFuzzy,bootstrapMethod = "anti",cutsNumber = cutsNumber,combineMethod = "mean")
  
  output["true","res-anti"] <- ResamplingStatisticEpistemicTest(dataWithoutChangesFuzzy,dataBeforeImputationFuzzy,bootstrapMethod = "anti",cutsNumber = cutsNumber,K=K,combineMethod = "mean")
  
   output["imputed","avs-anti"] <- AverageStatisticEpistemicTest(dataWithoutChangesFuzzy,dataFromImputationFuzzy,bootstrapMethod = "anti",cutsNumber = cutsNumber)
  
  output["imputed","ms-anti"] <- MultiStatisticEpistemicTest(dataWithoutChangesFuzzy,dataFromImputationFuzzy,bootstrapMethod = "anti",cutsNumber = cutsNumber,combineMethod = "mean")
  
  output["imputed","res-anti"] <- ResamplingStatisticEpistemicTest(dataWithoutChangesFuzzy,dataFromImputationFuzzy,bootstrapMethod = "anti",cutsNumber = cutsNumber,K=K,combineMethod = "mean")
  
  output["parts","avs-anti"] <- AverageStatisticEpistemicTest(dataBeforeImputationFuzzy,dataFromImputationFuzzy,bootstrapMethod = "anti",cutsNumber = cutsNumber)
  
  output["parts","ms-anti"] <- MultiStatisticEpistemicTest(dataBeforeImputationFuzzy,dataFromImputationFuzzy,bootstrapMethod = "anti",cutsNumber = cutsNumber,combineMethod = "mean")
  
  output["parts","res-anti"] <- ResamplingStatisticEpistemicTest(dataBeforeImputationFuzzy,dataFromImputationFuzzy,bootstrapMethod = "anti",cutsNumber = cutsNumber,K=K,combineMethod = "mean")
  
  
  return(output)
  
}
