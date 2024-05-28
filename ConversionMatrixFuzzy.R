# conversion of matrix to list of fuzzy numbers

MatrixToFuzzyNumbers <- function(macierzFuzzy, varNames=NA)
{
  if(nrow(macierzFuzzy)==0)
  {
    print("Not enough rows!")
    
  }
  
  
  output <- list(rep(NA,nrow(macierzFuzzy)))
  
  if(ncol(macierzFuzzy)==3)
  {
    for (i in 1:nrow(macierzFuzzy)) {
      
      output[[i]] <- TriangularFuzzyNumber(a1=macierzFuzzy[i,1],amid = macierzFuzzy[i,2],a4 = macierzFuzzy[i,3])
      
      
    }
    
    
  }
  
  if(ncol(macierzFuzzy)==4)
  {
    for (i in 1:nrow(macierzFuzzy)) {
      
      output[[i]] <- TrapezoidalFuzzyNumber(a1=macierzFuzzy[i,1],a2 = macierzFuzzy[i,2],
                                            a3 = macierzFuzzy[i,3],a4 = macierzFuzzy[i,4])
      
      
    }
  
  }
  
  if(!anyNA(varNames))
  {
    names(output) <- varNames
  }
  
  return(output)
  
}

# conversion of list of fuzzy numbers to matrix

FuzzyNumbersToMatrix <- function(listaFuzzy,trapezoidal=TRUE)
{
  numberOfFN <- length(listaFuzzy)
  
  if(trapezoidal)
  {
    
    output <- matrix(NA,nrow = numberOfFN,ncol = 4)
    
  } else
    
  {
    output <- matrix(NA,nrow = numberOfFN,ncol = 3)
  }
  
  for (i in 1:numberOfFN) {
    
    output[i,1] <- supp(listaFuzzy[[i]])[1]
    
    output[i,2] <- core(listaFuzzy[[i]])[1]
    
    if(trapezoidal)
    {
      output[i,3] <- core(listaFuzzy[[i]])[2]
      
      output[i,4] <- supp(listaFuzzy[[i]])[2]
      
    } else {
      
      output[i,3] <- supp(listaFuzzy[[i]])[2]
      
    }
    
  }
  
  return(output)
  
}
