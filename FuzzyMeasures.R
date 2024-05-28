# Euclidean metric for two fuzzy numbers

EuclideanMetric <- function(value1,value2,trapezoidal=TRUE)
{
  if(trapezoidal)
  {
    
    fuzzyNumber1 <- value1
    fuzzyNumber2 <- value2
    
  } else {
    
    fuzzyNumber1 <- c(value1[1],value1[2],value1[2],value1[3])
    fuzzyNumber2 <- c(value2[1],value2[2],value2[2],value2[3])
    
  }
  
  
  output <- 1/3 * ( (fuzzyNumber1[1]-fuzzyNumber2[1])^2 + (fuzzyNumber1[2]-fuzzyNumber2[2])^2 +
                      (fuzzyNumber1[1]-fuzzyNumber2[1])*(fuzzyNumber1[2]-fuzzyNumber2[2]) +
                      (fuzzyNumber1[3]-fuzzyNumber2[3])^2 + (fuzzyNumber1[4]-fuzzyNumber2[4])^2 +
                      (fuzzyNumber1[3]-fuzzyNumber2[3])*(fuzzyNumber1[4]-fuzzyNumber2[4]))
  
  
  return(sqrt(output))
}


# AHD measure for two fuzzy numbers


MeasureAHD <- function(value1,value2,trapezoidal=TRUE)
{
  
  if(trapezoidal)
  {
    
    fuzzyNumber1 <- value1
    fuzzyNumber2 <- value2
    
  } else {
    
    fuzzyNumber1 <- c(value1[1],value1[2],value1[2],value1[3])
    fuzzyNumber2 <- c(value2[1],value2[2],value2[2],value2[3])
    
  }
  
  output <- abs(HValue(fuzzyNumber1)- HValue(fuzzyNumber2))+
    abs(HAmbiguity(fuzzyNumber1)-HAmbiguity(fuzzyNumber2))+
    abs(HArea(fuzzyNumber1)-HArea(fuzzyNumber2))+
    HDistance(fuzzyNumber1,fuzzyNumber2)

  
  return(output/2)
  
}


# HSD measure for two fuzzy numbers

MeasureHSD <- function(value1,value2,trapezoidal=TRUE)
{
  
  if(trapezoidal)
  {
    
    fuzzyNumber1 <- value1
    fuzzyNumber2 <- value2
    
  } else {
    
    fuzzyNumber1 <- c(value1[1],value1[2],value1[2],value1[3])
    fuzzyNumber2 <- c(value2[1],value2[2],value2[2],value2[3])
    
  }
  
  
  
  output <- abs(HValue(fuzzyNumber1)- HValue(fuzzyNumber2))+
    abs(HAmbiguity(fuzzyNumber1)-HAmbiguity(fuzzyNumber2))+
    HDistance(fuzzyNumber1,fuzzyNumber2)
  
  return(output/2)
  
}


