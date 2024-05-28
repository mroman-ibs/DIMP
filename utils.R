# check if the initial value is correct fuzzy number

IsFuzzy <- function(fuzzyNumber,trapezoidal)
{
  
  if(trapezoidal)
  {
    
    if((fuzzyNumber[1] <= fuzzyNumber[2]) & (fuzzyNumber[2] <= fuzzyNumber[3]) & (fuzzyNumber[3] <= fuzzyNumber[4]))
    {
      
      return(TRUE)
      
    } else {
      
      return(FALSE)
      
    }
    
  } else {
    
    if((fuzzyNumber[1] <= fuzzyNumber[2]) & (fuzzyNumber[2] <= fuzzyNumber[3]))
    {
      
      return(TRUE)
      
    } else {
      
      return(FALSE)
      
    }
    
    
  }
  
  
  
  
  
}
