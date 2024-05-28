library(FuzzyNumbers)

library(FuzzySimRes)

set.seed(123456)

# prepare matrix related to control chart data

controlChartMatrix <- FuzzyNumbersToMatrix(controlChartData,trapezoidal = FALSE)

head(controlChartMatrix)

# matrix to indicate missing values (TRUE)

maskMatrix <- matrix(FALSE,nrow = nrow(controlChartMatrix),ncol = ncol(controlChartMatrix))

# matrix with NAs

controlChartMatrixNA <- controlChartMatrix


# adding NAs to data (20% in each column)

for (i in 1:ncol(controlChartMatrix)) {
  
  rowsToImputation <- sample(nrow(controlChartMatrix),ceiling(nrow(controlChartMatrix)*0.2))
  
  # insert TRUE for NA and NA in the given place
  
  maskMatrix[rowsToImputation,i] <- TRUE
  
  controlChartMatrixNA[rowsToImputation,i] <- NA
  
}


# let's check outputs

head(maskMatrix,n=15)

head(controlChartMatrixNA,n=15)


# now we can impute values

controlChartMatrixImputed <- ImputationDMethod(controlChartMatrixNA,maskMatrix)

head(controlChartMatrixImputed,n=15)


# now we can find errors

ErrorMatrix(controlChartMatrix,controlChartMatrixImputed,maskMatrix)

# calculate statistical measures

StatisticalMeasures(controlChartMatrix,controlChartMatrixImputed,maskMatrix)

# which rows are with proper fuzzy numbers?

trueFNRowsNumbers <- which(apply(controlChartMatrixImputed, MARGIN=1,
                                 FUN=IsFuzzy, trapezoidal=FALSE) == TRUE)


# calculate distances for fuzzy numbers

CalculateFuzzyMeasures(controlChartMatrix[trueFNRowsNumbers,],
                       controlChartMatrixImputed[trueFNRowsNumbers,],
                       maskMatrix[trueFNRowsNumbers,])

# calculate p-values for statistical tests

ApplyStatisticalTests(controlChartMatrix[trueFNRowsNumbers,],
                      controlChartMatrixImputed[trueFNRowsNumbers,],
                      maskMatrix[trueFNRowsNumbers,],cutsNumber=100,K=100)
