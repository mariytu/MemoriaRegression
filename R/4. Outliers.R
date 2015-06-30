findOutliers <- function (x, col, value) {
  
  data <- data.frame(i=integer(),stringsAsFactors=FALSE)
  count <- 0
  for (i in 1:nrow(x)) {
    if (x[i,col]<value) {
      data[nrow(data)+1,1] <- i
      count <- count + 1
    }
  }
  
  if (count==0) {
    return (integer(0))
  }
  else {
    return (data)
  }
}

removeRows <- function (x,dataSet) {
  
  for (i in 1:nrow(x)) {
    dataSet <- dataSet[-x[i,1],]
  }
  
  return (dataSet)
}

outliers<-findOutliers(santarosa.pca$x,1,-600)

if (any(NA_values)) {
  #Retorno un Data.Frame
  normed <- removeRows(outliers, normed)
}

View(normed)