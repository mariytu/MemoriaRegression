#*******************************FUNCTIONS*******************************#
#This function receives a dataset (x), a column to check and a value to compare
#The value is compared with all values in colunm (col) and if it's less than value
#their row (i) it's add in a dataframe to be returned. If any value is found, this
#function return an integer(0)
findOutliers <- function (x, col, value) {
  data <- data.frame(i = integer(), stringsAsFactors = FALSE)
  count <- 0
  
  for (i in 1:nrow(x)) {
    if (x[i,col] < value) {
      data[nrow(data)+1,1] <- i
      count <- count + 1
    }
  }
  
  if (count == 0) {
    return (integer(0))
  }
  else {
    return (data)
  }
}

#This function remove all rows that contain an outlier
#You must pass a Data.Frame (outliers) for identify all outliers and a normalized DataSet
#This function return a new DataSet without outliers
removeRows <- function (outliers, dataSet) {
  
  for (i in 1:nrow(outliers)) {
    dataSet <- dataSet[-outliers[i,1],]
  }
  
  return (dataSet)
}

#*******************************MAIN PROGRAM*******************************#
#load("SantarosaAllPCA.Rda") #If you made the third step (3. PCA)
load("D:/Dropbox/Marianela Iturriaga/data/SantarosaAllPCA.Rda") #Absolute path from my computer
#load("Santarosa.Rda") #If you made the first step (1. FindMissingValues)
load("D:/Dropbox/Marianela Iturriaga/data/Santarosa.Rda") #Absolute path from my computer

outliers <- findOutliers(santarosa.pca$x, 1, -600)

if (any(outliers)) { #Validation if outliers exist
  santarosa <- removeRows(outliers, santarosa)
}

View(santarosa)

save(santarosa,file = "Santarosa.Rda") #Save object in your Documents folder
