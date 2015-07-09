#*******************************FUNCTIONS*******************************#
#This function return an integer(0) when no null values,
#or a Data.Frame with all null values identified by their positions (i,j) 
findNA <- function (x) {
  
  data <- data.frame(i=integer(), j=integer(), stringsAsFactors=FALSE)
  count <- 0
  for (i in 1:nrow(x)) {
    vector <- which(is.na(x[i,]))
    if (any(vector)) {
      #The vector has null values!!!
      for (j in 1:length(vector)) { #Recorrer el vector!!
        #cat("[" , i , "," , vector[j] , "]\n") #Show row and column of missing value
        data[nrow(data)+1,1] <- i
        data[nrow(data),2] <- vector[j]
      }
      count <- count + length(vector)
    }
  }
  
  if (count == 0) {
    return (integer(0))
  }
  else {
    return (data)
  }
}

#This function remove all rows that contains a missing value
#You must pass a Data.Frame (x) for identify all missing values and original DataSet
#This function return a new DataSet without missing values
removeRows <- function (missing, dataSet) {
  
  for (i in 1:nrow(missing)) {
    dataSet <- dataSet[-missing[i,1],] #Remove row
  }
  
  return (dataSet)
}

#*******************************MAIN PROGRAM*******************************#
#Import data
#santarosa <- read.csv("D:/Dropbox/Marianela Iturriaga/data/santarosa.csv", sep=";", dec=",") #Absolute path from my computer
santarosa <- read.csv("https://dl.dropboxusercontent.com/u/12599702/santarosa.csv", sep=";", dec=",") #Path from URL

View(santarosa)
NA_values <- findNA(santarosa)

if (any(NA_values)) { #Validation if missing values exist
  santarosa <- removeRows(NA_values, santarosa)
}

View(santarosa)

save(santarosa,file="Santarosa.Rda") #Save object in your Documents folder