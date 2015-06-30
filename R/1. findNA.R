santarosa <- read.csv("D:/Dropbox/Marianela Iturriaga/data/santarosa.csv", sep=";", dec=",")
#Import data

View(santarosa)

#Esta funcion retorna un integer(0) cuando no encuentra valores nulos,
#o un Data.Frame con todos los valores nulos, identificados por sus posiciones
#i,j
findNA <- function (x) {
  
  data <- data.frame(i=integer(),j=integer(),stringsAsFactors=FALSE)
  count <- 0
  for (i in 1:nrow(x)) {
    vector <- which(is.na(x[i,]))
    if (any(vector)) {
      #El vector tiene valores!!!
      for (j in 1:length(vector)) { #Recorrer el vector!!
        #cat("[" , i , "," , vector[j] , "]\n")
        data[nrow(data)+1,1] <- i
        data[nrow(data),2] <- vector[j]
      }
      count <- count + length(vector)
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

NA_values<-findNA(santarosa)

if (any(NA_values)) {
  #Retorno un Data.Frame
  santarosa <- removeRows(NA_values, santarosa)
}

View(santarosa)