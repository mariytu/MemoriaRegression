#*******************************FUNCTIONS*******************************#
#Function to normalize dataset in range of 0 to 1
normalize <- function(x){
  (x - min(x, na.rm=TRUE))/(max(x,na.rm=TRUE) - min(x, na.rm=TRUE))
}

#*******************************MAIN PROGRAM*******************************#
#load("Santarosa.Rda") #If you made the first step (1. FindMissingValues)
load("D:/Dropbox/Marianela Iturriaga/data/Santarosa.Rda") #Absolute path from my computer

head(lapply(santarosa, range)) #Checking columns range (min & max)

#the two forst columns are removed because they are not continous.
#thefirst column has the number of the city in which they sample was taken
#the second column is a correlative (nominal) value that does not add information for the PCA process
subsetSantarosa <- subset(santarosa, select = X350:Rdto..gr.parc.) #Subset without 2 first columns

#now we normalize the data, storing it in a data frame
normedSubset <- as.data.frame(lapply(subsetSantarosa, normalize)) #Subset normalized

#show the ranges just to be sure the ranges are correctly modified
head(lapply(normedSubset, range)) #Checking columns range between 0 to 1

#we copy the two first columns back into the data frame, just to maintain the original information
twoColumns <- subset(santarosa, select = REPETICION:N.Parcela..Longitud.de.onda) #First two columns of original Dataset
santarosaNormalized <- data.frame(twoColumns, normedSubset) #All dataset Normalized

View(santarosaNormalized)

#save the data into a binary object in the hard disk
save(santarosaNormalized,file = "SantarosaNormalized.Rda") #Save object in your Documents folder
