#*******************************FUNCTIONS*******************************#
#Function to normalize dataset in range of 0 to 1
normalize <- function(x){
  (x - min(x, na.rm=TRUE))/(max(x,na.rm=TRUE) - min(x, na.rm=TRUE))
}


#*******************************MAIN PROGRAM*******************************#
#load("Santarosa.Rda") #If you made the first step (1. FindMissingValues)
load("D:/Dropbox/Marianela Iturriaga/data/Santarosa.Rda") #Absolute path from my computer

head(lapply(santarosa, range)) #Checking columns range (min & max)

subsetSantarosa <- subset(santarosa, select = X350:Rdto..gr.parc.) #Subset without 2 first columns
normedSubset <- as.data.frame(lapply(subsetSantarosa, normalize)) #Subset normalized

head(lapply(subsetSantarosa, range)) #Checking columns range between 0 to 1

twoColumns <- subset(santarosa, select = REPETICION:N.Parcela..Longitud.de.onda) #First two columns of original Dataset
santarosaNormalized <- data.frame(twoColumns, normedSubset) #All dataset Normalized

View(santarosaNormalized)

save(santarosaNormalized,file="SantarosaNormalized.Rda") #Save object in your Documents folder