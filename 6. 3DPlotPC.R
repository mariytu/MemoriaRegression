#SOURCE: http://stackoverflow.com/questions/10413678/how-to-assign-color-scale-to-a-variable-in-a-3d-scatter-plot

#*******************************FUNCTIONS*******************************#
myColorRamp <- function(colors, values) {
  v <- (values - min(values))/diff(range(values))
  x <- colorRamp(colors)(v)
  rgb(x[,1], x[,2], x[,3], maxColorValue = 255)
}

#*******************************MAIN PROGRAM*******************************#
#load("SantarosaAllPCA.Rda") #If you made the third or fourth step (3. PCA)
load("D:/Dropbox/Marianela Iturriaga/data/SantarosaAllPCA.Rda") #Absolute path from my computer
#load("SantarosaNormalized.Rda") #If you made the second or fourth step (2. NormalizedDataset)
load("D:/Dropbox/Marianela Iturriaga/data/SantarosaNormalized.Rda") #Absolute path from my computer

library(rgl)
PCA1to3<-as.data.frame(santarosa.pca$x[,1:3])
performance <- santarosaNormalized[,2154]
PC1 <- PCA1to3[,1]
PC2 <- PCA1to3[,2]
PC3 <- PCA1to3[,3]

cols <- myColorRamp(c("darkred", "yellow", "darkgreen"), performance) 
plot3d(x = PC1, y = PC2, z = PC3, col = cols, size = "4")
