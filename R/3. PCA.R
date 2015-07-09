#*******************************FUNCTIONS*******************************#
CalculateVariance <- function(x) {
  for (i in 1:nrow(x)) {
    x[i,2] <- x[i,2]*x[i,2]
  }
  return (x)
}

#*******************************MAIN PROGRAM*******************************#
#load("SantarosaNormalized.Rda") #If you made the second step (2. NormalizedDataset)
load("D:/Dropbox/Marianela Iturriaga/data/SantarosaNormalized.Rda") #Absolute path from my computer

#Preparing to PCA
subsetSantarosa <- santarosaNormalized[,3:2153] #The third column is the first independent variable
#and the last one is the column number 2153
performance <- santarosaNormalized[,2154] #The column 2154 is the dependent variable

#Apply PCA Algorithm
santarosa.pca <- prcomp(subsetSantarosa, center = TRUE, scale. = TRUE)

print(santarosa.pca)
summary(santarosa.pca)
predict(santarosa.pca, newdata = tail(subsetSantarosa, 2))

plot(santarosa.pca, type = "l", main = NULL) #Plot of first 10 PCA

#Alternative plot using ggplot
install.packages("devtools")
library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)

dataPlot <- data.frame(santarosaNormalized$N.Parcela..Longitud.de.onda, santarosa.pca$sdev)
names(dataPlot) <- c("PCA", "Variances")
dataPlot <- dataPlot[1:10,]
dataPlot <- CalculateVariance(dataPlot)

#Plot PCA1/PCA2
PCA1to2 <- data.frame(santarosa.pca$x[,1:2], performance)
names(PCA1to2) <- c("PC1", "PC2", "Performance")

ggplot(PCA1to2, aes_string(x = "PC1", y = "PC2")) + 
  geom_point(aes(colour=Performance), na.rm = TRUE, alpha=0.8, size=2) + 
  scale_color_gradientn(colours = c("darkred", "yellow", "darkgreen")) + #set the pallete
  theme(panel.grid.minor=element_blank(), #remove gridlines
        legend.position="bottom" #legend at the bottom
  )#end theme

save(santarosa.pca,file="SantarosaAllPCA.Rda") #Save object in your Documents folder