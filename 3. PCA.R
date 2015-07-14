#*******************************FUNCTIONS*******************************#
CalculateVariance <- function(x) {
  for (i in 1:nrow(x)) {
    x[i,2] <- x[i,2]*x[i,2]
  }
  return (x)
}

#LinePlot using ggplot to plot variances of 10 first PC
linePlot <- function(santarosa.pca, santarosaNormalized) {
  dataPlot <- data.frame(santarosaNormalized$N.Parcela..Longitud.de.onda, santarosa.pca$sdev)
  names(dataPlot) <- c("PCA", "Variances")
  dataPlot <- dataPlot[1:10,]
  dataPlot <- CalculateVariance(dataPlot)
  
  ggplot(data = dataPlot, aes(x = PCA, y = Variances, group = 1)) +
    geom_line(colour = "dodgerblue4", alpha = 0.5, size = 1) +
    geom_point(colour = "dodgerblue4", size = 2, alpha = 0.5) +
    expand_limits(y = 0) +
    xlab("PCs") + ylab("Variances") +
    scale_x_continuous(breaks = dataPlot$PCA) +
    theme(panel.grid.minor = element_blank(), #remove gridlines
          legend.position = "bottom" #legend at the bottom
    )#end theme
}

#Function to plot 2 PC. You must indicate which PC you want in the graph.
plotPC <- function(santarosa.pca, performance, x_axis, y_axis) {
  PCs <- data.frame(santarosa.pca$x[,x_axis], santarosa.pca$x[,y_axis], performance)
  x_axis <- paste(c("PC", x_axis), collapse = "")
  y_axis <- paste(c("PC", y_axis), collapse = "")
  names(PCs) <- c(x_axis, y_axis, "Performance")
  
  ggplot(PCs, aes_string(x = x_axis, y = y_axis)) + 
    geom_point(aes(colour = Performance), na.rm = TRUE, alpha = 0.8, size = 2) + 
    scale_color_gradientn(colours = c("darkred", "yellow", "darkgreen")) + #set the pallete
    theme(panel.grid.minor = element_blank(), #remove gridlines
          legend.position = "bottom" #legend at the bottom
    )#end theme
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
#show the eigen vector
print(santarosa.pca)
#statistical information regarding the PCA
#the accumulated variance is key here
#one method is to look for more than 85% of the cumulative deviation. that will give a rough
#estimate regarindg which is the correct number of components.
summary(santarosa.pca)
#we can now calculate the position of every instance in the transformed space
predict(santarosa.pca, newdata = tail(subsetSantarosa, 2))
#elbow method. this is an alternative method for estimating the correct number of components

plot(santarosa.pca, type = "l", main = NULL) #Plot of first 10 PCA

#Alternative plot using ggplot
#install.packages("devtools")
library(devtools)
#install_github("vqv/ggbiplot")
library(ggbiplot)
linePlot(santarosa.pca, santarosaNormalized)

#Plot PC1/PC2
plotPC(santarosa.pca, performance, x_axis = 1, y_axis = 2)

save(santarosa.pca,file = "SantarosaAllPCA.Rda") #Save object in your Documents folder