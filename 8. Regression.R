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

#Function to normalize dataset in range of 0 to 1
normalize <- function(x){
  (x - min(x, na.rm=TRUE))/(max(x,na.rm=TRUE) - min(x, na.rm=TRUE))
}

normedInstance <- function(X, instance){
  
  for (j in 1:ncol(X))
  {
    instance[1,j] <- (instance[1,j] - min(X[,j], na.rm = TRUE))/(max(X[,j], na.rm = TRUE) - min(X[,j], na.rm = TRUE))
  }
  
  return (instance)
}

#SOURCE: http://www.statmethods.net/stats/regression.html
#*******************************MAIN PROGRAM*******************************#
#load("Santarosa.Rda") #If you made the first step (1. FindMissingValues)
load("D:/Dropbox/Marianela Iturriaga/data/Santarosa.Rda") #Absolute path from my computer

#Normalizacion
subsetSantarosa <- subset(santarosa, select = X350:Rdto..gr.parc.) #Subset without 2 first columns
#now we normalize the data, storing it in a data frame
normedSubset <- as.data.frame(lapply(subsetSantarosa, normalize)) #Subset normalized
#we copy the two first columns back into the data frame, just to maintain the original information
twoColumns <- subset(santarosa, select = REPETICION:N.Parcela..Longitud.de.onda) #First two columns of original Dataset
santarosaNormalized <- data.frame(twoColumns, normedSubset) #All dataset Normalized

#Reduccion de la Dimensionalidad
subsetSantarosa <- santarosaNormalized[,3:2153] #The third column is the first independent variable
#and the last one is the column number 2153
performance <- santarosaNormalized[,2154] #The column 2154 is the dependent variable
#Apply PCA Algorithm
santarosa.pca <- prcomp(subsetSantarosa, center = TRUE, scale. = TRUE)

library(ggbiplot)
linePlot(santarosa.pca, santarosaNormalized)

#Regression
PCA1to3 <- as.data.frame(santarosa.pca$x[,1:3])
performance <- santarosaNormalized[,2154]

PC1 <- PCA1to3[,1]
PC2 <- PCA1to3[,2]
PC3 <- PCA1to3[,3]

fit <- lm(performance ~ PC1 + PC2 + PC3, data = PCA1to3)

summary(fit)

#This provide a better insight into the 'importance' of a predictor in the model
#bigger absolute value = more important
lm.beta(fit)

#Confidence intervals for model parameters
confint(fit)

# Other useful functions
coefficients(fit) # model coefficients
fitted(fit) # predicted values
residuals(fit) # residuals
anova(fit) # anova table
vcov(fit) # covariance matrix for model parameters
influence(fit) # regression diagnostics

#Comparing models
PCA1to5 <- as.data.frame(santarosa.pca$x[,1:5])
performance <- santarosaNormalized[,2154]
PC1 <- PCA1to5[,1]
PC2 <- PCA1to5[,2]
PC3 <- PCA1to5[,3]
PC4 <- PCA1to5[,4]
PC5 <- PCA1to5[,5]
fit3 <- lm(performance ~ PC1 + PC2 + PC3)
fit5 <- lm(performance ~ PC1 + PC2 + PC3 + PC4 + PC5)
anova(fit3, fit5)

#CROSS VALIDATION
# K-fold cross-validation
#install.packages("DAAG")
library(lattice)
library(DAAG)
layout(matrix(c(1),2,2))
par(mar = c(5.1,4.1,4.1,2.1))
cv.lm(df = PCA1to3, fit, m = 10) # 10 fold cross-validation

#Our model to predict any observation
model <- function (santarosa, instance, santarosa.pca, fit) {
  subsetSantarosa <- subset(santarosa, select = X350:Rdto..gr.parc.)
  
  instance <- normedInstance(subsetSantarosa, instance)
  instanceSubset <- instance[,1:2151]
  instancePerformance <- instance[,2152]
  
  #We can now calculate the position of every instance in the transformed space
  instance.pca <- predict(santarosa.pca, newdata = instanceSubset)
  
  predicted = fit$coefficients[1]
  predicted = predicted + fit$coefficients[2]*instance.pca[1]
  predicted = predicted + fit$coefficients[3]*instance.pca[2]
  predicted = predicted + fit$coefficients[4]*instance.pca[3]
  
  x <- santarosa[,2154]
  predicted <- predicted * (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)) + min(x, na.rm = TRUE)
  
  return (predicted)
}

#We try to predict some value
row <- 757
instance <- santarosa[row,]
instance <- instance[,-1]
instance <- instance[,-1]
predictedValue <- model(santarosa, instance, santarosa.pca, fit)

predictedValue
santarosa[row,2154]
