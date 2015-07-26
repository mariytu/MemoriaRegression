#REV: http://www.statmethods.net/stats/rdiagnostics.html

#*******************************FUNCTIONS*******************************#

diagnosticData <- function(santarosa.pca, performance) {
  diagnostic <- data.frame(santarosa.pca$x[,1:3], performance)
  names(diagnostic) <- c("PC1", "PC2", "PC3", "Performance")
  resid <- resid(fit)
  diagnostic <- data.frame(diagnostic, resid)
  stz.r <- rstandard(fit)
  diagnostic <- data.frame(diagnostic, stz.r)
  stu.r <- rstudent(fit)
  diagnostic <- data.frame(diagnostic, stu.r)
  cooks <- cooks.distance(fit)
  diagnostic <- data.frame(diagnostic, cooks)
  dfbeta <- dfbeta(fit)
  diagnostic <- data.frame(diagnostic, dfbeta)
  dffit <- dffits(fit)
  diagnostic <- data.frame(diagnostic, dffit)
  leverage <- hatvalues(fit)
  diagnostic <- data.frame(diagnostic, leverage)
  cov.rat <- covratio(fit)
  diagnostic <- data.frame(diagnostic, cov.rat)
  fitted <- fitted(fit)
  diagnostic <- data.frame(diagnostic, fitted)
  sqrt.abs.stz.r <- sqrt(abs(diagnostic$stz.r))
  diagnostic <- data.frame(diagnostic, sqrt.abs.stz.r)
  
  return (diagnostic)
}

ResidualsFitted <- function(diagnostic) {
  ggplot(diagnostic, aes(fitted, resid)) +
    geom_hline(yintercept = 0, colour = "grey50", size = 0.5, linetype="dashed") +
    geom_point(aes(colour = Performance), na.rm = TRUE) + 
    scale_color_gradientn(colours = c("darkred", "yellow", "darkgreen")) + #set the pallete
    geom_smooth(method = "auto", size = 0.7, se = F, colour = "#299E98") +
    xlab("Fitted Values") +
    ylab("Residuals") +
    theme(legend.position = "bottom" #legend at the bottom
    )#end theme
}

StResidualsFitted <- function(diagnostic) {
  ggplot(diagnostic, aes(fitted, sqrt.abs.stz.r)) +
    geom_point(aes(colour = Performance), na.rm = TRUE) + 
    scale_color_gradientn(colours = c("darkred", "yellow", "darkgreen")) + #set the pallete
    geom_smooth(method = "auto", size = 0.7, se = F, colour = "#299E98") +
    xlab("Fitted Values") +
    ylab(expression(sqrt("|Standarized Residuals|"))) +
    theme(legend.position = "bottom" #legend at the bottom
    )#end theme
}

NormalQQ <- function(diagnostic) {
  #SOURCE: http://librestats.com/2012/06/11/autoplot-graphical-methods-with-ggplot2/
  
  a <- quantile(diagnostic$stz.r, c(0.25, 0.75))
  b <- qnorm(c(0.25, 0.75))
  slope <- diff(a)/diff(b)
  int <- a[1] - slope * b[1]
  
  ggplot(diagnostic, aes(sample = diagnostic$stz.r)) +
    stat_qq() +
    geom_abline(slope = slope, intercept = int, colour = "#299E98", linetype="dashed") +
    scale_x_continuous("Theoretical Quantiles") +
    scale_y_continuous("Standardized Residuals")
}

StResidualsLeverange <- function(diagnostic) {
  ggplot(diagnostic, aes(leverage, stz.r)) +
    geom_hline(yintercept = 0, colour = "grey50", size = 0.5, linetype="dashed") +
    geom_point(aes(colour = Performance), na.rm = TRUE) + 
    scale_color_gradientn(colours = c("darkred", "yellow", "darkgreen")) + #set the pallete
    geom_smooth(method = "auto", size = 0.7, se = F, colour = "#299E98") +
    xlab("Leverange") +
    ylab("Standarized Residuals") +
    theme(legend.position = "bottom" #legend at the bottom
    )#end theme
}

findRow <- function (delete, dataSet) {
  
  for (i in 1:nrow(dataSet)) {
    if ((dataSet[i,2]==delete)[1])
    {
      return (i)
    }
  }
  return (0)
}

removeRows <- function (delete, dataSet) {
  
  for (i in 1:ncol(delete)) {
    row <- findRow(delete[i], dataSet)
    dataSet <- dataSet[-row,]
  }
  
  return (dataSet)
}

#*******************************PACKAGES*******************************#
#install.packages("Rcmdr")
#install.packages("car")
#install.packages("QuantPsyc")
#install.packages("stringi")

library(boot)
library(car)
library(MASS)
library(QuantPsyc)
library(Rcmdr)
library(ggbiplot)

#SOURCE: http://www.statmethods.net/stats/regression.html
#*******************************MAIN PROGRAM*******************************#
#load("SantarosaAllPCA.Rda") #If you made the third or fourth step (3. PCA)
load("D:/Dropbox/Marianela Iturriaga/data/SantarosaAllPCA.Rda") #Absolute path from my computer
#load("SantarosaNormalized.Rda") #If you made the second or fourth step (2. NormalizedDataset)
load("D:/Dropbox/Marianela Iturriaga/data/SantarosaNormalized.Rda") #Absolute path from my computer
#load("Santarosa.Rda") #If you made the first step (1. FindMissingValues)
load("D:/Dropbox/Marianela Iturriaga/data/Santarosa.Rda") #Absolute path from my computer

#Regression
PCA1to3 <- as.data.frame(santarosa.pca$x[,1:3])
performance <- santarosaNormalized[,2154]

PC1 <- PCA1to3[,1]
PC2 <- PCA1to3[,2]
PC3 <- PCA1to3[,3]

fit <- lm(performance ~ PC1 + PC2 + PC3, data = PCA1to3)

summary(fit)

# diagnostic plots 
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(fit)

#Alternative plots using ggplot
diagnostic <- diagnosticData(santarosa.pca, performance)
ResidualsFitted(diagnostic)
StResidualsFitted(diagnostic)
NormalQQ(diagnostic)
StResidualsLeverange(diagnostic)

delete <- data.frame(407,777,800)

santarosa <- removeRows(delete, santarosa)

save(santarosa,file = "Santarosa.Rda") #Save object in your Documents folder
