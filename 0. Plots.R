#*******************************FUNCTIONS*******************************#
Spectral <- function (santarosaNormalized, row) {
  y <- santarosaNormalized[row,]
  y <- y[,3:(ncol(y)-1)]
  
  dataPlot <- data.frame(x=integer(), y=integer(), stringsAsFactors=FALSE)
  count <- 0
  for (i in 1:ncol(y)) {
    dataPlot[nrow(dataPlot)+1,1] <- i
    dataPlot[nrow(dataPlot),2] <- y[1,i]
  }
  row <- santarosaNormalized[row,2]
  ggplot(data = dataPlot, aes(x = x, y = y, group = 1)) +
    geom_line(colour = "dodgerblue4", alpha = 0.8, size = 1) +
    xlab(paste("Instance", row)) + ylab("Values") +
    theme(panel.grid.minor = element_blank(), #remove gridlines
          legend.position = "bottom" #legend at the bottom
    )#end theme
}

Plot1Col <- function (santarosaNormalized, col) {
  x <- santarosaNormalized[,col]
  Performance <- santarosaNormalized$Rdto..gr.parc.
  
  dataPlot <- data.frame(x, Performance)
  
  ggplot(data = dataPlot, aes(x = x, y = Performance, group = 1)) +
    geom_point(aes(colour = Performance), na.rm = TRUE) + 
    scale_color_gradientn(colours = c("darkred", "yellow", "darkgreen")) + #set the pallete
    xlab(names(santarosaNormalized)[col]) +
    ylab("Performance") +
    theme(panel.grid.minor = element_blank(), #remove gridlines
          legend.position = "bottom" #legend at the bottom
    )#end theme
}

#SOURCE:
#http://stackoverflow.com/questions/2397097/how-can-a-data-ellipse-be-superimposed-on-a-ggplot2-scatterplot
PlotWithEllipse <- function (santarosaNormalized, col) {
  x <- santarosaNormalized[,col]
  y <- santarosaNormalized$Rdto..gr.parc.
  
  df <- data.frame(x, y, group="A")
  
  #calculating ellipses
  df_ell <- data.frame()
  for(g in levels(df$group)){
    df_ell <- rbind(df_ell, cbind(as.data.frame(with(df[df$group==g,], ellipse(cor(x, y), 
              scale = c(sd(x), sd(y)),
              centre = c(mean(x), mean(y))))), group = g))
  }
  
  #drawing
  ggplot(data = df, aes(x = x, y = y, colour = group)) + 
    geom_point(colour = "#A4C100", size = 2, alpha = 0.8) +
    geom_path(data = df_ell, aes(x = x, y = y, colour = group), size = 1, linetype = 2,
              colour = "darkgreen") +
    xlab(names(santarosaNormalized)[col]) +
    ylab("Performance") +
    theme(panel.grid.minor = element_blank(), #remove gridlines
          legend.position = "bottom" #legend at the bottom
    )#end theme
}

#*******************************MAIN PROGRAM*******************************#
#load("SantarosaNormalized.Rda") #If you made the second or fourth step (2. NormalizedDataset)
load("D:/Dropbox/Marianela Iturriaga/data/SantarosaNormalized.Rda") #Absolute path from my computer
santarosa <- read.csv("D:/Dropbox/Marianela Iturriaga/data/santarosa.csv", sep=";", dec=",") #Absolute path from my computer

library(ggplot2)
#install.packages("ellipse")
library(ellipse)

Spectral(santarosa, 701) #Instancia 701
Spectral(santarosa, 141) #Instancia 141

Plot1Col(santarosaNormalized, 50) #X397
Plot1Col(santarosaNormalized, 700) #X1047
Plot1Col(santarosaNormalized, 1153) #X1500
Plot1Col(santarosaNormalized, 1620) #X1967
Plot1Col(santarosaNormalized, 1500) #X1847

PlotWithEllipse(santarosaNormalized, 1500) #X1847
