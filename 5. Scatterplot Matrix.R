#This code was obtained from https://gastonsanchez.wordpress.com/2012/08/27/scatterplot-matrices-with-ggplot/
#We just made some minor changes

#*******************************FUNCTIONS*******************************#
makePairs <- function(data){
  grid <- expand.grid(x = 1:ncol(data), y = 1:ncol(data))
  grid <- subset(grid, x != y)
  all <- do.call("rbind", lapply(1:nrow(grid), function(i) {
    xcol <- grid[i, "x"]
    ycol <- grid[i, "y"]
    data.frame(xvar = names(data)[ycol], yvar = names(data)[xcol], 
               x = data[, xcol], y = data[, ycol], data)
  }))
  all$xvar <- factor(all$xvar, levels = names(data))
  all$yvar <- factor(all$yvar, levels = names(data))
  densities <- do.call("rbind", lapply(1:ncol(data), function(i) {
    data.frame(xvar = names(data)[i], yvar = names(data)[i], x = data[, i])
  }))
  list(all = all, densities = densities)
}

#*******************************MAIN PROGRAM*******************************#
#load("SantarosaAllPCA.Rda") #If you made the third or fourth step (3. PCA)
load("D:/Dropbox/Marianela Iturriaga/data/SantarosaAllPCA.Rda") #Absolute path from my computer
#load("SantarosaNormalized.Rda") #If you made the second or fourth step (2. NormalizedDataset)
load("D:/Dropbox/Marianela Iturriaga/data/SantarosaNormalized.Rda") #Absolute path from my computer

# expand data frame for pairs plot
PCA1to3 <- as.data.frame(santarosa.pca$x[,1:3])
gg1 = makePairs(PCA1to3)
performance <- santarosaNormalized[,2154]

#New data frame mega PCA 1..3
mega_PCA = data.frame(gg1$all, Performance = rep(performance, length = nrow(gg1$all)))
Performance = rep(performance, length = nrow(gg1$all))

# pairs plot
ggplot(mega_PCA, aes_string(x = "x", y = "y")) + 
  facet_grid(xvar ~ yvar, scales = "free") + 
  geom_point(aes(colour=Performance), na.rm = TRUE, alpha=0.5, size=1) + 
  stat_density(aes(x = x, y = ..scaled.. * diff(range(x)) + min(x)), 
               data = gg1$densities, position = "identity", 
               colour = "dodgerblue4", geom = "line", size=1, alpha=0.5) + 
  scale_color_gradientn(colours = c("darkred", "yellow", "darkgreen")) + #set the pallete
  #theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(), #remove gridlines
  theme(panel.grid.minor=element_blank(), #remove gridlines
        legend.position="bottom", #legend at the bottom
        axis.title.x = element_blank(), #remove x label
        axis.title.y = element_blank()  #remove y label
        )#end theme