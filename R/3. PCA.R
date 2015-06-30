#PCA
subsetSantarosa <- normed[,3:2153]
#3 es la columna X350, 2153 es la columna X2500
resp <- normed[,2154]
#Columna con los resultados

santarosa.pca <- prcomp(subsetSantarosa,
                 center = TRUE,
                 scale. = TRUE)

print(santarosa.pca)
plot(santarosa.pca, type = "l")

santarosa.pca #El pca1 es el más importante por los vectores propios

summary(santarosa.pca)
predict(santarosa.pca,newdata=tail(subsetSantarosa, 2))

install.packages("devtools")
library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)

library("RColorBrewer")
myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))

g <- ggbiplot(santarosa.pca, choices=c(1,2), obs.scale = 1, var.scale = 1, 
              groups = resp, ellipse = TRUE, 
              circle = TRUE)
g <- g + scale_colour_gradientn(colours = myPalette(100))
g <- g + scale_color_discrete(name = '') #NOO porque es solo para var. discretas
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)
