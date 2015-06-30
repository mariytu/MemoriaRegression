normalize <- function(x){
  (x - min(x, na.rm=TRUE))/(max(x,na.rm=TRUE) - min(x, na.rm=TRUE))
}
#Funcion para normalizar los datos
head(lapply(santarosa, range))
#Revisando el rango (min y max de las columnas)

subsetSantarosa <- subset(santarosa, select = X350:Rdto..gr.parc.)
#Crea un subconjunto de los datos
#para omitir las 2 primeras columnas!!!

normedSubset <- as.data.frame(lapply(subsetSantarosa, normalize))
#Normalizando el subconjunto
head(lapply(subsetSantarosa, range))
#Verificando que los datos quedaron con un rango entre 0 y 1

twoColumns <- subset(santarosa, select = REPETICION:N.Parcela..Longitud.de.onda)
normed <- data.frame(twoColumns, normedSubset)
#Conjunto completo
View(normed)