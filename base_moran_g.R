##calcule de l'incide de moran pour Graziella

rm(list = ls())

library(rgdal) #chargement des données SIG
library(raster) #manipulation de raster
library(adehabitat)##pour transformer en raster les points
library(tcltk2) ##pour une interface graphique
library(stringr)

myfile <- tk_choose.files(default = "", caption = "Select files", multi = TRUE, filters = NULL, index = 1)

position <-max(gregexpr(pattern ='/',myfile)[[1]])

point.shp <- readOGR(dsn = str_sub(myfile, 1,position), layer = str_sub(myfile,(position+1), -5))

myxy <- as.data.frame(point.shp@coords)##Je récupère les coordonée

size.m <- data.frame()
for(i in 5:50){
  r  <-  ascgen(xy=myxy, nrcol = i)
  r <- raster(r)
  moran.z <- Moran(r)
  size.m <- rbind(size.m,c(i,moran.z))
}
size.m <- as.data.frame(size.m)
colnames(size.m)<-c("i","moran")

plot(x = size.m$i, y = size.m$moran)

r  <-  ascgen(xy=myxy, nrcol = 20)
r <- raster(r)
plot(r)

###Ecriture du raster
writeRaster(r, "~/Téléchargements/raster.tif")

r  <-  ascgen(xy=myxy, nrcol = 40)
r <- raster(r)
plot(r)

writeRaster(r, "~/Téléchargements/raster40.tif")

Moran(r) #this is the global index of autocorrelation
x1  <-  MoranLocal(r) #local measure of autocorr as a raster object that can be plotted
plot(x1) #this will plot the autocorrelation raster results

##20 = 25 m & 40 = 12
