##calcule de l'incide de moran pour Graziella

rm(list = ls())

library(rgdal) #chargement des données SIG
library(raster) #manipulation de raster
library(adehabitat)##pour transformer en raster les points
library(tcltk) ##pour une interface graphique
library(stringr)
library(ggplot2)

myfile <- tk_choose.files(default = "", caption = "Select files", multi = TRUE, filters = NULL, index = 1)

both.moran.df <- NULL
for(f in 1:length(myfile)){
  position <-max(gregexpr(pattern ='/',myfile[f])[[1]])
  
  point.shp <- readOGR(dsn = str_sub(myfile[f], 1,position), layer = str_sub(myfile[f],(position+1), -5))
  
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
  size.m$type <- str_sub(myfile[f],(position+1), -5)
  
  both.moran.df <- rbind(both.moran.df, size.m)

}

################## GGPLOT ##########################

#### DEFINITION DU THEME GGPLOT####
ggtheme<-theme(strip.text.x = element_text(size=14),
               strip.text.y = element_text(size=14),
               legend.title = element_text(size=16),
               legend.text = element_text(size=15),
               axis.title.x = element_text(face="bold", size=12),
               axis.title.y = element_text(face="bold", size=12),
               axis.text = element_text(size=14)
               # legend.position="none"
)

ggplot(data = both.moran.df)+
  geom_point(aes(x = i, y = moran, colour = type))+
  geom_smooth(aes(x = i, y = moran, colour = type))+
  scale_colour_manual(values = c("#969696","#252525"),
                    name = "Simulations",
                    breaks=c("pos_lob_centrer0","pos_lob0dist50p"),
                    labels=c("Centred", "offset 100m"))+
  labs(x = "tiles numbers", y = "Moran")
  ggtheme



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
