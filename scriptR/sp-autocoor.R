## Calcul a moran index for each cell of a grid
## Take in input spatial point and grid. This script produce in output a grid with for each cell the moran index

# Auteur Etienne DELAY (GEOLAB UMR 6042 Limoges)
# source : http://www.ats.ucla.edu/stat/mult_pkg/faq/general/spatial_autocorr.htm

## Cancel env variable
rm(list = ls())

## defined working directory
setwd(dir = "~/github/QRMoranIndex/")

## load packages
require(rgdal)
require(sp)
require(ape)#for moran index



points.shp <- readOGR(dsn = "./data_demo/points/", layer = "pos_lob0dist50p")
grid.shp <- readOGR(dsn = "./data_demo/grid/", layer = "grid001")


proj4string(points.shp) <- proj4string(grid.shp)


mi.v <- NULL
for(i in 0:(length(grid.shp@data$ID)-1)){
  small.grid <- grid.shp[grid.shp@data$ID == i,]
  
  
  a <- over(points.shp, small.grid)
  b <- !is.na(a$ID)
  
  small.pts <- points.shp[b,]
#   plot(small.grid)
#   plot(small.pts, add = T)
  
  if(length(small.pts@data$WHO) != 0 & length(small.pts@data$WHO) > 20){
    ##MORAN INDEX ####
    ## Distance matrix on my point selection
    pts.dists <- as.matrix(dist(cbind(small.pts@coords[,1],small.pts@coords[,2])))
    #work on inverse distance
    pts.dists.inv <- 1/pts.dists
    diag(pts.dists.inv) <- 0
    
    #pts.dists.inv[1:5, 1:5]
    #sum(colSums(indiv.dists.inv == 0)) ##offset 1020 ##centred 1103
    
    
    pts.dists.inv[is.infinite(pts.dists.inv)] <- 0
    #sum(colSums(indiv.dists.inv == 0)) ##offset 1028 ##centred 1157
    
    m.i <-Moran.I(small.pts@data$HEATREQUIR, pts.dists.inv)[[4]]
    
  }else{
    m.i <- 0
  }
  mi.v <- c(mi.v, m.i)
}

grid.shp@data$Moran.I <- mi.v

mean(mi.v)

spplot(grid.shp, "Moran.I", main = "Moran")

#writeOGR(grid.shp, dsn = "./data_demo/", layer = "moranGrid", driver = "ESRI Shapefile")

##MANTEL test ####
require(ade4)

ma.v <- NULL
for(i in 0:(length(grid.shp@data$ID)-1)){
  small.grid <- grid.shp[grid.shp@data$ID == i,]
  
  
  a <- over(points.shp, small.grid)
  b <- !is.na(a$ID)
  
  small.pts <- points.shp[b,]
#   plot(small.grid, main = i)
#   plot(small.pts, add = T)
  
  if(length(small.pts@data$WHO) != 0 & length(small.pts@data$WHO) > 10){
    ##MORAN INDEX ####
    ## Distance matrix on my point selection
    pts.dists <- dist(cbind(small.pts@coords[,1],small.pts@coords[,2]))
    
    val.dist <- dist(small.pts@data$HEATREQUIR)
    as.matrix(val.dist)
    
    
    mantel.i <-mantel.rtest(val.dist, pts.dists)[[4]]
    
  }else{
    mantel.i <- 0
  }
  ma.v <- c(ma.v, mantel.i)
}

grid.shp@data$Mantel.I <- ma.v

mean(ma.v)

spplot(grid.shp, "Mantel.I", main = "Mantel")
