
library(osmdata)
library(sf)
library(mapview)
library(raster)
library(tmap)

#treeH <- raster("E:/Google Drive/GIS/forest/Forest_height_2019_NAM.tif",
   #             format="GTiff")

BB <- c(-75.4958,43.0102,-75.3087,43.0941)

treeC <- crop(treeH, extent(c(-75.4958,-75.3087,43.0102,43.0941)))
#writeRaster(treeC, "E:/Google Drive/GIS/Kirkland/kirkland_height_2019.tif",
#            format="GTiff", overwrite=TRUE)


big_streets <-  opq(bbox=BB)%>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", "motorway_link", "primary_link")) %>%
  osmdata_sf()

plot(big_streets$osm_lines$geometry)


med_streets <-  opq(bbox=BB)%>%
  add_osm_feature(key = "highway", 
                  value = c("secondary", "tertiary", "secondary_link", "tertiary_link")) %>%
  osmdata_sf()


plot(med_streets$osm_lines$geometry)

small_streets <-  opq(bbox = BB)%>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street",
                            "unclassified",
                            "service", "footway", "road",
                  ))%>%
  osmdata_sf()

plot(small_streets$osm_lines$geometry)

all_streets <- opq(bbox = BB)%>%
  add_osm_feature(key = "highway", 
                  value = available_tags("highway"))%>%
  osmdata_sf()


available_tags("highway")

plot(all_streets$osm_lines$geometry)

mapview(small_streets$osm_lines$geometry)+
  mapview(med_streets$osm_lines$geometry)+
  mapview(big_streets$osm_lines$geometry)

q <-  opq(bbox = BB) %>%
  add_osm_feature(key = 'natural', value = 'water') %>%
  osmdata_sf ()

r <-  opq(bbox = BB) %>%
  add_osm_feature(key = 'waterway', value = 'stream') %>%
  osmdata_sf ()


cols1 <- c("#125C13","#3E7C17","#BFD8B8","#F4A442","#E8E1D9")
cols2 <- c("#a1cca5", "#FEF5ED","#D3E4CD","#ADC2A9", "#99A799","#0f2c30")
cols3 <- c("#7a918d","#a1cca5","#8fb996","#709775","#415d43","#0f2c30")

cols4 <- c("#7a918d","#a1cca5","#8fb996","#709775","#415d43","#0f2c30")

cols5 <- c("#70967e","#568664","#367348","#346847","#2c523d","#2c523d")

cols6 <- c("#70967e90","#56866490","#36734890","#34684790","#2c523d90","#2c523d90")

order(unique(getValues(treeC)))

plot(q$osm_polygons)
plot(q$osm_points)

plot(r$osm_lines)

plot(treeC, col=rev(cols3),
     breaks=c(0,3,10,15,20,25,30))

tallF <- function(x){
  ifelse(x >= 100,NA,x )
}

treeSub <- calc(treeC, tallF)

tm_shape(treeSub)+
  tm_raster(palette ="Greens",
            style="sd")



test <- tm_shape(treeSub)+
  tm_raster(palette ="Greens",
            style="sd")

tm_shape(treeSub)+
  tm_raster(palette ="Greens")

#breaks SD with rounding
breaksSD <- c(0,3,8,12.5,17,22,26,31)
breaksEqual <- c(0,5,10,15,20,25,31)


plot(treeSub, col=c("white",cols3),
     breaks=breaksSD)

plot(all_streets$osm_lines$geometry, col="grey30",  add=TRUE)



plot(treeSub, col=c("white",cols4),
     breaks=breaksSD)

plot(all_streets$osm_lines$geometry, col="grey30",  add=TRUE)

plot(r$osm_lines$geometry, col="royalblue3", add=TRUE)
