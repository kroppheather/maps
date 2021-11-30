
library(osmdata)
library(sf)
library(mapview)
library(raster)
library(tmap)
library(elevatr)
library(progress)


#treeH <- raster("E:/Google Drive/GIS/forest/Forest_height_2019_NAM.tif",
   #             format="GTiff")

BB <- c(-75.4958,43.0102,-75.3087,43.0941)

# treeC <- crop(treeH, extent(c(-75.4958,-75.3087,43.0102,43.0941)))
#writeRaster(treeC, "E:/Google Drive/GIS/Kirkland/kirkland_height_2019.tif",
#            format="GTiff", overwrite=TRUE)
treeC <- raster("E:/Google Drive/GIS/Kirkland/kirkland_height_2019.tif")

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
                  value = c("residential"
                  ))%>%
  osmdata_sf()
#  "service", "unclassified", 

plot(small_streets$osm_lines$geometry)

all_streets <- opq(bbox = BB)%>%
  add_osm_feature(key = "highway", 
                  value = available_tags("highway"))%>%
  osmdata_sf()
library(ggplot2)

plot(all_streets$osm_lines$geometry, lwd=0.005, col="grey")
plot(med_streets$osm_lines$geometry, lwd=1, col="black", add=TRUE)
plot(small_streets$osm_lines$geometry, lwd=1, col="black", add=TRUE)
ggplot(all_streets$osm_points)+
  geom_sf()

library(smoothr)
tm_shape(all_streets)+
  smooth_map()

street_smooth <- smooth(all_streets$osm_lines, method = "ksmooth", smoothness = 2.8)
plot(street_smooth$geometry)
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
  tm_raster(palette ="Greens")+
  tm_shape(all_streets$osm_lines)+
  tm_lines(col="grey50", alpha=0.75,lwd=0.5)

#breaks SD with rounding
breaksSD <- c(0,3,8,12.5,17,22,26,31)
breaksEqual <- c(0,5,10,15,20,25,31)


plot(treeSub, col=c("white",cols3),
     breaks=breaksSD)

plot(all_streets$osm_lines$geometry, col="grey30",  add=TRUE)



plot(treeSub, col=c("white",cols4),
     breaks=breaksSD)

plot(all_streets$osm_lines$geometry, col="grey30",  add=TRUE)
plot(all_streets$osm_lines$geometry, col="grey30")


plot(r$osm_lines$geometry, col="royalblue3", add=TRUE)

plot(treeSub, col=c("grey30",cols3),colNA="grey30",
     breaks=breaksSD)
plot(all_streets$osm_lines$geometry, col="white",  add=TRUE)



tm_shape(treeSub)+
  tm_raster(palette ="Greens")+
  tm_shape(all_streets$osm_lines)+
  tm_lines(size=0.5)

cElev <- raster("E:/Google Drive/GIS/Kirkland/p35elu.dem")

plot(cElev)

cElevp <- projectRaster(cElev, crs=treeC@crs)

slope <- terrain(cElevp, opt=
                   "slope")

aspect <- terrain(cElevp, opt=
                    "aspect")

hillshade <- hillShade(slope,aspect)

plot(treeSub, col=c("white",cols4),
     breaks=breaksSD)
plot(hillshadE, col=grey(1:100/100, 0.25), add=TRUE)


lulc <- raster("E:/Google Drive/GIS/NLCD_2016_Land_Cover_L48_20190424/NLCD_2016_Land_Cover_L48_20190424.img")
plot(lulc, axes=TRUE)

lulC <- crop(lulc,extent(1550000,1720000,2350000,2420000))
plot(lulC)

lulCp <- projectRaster(lulC, crs=treeC@crs, method="ngb")


lulCk <- crop(lulCp,treeC)
plot(lulCk)
unique(getValues(lulCk))
help(projectRaster)

# isolate landcover

#crops

cropsF <- function(x){
  ifelse(x == 82,1,NA)
}
crops <- calc(lulCk,cropsF)

plot(treeSub, col=c("white",cols4),
     breaks=breaksSD)
plot(crops, col="#b8860b55",add=TRUE)

wetlandsF <- function(x){
  ifelse(x == 90 | x == 95,1,NA)
}
wetlands <- calc(lulCk,wetlandsF)
plot(wetlands, col="cornflowerblue", add=TRUE)
plot(r$osm_lines$geometry, col="royalblue3", lwd=2,add=TRUE)
plot(lulCk)
#81: pasture hay
#90: woody wetlands
#95: emergent herbacious wetlands
#41: deciduous forest
#21: Developed, Open Space - 
#   Includes areas with a mixture of some constructed materials, but mostly vegetation in the form of lawn grasses. 
#   Impervious surfaces account for less than 20 percent of total cover. 
# These areas most commonly include large-lot single-family housing units, parks, golf courses, and vegetation 
# planted in developed settings for recreation, erosion control, or aesthetic purposes

# 82: Cultivated Crops Areas used for the production of annual crops, such as corn, soybeans, vegetables, tobacco, and cotton, and also perennial woody crops such as orchards
# 43: Mixed Forest - Areas dominated by trees generally greater than 5 meters tall
# 71:Grassland/Herbaceous - Areas dominated by grammanoid or herbaceous vegetation, generally greater than 80% of total vegetation
# 11: Open Water - All areas of open water, generally with less than 25% cover or vegetation or soil
# 52: Shrub/Scrub - Areas dominated by shrubs; 
# 42: Evergreen Forest 
# 22: Developed, Low Intensity
# 23: Developed, Medium Intensity
# 24: Developed, High Intensity
# 31: Barren Land (Rock/Sand/Clay) - Barren areas of bedrock, desert pavement, 
#     scarps, talus, slides, volcanic material, glacial debris, sand dunes, 
#     strip mines, gravel pits and other accumulations of earthen material. 
#    Generally, vegetation accounts for less than 15% of total cover