
library(osmdata)
library(sf)
library(mapview)
library(raster)

treeH <- raster("E:/Google Drive/GIS/forest/Forest_height_2019_NAM.tif")

BB <- c(-75.4958,43.0102,-75.3087,43.0941)

treeC <- crop(treeH, extent(BB))

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


cols <- c("#125C13","#3E7C17","#F4A442","#E8E1D9")


plot(q$osm_polygons)
plot(q$osm_points)

plot(r$osm_lines)
