library(shiny)
library(leaflet)
library(raster)
library(sf)
require(mapview)
require(sp)
require(rgeos)
require(rgdal)
require(stplanr)
require(maptools)
library(raster)

load("data/trips.export.RData")
load("data/pollution_brick.RData")

# OS map network
roads<- readOGR("data/Road_Network.shp")

# Clip to box of raster brick
roads <- crop(roads, extent(projectRaster(pollution_brick, crs=crs(roads))))

# Convert to sf
roads<- st_as_sf(roads)
trips<- st_as_sf(trips.export[1:1000,]) # Subset the data because otherwise st_intersects takes ages

## Tell sf what coordinate system we're in
st_crs(roads)<- "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs" # From proj4string?
st_crs(trips)<- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

# Convert trips data to roads coordinates because distances are now in meters
trips<-st_transform(trips, st_crs(roads))



# Create 5m buffer around each road
road.buffers<-st_buffer(roads, 5)

# Find all the trips by each mode within buffers
inter.car<- st_intersects(road.buffers, trips[trips$modality=="Car",])
inter.bike<- st_intersects(road.buffers, trips[trips$modality=="Bike",])
inter.bus<- st_intersects(road.buffers, trips[trips$modality=="Bus",])
inter.walk<- st_intersects(road.buffers, trips[trips$modality=="Foot",])

# summarise how many trips on each road by mode
roads$trips.car<-sapply(inter.car, function(x) length(x))
roads$trips.bike<-sapply(inter.bike, function(x) length(x))
roads$trips.bus<-sapply(inter.bus, function(x) length(x))
roads$trips.walk<-sapply(inter.walk, function(x) length(x))

# create interactive map of where there is more than one "walk" trip
mapview(roads[roads$trips.walk>0,], zcol="trips.walk")
