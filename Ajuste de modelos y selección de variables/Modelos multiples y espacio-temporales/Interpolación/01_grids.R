rm(list=ls())

setwd('C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Analisis Exploratorio/ModelosReg/interpolacion')
stations <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/stations.rds")

library(sp)
library(sf)
library("tidyverse")
library("rnaturalearth")
library("rnaturalearthdata")

#------------------------GRID LON/LAT------------------------
#limites y grid del mapa
limits <- st_transform(
  as(
    SpatialPointsDataFrame(
      coords = data.frame(X = c(-10, 4), Y = c(35.5, 44)), 
      data = data.frame(X = c(-10, 4), Y = c(35.5, 44)),
      proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")),
    'sf'
  ),
  2062
)

saveRDS(limits,'limits.rds')
# Load the polygon of Spain and projection 2062
spain <- ne_countries(scale = "large", country = "Spain", returnclass = "sf")
spain <- st_transform(spain, 2062)

# Extract peninsular Spain
spain_coords <- Polygons(
  list(Polygon(st_coordinates(spain)[st_coordinates(spain)[,"L2"] == 3, 1:2])),
  ID = "spain")
spain_coords <- SpatialPolygons(list(spain_coords))
spain_coords <- as(spain_coords, "sf")
st_crs(spain_coords) <- st_crs(spain)

#background
world_map <- ne_countries(scale = "large", returnclass = 'sf')
european_union <- c("Algeria", "Andorra", "France", "Gibraltar", "Morocco", "Portugal", "Spain")
european_union_map <- 
  world_map %>% 
  filter(name %in% european_union)
background <- st_transform(european_union_map, 2062)
saveRDS(background,'background.rds')

# Build squared grid 25 x 25 km and intersect with peninsular Spain
grid <- st_make_grid(spain, cellsize = 25000, what = "centers")
grid <- st_intersection(grid, spain_coords)
saveRDS(grid,'grid.rds')

#grid en metros
grid_km<-st_coordinates(grid)/1000
#grid en grados
grid_grados<-st_transform(grid,crs=4326)
grid_grados<-st_coordinates(grid_grados)

saveRDS(grid_grados,'grid_grados.rds')
saveRDS(grid_km,'grid_km.rds')

#------------------------GRID ELEVACION------------------------
library("elevatr")
# OBTAIN ELEVATION (in m)
# Define the grid as sp object
coords.grid <- data.frame(st_coordinates(grid))
colnames(coords.grid) <- c("x", "y")
elev <- get_elev_point(coords.grid, prj = "EPSG:2062", src = "aws", z =
                         10)
elev$elevation[elev$elevation < 5] <- 5

# Insert elevation
grid_elev<-elev$elevation

saveRDS(grid_elev,'grid_elev.rds')


#------------------------GRID DISTANCIA COSTA------------------------
limits <- st_transform(
  as(
    SpatialPointsDataFrame(
      coords = data.frame(X = c(-10, 4), Y = c(35.5, 44)), 
      data = data.frame(X = c(-10, 4), Y = c(35.5, 44)),
      proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")),
    'sf'
  ),
  2062
)

# Load the polygon of Spain and projection 2062
spain <- ne_countries(scale = "large", country = "Spain", returnclass = "sf")
spain <- st_transform(spain, 2062)

# Extract peninsular Spain
spain_coords <- Polygons(
  list(Polygon(st_coordinates(spain)[st_coordinates(spain)[,"L2"] == 3, 1:2])),
  ID = "spain")
spain_coords <- SpatialPolygons(list(spain_coords))
spain_coords <- as(spain_coords, "sf")
st_crs(spain_coords) <- st_crs(spain)

# OBTAIN DISTANCE TO THE COAST (in km)
# Load the polygon of neighboring countries, join them, and projection 2062
world_map <- ne_countries(scale = "large", returnclass = 'sf')
european_union <- c("Algeria", "Andorra", "France", "Gibraltar", "Morocco", "Portugal", "Spain")
european_union_map <- 
  world_map %>% 
  filter(name %in% european_union)
background <- st_transform(european_union_map, 2062)
bbox_europe <- st_bbox(c(xmin = -10, ymin = 20, xmax = 50, ymax = 80), crs = st_crs(european_union_map))
european_union_map_cropped <- st_crop(european_union_map, bbox_europe)
european_union_map_cropped <- st_union(european_union_map_cropped)
european_union_map_cropped <- st_transform(european_union_map_cropped, 2062)

# Distance from each grid cell to the boundary
dist <- st_distance(st_boundary(european_union_map_cropped), grid)
grid_dist <- st_sf(dist = round(as.vector(dist) / 1000, 3), geometry = grid)
grid_dist<-grid_dist$dist

saveRDS(grid_dist,'grid_dist.rds')
