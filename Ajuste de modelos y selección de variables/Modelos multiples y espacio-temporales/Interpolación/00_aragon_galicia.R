rm(list=ls())

setwd('C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Analisis Exploratorio/ModelosReg/interpolacion')
stations <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/stations.rds")

stations <- st_transform(
  as(
    SpatialPointsDataFrame(
      coords = stations[c("LON", "LAT")], 
      data = stations[c("STAID", "STANAME", "LON", "LAT", "HGHT","color")],
      proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")),
    'sf'
  ),
  2062
)



# DEFINE GRID OF SPAIN
# Boundaring box of Spain
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

spain_regions<-ne_states( country = "Spain", returnclass = "sf")
aragon<-spain_regions[spain_regions$region=='Aragón',]
aragon<-st_transform(aragon, 2062)


spain_coords <- Polygons(
  list(Polygon(st_coordinates(spain)[st_coordinates(spain)[,"L2"] == 3, 1:2])),
  ID = "spain")
spain_coords <- SpatialPolygons(list(spain_coords))
spain_coords <- as(spain_coords, "sf")
st_crs(spain_coords) <- st_crs(spain)


aragon_union <- st_union(aragon)
saveRDS(aragon_union,'aragon.rds')


#GALICIA
galicia<-spain_regions[spain_regions$region=='Galicia',]
galicia<-st_transform(galicia, 2062)
galicia_union<-st_union(galicia)
saveRDS(galicia_union,'galicia.rds')
