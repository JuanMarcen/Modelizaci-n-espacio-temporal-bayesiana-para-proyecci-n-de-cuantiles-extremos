lon <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/lon.rds")
lat <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/lat.rds")
g_700_12pm_60_23 <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/g_700_12pm_60_23.rds")
g_500_12pm_60_23 <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/g_500_12pm_60_23.rds")
g_300_12pm_60_23 <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/g_300_12pm_60_23.rds")
Date <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Date.rds")

setwd('C:/Users/jumar/OneDrive/Escritorio/TFM/Datos')
library(dplyr)
library(lubridate)
Date1<-as.Date(Date,format = "%m/%d/%Y")

#subsets de JJA
jja<-which(month(Date1) %in% c(6,7,8))

Date_jja<-Date1[jja]
lat_jja<-lat[jja]
lon_jja<-lon[jja]
g_300_jja<-g_300_12pm_60_23[jja]
g_500_jja<-g_500_12pm_60_23[jja]
g_700_jja<-g_700_12pm_60_23[jja]

#coordenadas unicas 11x16
coord <- data.frame(lat=lat_jja,lon=lon_jja)
coord <- unique(coord)
coord$station <- paste(coord$lat, coord$lon, sep = "_")

head(coord)

coord<-coord[order(-coord$lat),] #de izq a derecha
head(coord)
orden_estaciones <- unique(coord$station)
head(orden_estaciones)

#g300
g300_jja <- data.frame(Date = Date_jja, lat = lat_jja, lon = lon_jja, g300 = g_300_jja)
g300_jja$station <- paste(g300_jja$lat, g300_jja$lon, sep = "_")
g300_jja$lat<-NULL
g300_jja$lon<-NULL

# Convertir el data frame de largo a ancho
g300_jja <- reshape(g300_jja, 
                     idvar = "Date", 
                     timevar = "station", 
                     direction = "wide")

colnames(g300_jja)[-1] <- sub("^g300\\.", "", colnames(g300_jja)[-1])
g300_jja <- g300_jja[, c("Date", orden_estaciones)]
head(g300_jja)

#g500
g500_jja <- data.frame(Date = Date_jja, lat = lat_jja, lon = lon_jja, g500 = g_500_jja)
g500_jja$station <- paste(g500_jja$lat, g500_jja$lon, sep = "_")
g500_jja$lat<-NULL
g500_jja$lon<-NULL

# Convertir el data frame de largo a ancho
g500_jja <- reshape(g500_jja, 
                    idvar = "Date", 
                    timevar = "station", 
                    direction = "wide")

colnames(g500_jja)[-1] <- sub("^g500\\.", "", colnames(g500_jja)[-1])
g500_jja <- g500_jja[, c("Date", orden_estaciones)]
head(g500_jja)

#g700
g700_jja <- data.frame(Date = Date_jja, lat = lat_jja, lon = lon_jja, g700 = g_700_jja)
g700_jja$station <- paste(g700_jja$lat, g700_jja$lon, sep = "_")
g700_jja$lat<-NULL
g700_jja$lon<-NULL

# Convertir el data frame de largo a ancho
g700_jja <- reshape(g700_jja, 
                    idvar = "Date", 
                    timevar = "station", 
                    direction = "wide")

colnames(g700_jja)[-1] <- sub("^g700\\.", "", colnames(g700_jja)[-1])
g700_jja <- g700_jja[, c("Date", orden_estaciones)]
head(g700_jja)


saveRDS(coord, "coord.rds")
saveRDS(g300_jja, "g300_jja.rds")
saveRDS(g500_jja, "g500_jja.rds")
saveRDS(g700_jja, "g700_jja.rds")


