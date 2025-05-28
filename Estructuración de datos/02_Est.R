setwd('C:/Users/jumar/Desktop/TFM/Datos')

g700_jja <- readRDS("C:/Users/jumar/Desktop/TFM/Datos/g700_jja.rds")
g500_jja <- readRDS("C:/Users/jumar/Desktop/TFM/Datos/g500_jja.rds")
g300_jja <- readRDS("C:/Users//jumar/Desktop/TFM/Datos/g300_jja.rds")
coord <- readRDS("C:/Users/jumar/Desktop/TFM/Datos/coord.rds")
Date <- readRDS("C:/Users/jumar/Desktop/TFM/Datos/Date.rds")
library(readr)
Tx_mat <- read_csv("Tx_mat.csv")
stations <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/stations.rds")
 

library(lubridate)
library(dplyr)
library(fields)
head(Date)
Date1<-as.Date(Date,format = "%m/%d/%Y")

#subsets de JJA
data <- Tx_mat
data$fecha <- as.Date(data$Date, format = "%Y-%m-%d")

data_jja <- data %>%
  filter(month(fecha) >= 6 & month(fecha) <= 8)
data_jja$fecha<-NULL


Y_vector <- as.vector(as.matrix(data_jja[,-1]))
head(Y_vector)
Y <- data.frame(
  Date = rep(data_jja$Date, times = ncol(data_jja[,-1])),  # Repetir fechas
  station = rep(colnames(data_jja[,-1]), each = nrow(data_jja[,-1])),  # Repetir estaciones
  Value = Y_vector
)
head(Y)

# estacion mas cercana del mallado a cada una de las estaciones de temperatura
#distancia euclidea
nearest_station<-function(lat,lon,coord){
  d <- sqrt((lat-coord$lat)^2+(lon-coord$lon)^2)
  return(coord$station[which.min(d)])
}

nearest_station(stations[1,4],stations[1,5],coord)

nearest_stations<-mapply(nearest_station,stations$LAT,stations$LON, MoreArgs = list(coord=coord))
head(nearest_stations)

rel_stations<-data.frame(
  station = stations$STAID,
  station_2 = nearest_stations
)
head(rel_stations)

#distancia geodésica (primera columna longitudes, segunda latitudes)
#hay que ponerlo como matrices
m1<-as.matrix(stations[,c('LON','LAT')])
m2<-as.matrix(coord[,c('lon','lat')])

dist<-rdist.earth(m1,m2,miles=F)
dist

nearest<-apply(dist,1,which.min)

rel_stations2<-data.frame(
  station=stations$STAID,
  station_2=coord$station[nearest]
)

rel_stations==rel_stations2 # se obtiene lo mismo

# X
#esquinas
col<-which(colnames(g300_jja) %in% c('45_-10','45_5','35_-10','35_5'))

esquinas_g300 <- matrix(NA, nrow = dim(stations)[1]*dim(g300_jja)[1], ncol = 4)
dim(esquinas_g300)

for( i in 1:4){
  esquinas_g300[,i]<-rep(g300_jja[[col[i]]],times=dim(stations)[1])
}

esquinas_g300<-as.data.frame(esquinas_g300)
colnames(esquinas_g300)<-c('45_-10','45_5','35_-10','35_5')


esquinas_g500 <- matrix(NA, nrow = dim(stations)[1]*dim(g500_jja)[1], ncol = 4)
dim(esquinas_g500)

for( i in 1:4){
  esquinas_g500[,i]<-rep(g500_jja[[col[i]]],times=dim(stations)[1])
}

esquinas_g500<-as.data.frame(esquinas_g500)
colnames(esquinas_g500)<-c('45_-10','45_5','35_-10','35_5')

esquinas_g700 <- matrix(NA, nrow = dim(stations)[1]*dim(g700_jja)[1], ncol = 4)
dim(esquinas_g700)

for( i in 1:4){
  esquinas_g700[,i]<-rep(g700_jja[[col[i]]],times=dim(stations)[1])
}

esquinas_g700<-as.data.frame(esquinas_g700)
colnames(esquinas_g700)<-c('45_-10','45_5','35_-10','35_5')

#g300 cercano
order<-match(rel_stations$station_2, colnames(g300_jja)[-1]) #a estos indices hay que sumar uno por el de fecha que ignaramos
g300_jja_ord <- g300_jja[, c(1, order + 1)]
Xg300_vector <- as.vector(as.matrix(g300_jja_ord[,-1]))
head(Xg300_vector)
Xg300 <- data.frame(
  Date = rep(g300_jja_ord$Date, times = ncol(g300_jja_ord[,-1])),  # Repetir fechas
  Station = rep(colnames(g300_jja_ord[,-1]), each = nrow(g300_jja_ord[,-1])),  # Repetir estaciones
  Value = Xg300_vector
)
head(Xg300)

#g500 cercano
order<-match(rel_stations$station_2, colnames(g500_jja)[-1]) #a estos indices hay que sumar uno por el de fecha que ignaramos
g500_jja_ord <- g500_jja[, c(1, order + 1)]

Xg500_vector <- as.vector(as.matrix(g500_jja_ord[,-1]))
head(Xg500_vector)
Xg500 <- data.frame(
  Date = rep(g500_jja_ord$Date, times = ncol(g500_jja_ord[,-1])),  # Repetir fechas
  Station = rep(colnames(g500_jja_ord[,-1]), each = nrow(g500_jja_ord[,-1])),  # Repetir estaciones
  Value = Xg500_vector
)
head(Xg500)

#g700 cercano
order<-match(rel_stations$station_2, colnames(g700_jja)[-1]) #a estos indices hay que sumar uno por el de fecha que ignaramos
g700_jja_ord <- g700_jja[, c(1, order + 1)]

Xg700_vector <- as.vector(as.matrix(g700_jja_ord[,-1]))
head(Xg700_vector)
Xg700 <- data.frame(
  Date = rep(g700_jja_ord$Date, times = ncol(g700_jja_ord[,-1])),  # Repetir fechas
  Station = rep(colnames(g700_jja_ord[,-1]), each = nrow(g700_jja_ord[,-1])),  # Repetir estaciones
  Value = Xg700_vector
)
head(Xg700)



X <- data.frame(
  Date=Xg700$Date,
  station_x = Xg700$Station,
  g300 = Xg300$Value,
  `g300_45_-10` = esquinas_g300$`45_-10`,
  g300_45_5 = esquinas_g300$`45_5`,
  `g300_35_-10` = esquinas_g300$`35_-10`,
  g300_35_5 = esquinas_g300$`35_5`,
  g500 = Xg500$Value,
  `g500_45_-10` = esquinas_g500$`45_-10`,
  g500_45_5 = esquinas_g500$`45_5`,
  `g500_35_-10` = esquinas_g500$`35_-10`,
  g500_35_5 = esquinas_g500$`35_5`,
  g700 = Xg700$Value,
  `g700_45_-10` = esquinas_g700$`45_-10`,
  g700_45_5 = esquinas_g700$`45_5`,
  `g700_35_-10` = esquinas_g700$`35_-10`,
  g700_35_5 = esquinas_g700$`35_5`
)
head(X)

#anadir estacion y a X para poder luego hacer subsets
X$station<-NA
X<-X[,c(1,18,2:17)]
head(X)
for (name in stations$STAID){
  ind<-which(Y$station==name)
  X$station[ind]<-name
}
head(X)

#chequeo
for (name in stations$STAID){
  ind<-which(Y$station==name)
  ind2<-which(X$station==name)
  print(sum(ind==ind2))
}

#transformacion a km 
for (i in 4:18){
  X[[i]]<-X[[i]]/1000
}
head(X)

saveRDS(X,'X.rds')
saveRDS(Y,'Y.rds')
saveRDS(rel_stations,'rel_stations.rds')
