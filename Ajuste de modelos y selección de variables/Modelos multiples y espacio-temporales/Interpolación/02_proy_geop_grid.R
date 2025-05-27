rm(list=ls())
setwd('C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Analisis Exploratorio/ModelosReg/interpolacion')
grid_km <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Analisis Exploratorio/ModelosReg/interpolacion/grid_km.rds")
coord <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/coord.rds")

library(sf)
library(sp)
#pasar coodernadas a km
coord <- st_transform(
  as(
    SpatialPointsDataFrame(
      coords = coord[c("lon", "lat")], 
      data = coord[c('lon','lat','station')],
      proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")),
    'sf'
  ),
  2062
)
coord_km<-st_coordinates(coord)/1000

## nearest neighbor
library(RANN)
nn <- nn2(data = coord_km, query = grid_km, k = 1)
indices_mas_cercanos <- nn$nn.idx[, 1]
estacion_mas_cercana <- coord$station[indices_mas_cercanos]

grid_cercanos<-as.data.frame(cbind(1:790,grid_km,estacion_mas_cercana))
colnames(grid_cercanos)<-c('station_grid','lon','lat','station_2')

#construccion del data.frame que contiene todos (5888*790 filas)
g300_jja <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/g300_jja.rds")
g500_jja <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/g500_jja.rds")
g700_jja <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/g700_jja.rds")

#esquinas
col<-which(colnames(g300_jja) %in% c('45_-10','45_5','35_-10','35_5'))
esquinas_g300 <- matrix(NA, nrow = dim(grid_cercanos)[1]*dim(g300_jja)[1], ncol = 4)
dim(esquinas_g300)

for( i in 1:4){
  esquinas_g300[,i]<-rep(g300_jja[[col[i]]],times=dim(grid_cercanos)[1])
}

esquinas_g300<-as.data.frame(esquinas_g300)
colnames(esquinas_g300)<-c('45_-10','45_5','35_-10','35_5')


esquinas_g500 <- matrix(NA, nrow = dim(grid_cercanos)[1]*dim(g500_jja)[1], ncol = 4)
dim(esquinas_g500)

for( i in 1:4){
  esquinas_g500[,i]<-rep(g500_jja[[col[i]]],times=dim(grid_cercanos)[1])
}

esquinas_g500<-as.data.frame(esquinas_g500)
colnames(esquinas_g500)<-c('45_-10','45_5','35_-10','35_5')

esquinas_g700 <- matrix(NA, nrow = dim(grid_cercanos)[1]*dim(g700_jja)[1], ncol = 4)
dim(esquinas_g700)

for( i in 1:4){
  esquinas_g700[,i]<-rep(g700_jja[[col[i]]],times=dim(grid_cercanos)[1])
}

esquinas_g700<-as.data.frame(esquinas_g700)
colnames(esquinas_g700)<-c('45_-10','45_5','35_-10','35_5')

#g300 cercano
order<-match(grid_cercanos$station_2, colnames(g300_jja)[-1]) #a estos indices hay que sumar uno por el de fecha que ignaramos
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
order<-match(grid_cercanos$station_2, colnames(g500_jja)[-1]) #a estos indices hay que sumar uno por el de fecha que ignaramos
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
order<-match(grid_cercanos$station_2, colnames(g700_jja)[-1]) #a estos indices hay que sumar uno por el de fecha que ignaramos
g700_jja_ord <- g700_jja[, c(1, order + 1)]

Xg700_vector <- as.vector(as.matrix(g700_jja_ord[,-1]))
head(Xg700_vector)
Xg700 <- data.frame(
  Date = rep(g700_jja_ord$Date, times = ncol(g700_jja_ord[,-1])),  # Repetir fechas
  Station = rep(colnames(g700_jja_ord[,-1]), each = nrow(g700_jja_ord[,-1])),  # Repetir estaciones
  Value = Xg700_vector
)
head(Xg700)



X_grid <- data.frame(
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
head(X_grid)

#anadir estacion y a X para poder luego hacer subsets
X_grid$station<-NA
X_grid<-X_grid[,c(1,18,2:17)]
X_grid$station<-rep(1:790,each = 5888)

for (i in 4:18){
  X_grid[[i]]<-X_grid[[i]]/1000
}
head(X_grid)

#añadido de elev y distancia a costa
X_grid<-cbind(X_grid, rep(grid_elev, each= 92*64),rep(grid_dist, each= 92*64))
colnames(X_grid)[c(19,20)]<-c('elev','dist')

saveRDS(X_grid,'X_grid.rds')

