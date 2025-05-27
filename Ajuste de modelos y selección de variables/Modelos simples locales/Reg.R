#setwd('C:/Users/jumar/Desktop/TFM/Datos')
rm(list = setdiff(ls(), c("stations",'X','Y','pendientes_R','met_ajuste')))
#portatil
limits <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Analisis Exploratorio/ModelosReg/interpolacion/limits.rds")
background <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Analisis Exploratorio/ModelosReg/interpolacion/background.rds")
Y <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Y.rds")
X <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/X.rds")
stations <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/stations.rds")

library(quantreg)
library(sf)
library(sp)
library("rnaturalearth")
library("rnaturalearthdata")
library(tidyverse)
library(ggplot2)
library(dplyr)
library(ggpubr)


#EJECUTAR f_pendientes.R para obtención de coeficientes modelos
#filtrados por meses y años
Y_junio <- Y %>% 
  filter(month(Date)==6)
X_junio <- X %>%
  filter(month(Date)==6)
Y_julio <- Y %>% 
  filter(month(Date)==7)
X_julio <- X %>%
  filter(month(Date)==7)  
Y_ag <- Y %>% 
  filter(month(Date)==8)
X_ag <- X %>%
  filter(month(Date)==8) 

Y_04_23<-Y %>%
  filter(year(Date)>=2004)
X_04_23<-X %>%
  filter(year(Date)>=2004)
Y_84_03 <- Y %>%
  filter(year(Date)>=1984 & year(Date)<=2003)
X_84_03 <- X %>%
  filter(year(Date)>=1984 & year(Date)<=2003)
Y_64_83 <- Y %>%
  filter(year(Date)>=1964 & year(Date)<=1983)
X_64_83 <- X %>%
  filter(year(Date)>=1964 & year(Date)<=1983)


#------------------------COEFICIENTES MODELOS SIMPLES LOCALES TODAS COVARIABLES------------------------
#toda serie todos meses jja CON INTERCEPTOS
coef<-pendientes_R(stations,Y,X)
saveRDS(coef,'coef_R.rds')
#coeficientes para el cuadrado de los anteriores
X_cuadrado<-X
X_cuadrado[, 4:ncol(X)] <- X[, 4:ncol(X)]^2

coef_cuadrados <- pendientes_R(stations, Y, X_cuadrado)

#coeficientes variables escaladas
coef_R_sc<-pendientes_R(stations,Y,X,scale=T)

saveRDS(coef_R_sc,'coef_R_sc.rds')
saveRDS(coef_cuadrados,'coef_R_cuadrados.rds')


#------------------------COEFICIENTES POR MESES------------------------
#por meses
coef_junio<-pendientes_R(stations,Y_junio,X_junio,scale=T)
coef_julio<-pendientes_R(stations,Y_julio,X_julio,scale=T)
coef_ag<-pendientes_R(stations,Y_ag,X_ag,scale=T)

#por periodos 20 años
coef_04_23<-pendientes_R(stations,Y_04_23,X_04_23,scale=T)
coef_84_03<-pendientes_R(stations,Y_84_03,X_84_03,scale=T)
coef_64_83<-pendientes_R(stations,Y_64_83,X_64_83,scale=T)


saveRDS(coef_junio,'coef_junio.rds')
saveRDS(coef_julio,'coef_julio.rds')
saveRDS(coef_ag,'coef_ag.rds')
saveRDS(coef_04_23,'coef_04_23.rds')
saveRDS(coef_64_83,'coef_64_83.rds')
saveRDS(coef_84_03,'coef_84_03.rds')

#------------------------MAPAS------------------------
mapSpain <- function(Z,XG, coords, ref, leyenda, nombre,colores,nombre_ley,limites) {
  # Load Spain map data
  spain <- ne_countries(scale = "medium", returnclass = "sf") %>%
    filter(admin == "Spain")
  
  # Create a data frame with coordinates and Z values
  data <- cbind(coords, Z)
  
  
  # Convert to sf object
  data_sf <- st_as_sf(data, coords = c("Longitude", "Latitude"), crs = 4326)
  #coords_sf <- st_as_sf(XG, coords = c("lon", "lat"), crs = 4326)
  
  
  # Plot the map
  p <- ggplot(data = background) +
    geom_sf(fill = "antiquewhite") +
    geom_sf(data = data_sf, aes(color = Z), size = 2) +
    xlab("Longitud (º)") + 
    ylab("Latitud (º)") + 
    ggtitle(nombre) +
    coord_sf(xlim = st_coordinates(limits)[,1], ylim = st_coordinates(limits)[,2]) +
    theme(panel.background = element_rect(fill = "aliceblue"),
          axis.text.x = element_text(size = 6),
          axis.text.y = element_text(size = 6, angle = 90),
          axis.title = element_text(size = 10, face = "bold"))
  
  # Add legend if leyenda is TRUE
  if (leyenda) {
    p <- p + theme(legend.position = "right") +
      scale_color_gradientn(colors = colores, limits = limites, name = nombre_ley) +
      guides(color = guide_colorbar(barwidth = 1.5, barheight = 10))
  } else {
    p <- p + 
      scale_color_gradientn(colors = colores, limits = limites, name = nombre_ley, guide = "none")+
      theme(legend.position = "none")  # Ocultar leyenda si leyenda es FALSE
  }
  
  # Save the plot to a file
  return(p)
}

colores<-c('darkred',"orange",'darkgreen')

Lat<-stations$LAT
Lon<-stations$LON


mapas <- function(Z1, Z2, tipo, cuantil1, cuantil2, name, nombre_ley){
  
  valor_min <- min(Z1, Z2, na.rm = TRUE)
  valor_max <- max(Z1, Z2, na.rm = TRUE)
  
  limites_comunes <- c(valor_min, valor_max)
  
  map1<-mapSpain(Z = Z1,XG=coord, coords = data.frame(Longitude = Lon, Latitude = Lat),
                 ref = 0.05, leyenda = F,
                 nombre=bquote(.(tipo) * .(name) * .(' (') * tau *.(' = ') * .(cuantil1) * .(')')),
                 colores = colores,nombre_ley = nombre_ley,limites = limites_comunes)
  map2<-mapSpain(Z = Z2,XG=coord, coords = data.frame(Longitude = Lon, Latitude = Lat),
                 ref = 0.05, leyenda = T,
                 nombre=bquote(.(tipo) * .(name) * .(' (') * tau *.(' = ') * .(cuantil2) * .(')')),
                 colores = colores,nombre_ley = nombre_ley,limites = limites_comunes)
  
  valor_min <- -10
  valor_max <- 10
  
  limites_comunes <- c(valor_min, valor_max)
  
  map_dif<-mapSpain(Z = Z2-Z1,XG=coord, coords = data.frame(Longitude = Lon, Latitude = Lat),
                 ref = 0.05, leyenda = T,
                 nombre=bquote(.(tipo) * .(name) * .('(Dif. cuantiles)')),
                 colores = c('blue','white','red'),nombre_ley = 'Dif.',limites = limites_comunes)
  
  g<-ggpubr::ggarrange(map1,map2,map_dif,
                       nrow = 1,ncol=3, widths = c(0.775, 1, 1),
                       common.legend = F, legend = 'right')
  g
}

mapas_dif <- function(Z1, Z2, tipo, cuantil1, cuantil2, name, nombre_ley){
  
  valor_min <- -10
  valor_max <- 10
  
  limites_comunes <- c(valor_min, valor_max)
  
  map1<-mapSpain(Z = Z2-Z1,XG=coord, coords = data.frame(Longitude = Lon, Latitude = Lat),
                 ref = 0.05, leyenda = T,
                 nombre=bquote(.(tipo) * .(name) * .(' (Dif. cuantiles)')),
                 colores = c('blue','white','red'),nombre_ley = nombre_ley,limites = limites_comunes)
  return(map1)
}


#COEFICIENTES
#cercanos
mapas(coef$q0.5_g300,coef$q0.95_g300,'', 0.5, 0.95, 'G300','Coef.')
mapas(coef$q0.5_g500,coef$q0.95_g500,'', 0.5, 0.95, 'G500','Coef.')
mapas(coef$q0.5_g700,coef$q0.95_g700,'', 0.5, 0.95, 'G700','Coef.')
mapas(coef_R_sc$q0.5_g300,coef_R_sc$q0.95_g300,'', 0.5, 0.95, 'G300','Coef.')

#setwd('C:/Users/jumar/Desktop/TFM/Datos/Analisis Exploratorio/ModelosReg/mapas')
m1<-mapas(coef_R_sc$q0.5_g300,coef_R_sc$q0.95_g300,'', 0.5, 0.95, 'G300','Coef.')
m2<-mapas(coef_R_sc$q0.5_g500,coef_R_sc$q0.95_g500,'', 0.5, 0.95, 'G500','Coef.')
m3<-mapas(coef_R_sc$q0.5_g700,coef_R_sc$q0.95_g700,'', 0.5, 0.95, 'G700','Coef.')
ggpubr::ggarrange(m1,m2,m3,
                  nrow = 3,ncol=1, common.legend = F)
dev.off()

m1<-mapas(coef_R_sc$int_q0.5_g300,coef_R_sc$int_q0.95_g300,'', 0.5, 0.95, 'G300','Coef.')
m2<-mapas(coef_R_sc$int_q0.5_g500,coef_R_sc$int_q0.95_g500,'', 0.5, 0.95, 'G500','Coef.')
m3<-mapas(coef_R_sc$int_q0.5_g700,coef_R_sc$int_q0.95_g700,'', 0.5, 0.95, 'G700','Coef.')
ggpubr::ggarrange(m1,m2,m3,
                  nrow = 3,ncol=1, common.legend = F)
dev.off()

#esquinas
mapas(coef_R_sc$q0.5_g300_45_.10,coef_R_sc$q0.95_g300_45_.10,'', 0.5, 0.95, 'G300 45ºN 10ºW','Coef.')
dev.off()

mapas(coef$q0.5_g500_45_.10,coef$q0.95_g500_45_.10,'', 0.5, 0.95, 'G500 45ºN 10ºW','Coeficiente')
mapas(coef$q0.5_g700_45_.10,coef$q0.95_g700_45_.10,'', 0.5, 0.95, 'G700 45ºN 10ºW','Coeficiente')

mapas(coef$q0.5_g300_45_5,coef$q0.95_g300_45_5,'', 0.5, 0.95, 'G300 45ºN 5ºE','Coeficiente')
mapas(coef$q0.5_g500_45_5,coef$q0.95_g500_45_5,'', 0.5, 0.95, 'G500 45ºN 5ºE','Coeficiente')
mapas(coef$q0.5_g700_45_5,coef$q0.95_g700_45_5,'', 0.5, 0.95, 'G700 45ºN 5ºE','Coeficiente')

mapas(coef$q0.5_g300_35_.10,coef$q0.95_g300_35_.10,'', 0.5, 0.95, 'G300 35ºN 10ºW','Coeficiente')
mapas(coef$q0.5_g500_35_.10,coef$q0.95_g500_35_.10,'', 0.5, 0.95, 'G500 35ºN 10ºW','Coeficiente')
mapas(coef$q0.5_g700_35_.10,coef$q0.95_g700_35_.10,'', 0.5, 0.95, 'G700 35ºN 10ºW','Coeficiente')

mapas(coef$q0.5_g300_35_5,coef$q0.95_g300_35_5,'', 0.5, 0.95, 'G300 35ºN 5ºE','Coeficiente')
mapas(coef$q0.5_g500_35_5,coef$q0.95_g500_35_5,'', 0.5, 0.95, 'G500 35ºN 5ºE','Coeficiente')
mapas(coef$q0.5_g700_35_5,coef$q0.95_g700_35_5,'', 0.5, 0.95, 'G700 35ºN 5ºE','Coeficiente')

# METRICAS DE AJUSTE (R)
#cercanos
mapas(coef$R_q0.5_g300,coef$R_q0.95_g300,'R ', 0.5, 0.95, 'G300','R')
mapas(coef$R_q0.5_g500,coef$R_q0.95_g500,'R ', 0.5, 0.95, 'G500','R')
mapas(coef$R_q0.5_g700,coef$R_q0.95_g700,'R ', 0.5, 0.95, 'G700','R')

#esquinas
mapas(coef$R_q0.5_g300_45_.10,coef$R_q0.95_g300_45_.10,'R ', 0.5, 0.95, 'G300 45ºN 10ºW','R')
mapas(coef$R_q0.5_g500_45_.10,coef$R_q0.95_g500_45_.10,'R ', 0.5, 0.95, 'G500 45ºN 10ºW','R')
mapas(coef$R_q0.5_g700_45_.10,coef$R_q0.95_g700_45_.10,'R ', 0.5, 0.95, 'G700 45ºN 10ºW','R')

mapas(coef$R_q0.5_g300_45_5,coef$R_q0.95_g300_45_5,'R ', 0.5, 0.95, 'G300 45ºN 5ºE','R')
mapas(coef$R_q0.5_g500_45_5,coef$R_q0.95_g500_45_5,'R ', 0.5, 0.95, 'G500 45ºN 5ºE','R')
mapas(coef$R_q0.5_g700_45_5,coef$R_q0.95_g700_45_5,'R ', 0.5, 0.95, 'G700 45ºN 5ºE','R')

mapas(coef$R_q0.5_g300_35_.10,coef$R_q0.95_g300_35_.10,'R ', 0.5, 0.95, 'G300 35ºN 10ºW','R')
mapas(coef$R_q0.5_g500_35_.10,coef$R_q0.95_g500_35_.10,'R ', 0.5, 0.95, 'G500 35ºN 10ºW','R')
mapas(coef$R_q0.5_g700_35_.10,coef$R_q0.95_g700_35_.10,'R ', 0.5, 0.95, 'G700 35ºN 10ºW','R')

mapas(coef$R_q0.5_g300_35_5,coef$R_q0.95_g300_35_5,'R ', 0.5, 0.95, 'G300 35ºN 5ºE','R')
mapas(coef$R_q0.5_g500_35_5,coef$R_q0.95_g500_35_5,'R ', 0.5, 0.95, 'G500 35ºN 5ºE','R')
mapas(coef$R_q0.5_g700_35_5,coef$R_q0.95_g700_35_5,'R ', 0.5, 0.95, 'G700 35ºN 5ºE','R')





