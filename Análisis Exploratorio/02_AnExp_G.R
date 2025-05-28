rm(list=ls())

library(tseries)
library(zoo)
X <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/X.rds")
stations <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/stations.rds")

# ANALISIS DESCRIPTIVO DE COVARIABLES

#alturas geopotenciales (km)
g<-9.80665 
X[,-c(1,2,3)]<-X[,-c(1,2,3)]/g

#--------------TENDENCIA(ANUAL)--------------
tendencia_mm<-function(var,l,u){
  ind<-which(X$station==stations$STAID[1])
  
  ts_jja<-ts(X[ind,'g300'],start=1960,frequency=92)
  mm<- rollmean(ts_jja, k = 92+1, fill = NA)
  sintendencia <- ts_jja - mm 
  
  indest <- rep(c(1:92), length.out = length(ts_jja))
  sst <- tapply(sintendencia, INDEX = indest, FUN = mean, na.rm = TRUE)
  estst <- ts(rep(sst,length.out=length(sintendencia)),start=1960,frequency=92) #componente estacional
  
  dt<-ts_jja-estst #x_t-s_t
  mm4<-rollmean(dt,k=736,fill=NA)
  #segunda aproximacion de la tendencia
  plot(mm4,main=paste('Estimación de tendencias', toupper(var),'por sitio (medias móviles)'),
       ylab='Altura (km)',xlab='Año',col=stations$color[1],ylim=c(l,u))
  
  legend("top", legend = c("Costa Cantábrica", "Centro Peninsular", "Costa Mediterránea"),
         col = c("forestgreen", "red",'blue'), lwd = 1,ncol=3,cex=0.7)
  
  for (i in 2:dim(stations)[1]){
    ind<-which(X$station==stations$STAID[i])
    ts_jja<-ts(X[ind,var],start=1960,frequency=92)
    mm<- rollmean(ts_jja, k = 92+1, fill = NA)
    sintendencia <- ts_jja - mm 
    
    indest <- rep(c(1:92), length.out = length(ts_jja))
    sst <- tapply(sintendencia, INDEX = indest, FUN = mean, na.rm = TRUE)
    estst <- ts(rep(sst,length.out=length(sintendencia)),start=1960,frequency=92) #componente estacional
    
    dt<-ts_jja-estst #x_t-s_t
    mm4<-rollmean(dt,k=736,fill=NA)
    
    lines(mm4,col=stations$color[i])
  } 
  
}

setwd('C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Analisis Exploratorio/Tend_geop')
png("todas_tend_g300.png", width = 950, height = 650, res = 150)
tendencia_mm('g300',9.3,9.7)
dev.off()
png("todas_tend_g500.png", width = 950, height = 650, res = 150)
tendencia_mm('g500',5.7,5.95)
dev.off()
png("todas_tend_g700.png", width = 950, height = 650, res = 150)
tendencia_mm('g700',3.1,3.25)
dev.off()

#--------------COMP.ESTACIONAL--------------
library(dplyr)
library(tidyr)
library(lubridate)

comp_est <- function(var, l, u) {
  X <- X %>%
    mutate(day_month = format(Date, "%d-%m"))
  
  df_comp_est <- X %>%
    group_by(day_month, station) %>%
    summarise(mean_var = mean({{ var }}, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = station, values_from = mean_var)
  
  df_comp_est <- df_comp_est %>%
    mutate(fake_date = as.Date(paste0(day_month, "-2000"), format = "%d-%m-%Y")) %>%
    arrange(fake_date) %>%
    select(-fake_date)
  
  df_comp_est <- as.data.frame(df_comp_est)
  
  plot(1:92, df_comp_est[, 2], type = 'l', col = stations$color[1],
       xlab = 'Día', ylab = 'Altura (km)',
       main = paste('Estimación de la componente estacional', toupper(as.character(substitute(var)))),
       ylim = c(l, u))
  legend("top", legend = c("Costa Cantábrica", "Centro Peninsular", "Costa Mediterránea"),
         col = c("forestgreen", "red",'blue'), lwd = 1,ncol=3,cex=0.7)
  for (i in 2:nrow(stations)) {
    lines(1:92, df_comp_est[, i + 1], col = stations$color[i])
  }
}

png('comp_estacional_g300.png',width = 950, height = 650, res = 150)
comp_est(g300,9.3,9.7)
dev.off()
png('comp_estacional_g500.png',width = 950, height = 650, res = 150)
comp_est(g500,5.7,5.95)
dev.off()
png('comp_estacional_g700.png',width = 950, height = 650, res = 150)
comp_est(g700,3.1,3.25)
dev.off()

#--------------MAPA EVOLUCIÓN TEMPORAL--------------
limits <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Analisis Exploratorio/ModelosReg/interpolacion/limits.rds")
background <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Analisis Exploratorio/ModelosReg/interpolacion/background.rds")

library(sf)
library(sp)
library("rnaturalearth")
library("rnaturalearthdata")
library(tidyverse)
library(ggplot2)
library(dplyr)
library(ggpubr)

f_ola<-as.Date('2022-07-14')
f1<-f_ola-3
f2<-f_ola-2
f3<-f_ola-1
f4<-f_ola+1
f5<-f_ola+2
f6<-f_ola+3
f7<-f_ola+4


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
      guides(fill = guide_colorbar(barwidth = 2, barheight = 15*2))
  } else {
    p <- p + 
      scale_color_gradientn(colors = colores, limits = limites, name = nombre_ley, guide = "none")+
      theme(legend.position = "none")  # Ocultar leyenda si leyenda es FALSE
  }
  
  # Save the plot to a file
  return(p)
}

colores<-c('darkred',"orange",'darkgreen')
lat_lon <- X %>%
  separate(station_x, into = c("LAT", "LON"), sep = "_", convert = TRUE)
Lat<-lat_lon$LAT
Lon<-lat_lon$LON


mapas_ev_g<-function(var,l,u){
  m1<-mapSpain(X[which(X$Date==f1),var],XG=coord, coords=data.frame(Longitude = Lon[which(X$Date==f1)], Latitude = Lat[which(X$Date==f1)]),ref=0.05,
               leyenda=T,f1,colores,'Altura (km)',limites = c(l,u))
  m2<-mapSpain(X[which(X$Date==f2),var],XG=coord, coords=data.frame(Longitude = Lon[which(X$Date==f2)], Latitude = Lat[which(X$Date==f1)]),ref=0.05,
               leyenda=T,f2,colores,'Altura (km)',limites = c(l,u))
  m3<-mapSpain(X[which(X$Date==f3),var],XG=coord, coords=data.frame(Longitude = Lon[which(X$Date==f3)], Latitude = Lat[which(X$Date==f1)]),ref=0.05,
               leyenda=T,f3,colores,'Altura (km)',limites = c(l,u))
  m4<-mapSpain(X[which(X$Date==f_ola),var],XG=coord, coords=data.frame(Longitude = Lon[which(X$Date==f_ola)], Latitude = Lat[which(X$Date==f1)]),ref=0.05,
               leyenda=T,f_ola,colores,'Altura (km)',limites = c(l,u))
  m5<-mapSpain(X[which(X$Date==f4),var],XG=coord, coords=data.frame(Longitude = Lon[which(X$Date==f4)], Latitude = Lat[which(X$Date==f1)]),ref=0.05,
               leyenda=T,f4,colores,'Altura (km)',limites = c(l,u))
  m6<-mapSpain(X[which(X$Date==f5),var],XG=coord, coords=data.frame(Longitude = Lon[which(X$Date==f5)], Latitude = Lat[which(X$Date==f1)]),ref=0.05,
               leyenda=T,f5,colores,'Altura (km)',limites = c(l,u))
  m7<-mapSpain(X[which(X$Date==f6),var],XG=coord, coords=data.frame(Longitude = Lon[which(X$Date==f6)], Latitude = Lat[which(X$Date==f1)]),ref=0.05,
               leyenda=T,f6,colores,'Altura (km)',limites = c(l,u))
  m8<-mapSpain(X[which(X$Date==f7),var],XG=coord, coords=data.frame(Longitude = Lon[which(X$Date==f7)], Latitude = Lat[which(X$Date==f1)]),ref=0.05,
               leyenda=T,f7,colores,'Altura (km)',limites = c(l,u))
  
  m<-ggarrange(m1,m2,m3,m4,m5,m6,m7,m8,ncol=4,nrow=2,
               common.legend = T, legend = 'right')
  return(m)
}

m_g300<-mapas_ev_g('g300',9.5,9.75)
m_g500<-mapas_ev_g('g500',5.8,6)
m_g700<-mapas_ev_g('g700',3.15,3.28)
png("mapa_g300_ola.png", width = 3000, height = 1400, res = 300)
m_g300
dev.off()
png("mapa_g500_ola.png", width = 3000, height = 1400, res = 300)
m_g500
dev.off()
png("mapa_g700_ola.png", width = 3000, height = 1400, res = 300)
m_g700
dev.off()
