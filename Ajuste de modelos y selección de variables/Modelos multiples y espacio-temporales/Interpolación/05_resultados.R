rm(list=ls())

Y <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Y.rds")
pred_cuantil_q0.95_freq <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Analisis Exploratorio/ModelosReg/interpolacion/pred_cuantil_q0.95_freq.rds")
pred_cuantil_q0.5_freq <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Analisis Exploratorio/ModelosReg/interpolacion/pred_cuantil_q0.5_freq.rds")
grid <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Analisis Exploratorio/ModelosReg/interpolacion/grid.rds")
background <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Analisis Exploratorio/ModelosReg/interpolacion/background.rds")
limits <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Analisis Exploratorio/ModelosReg/interpolacion/limits.rds")

pred_cuantil_q0.95_def <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Analisis Exploratorio/ModelosReg/interpolacion/pred_cuantil_q0.95_def.rds")
pred_cuantil_q0.5_def <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Analisis Exploratorio/ModelosReg/interpolacion/pred_cuantil_q0.5_def.rds")



library("sf")
library("sp")
library("tidyverse")
library("rnaturalearth")
library("rnaturalearthdata")
library(lubridate)
library(ggpubr)
library(dplyr)
library(akima)
library(metR)

#------------------------MAPAS------------------------
#CAMBIAR ESCALAS Y LINEAS DE CURVA COMO SE DESEE
mapa_pred<-function(valores,grid,g,tipobeta,cuantil,esta0=NULL){
  mu<-colMeans(valores,na.rm=T)
  grid_coords <- cbind(st_coordinates(grid), mu = mu)
  grid_coords <- na.omit(grid_coords)
  
  if (!is.null(esta0)) {
    cruces_coords <- grid_coords[esta0[!is.na(esta0)], c("X", "Y")]
  }
  
  grid <- st_sf(mu = round(as.vector(mu), 3), geometry = grid)
  
  #curvas de nivel
  grid_interp <- akima::interp(x = grid_coords[,1],
                               y = grid_coords[,2],
                               z = grid_coords[,3],
                               duplicate = "mean")
  df_contour <- expand.grid(x = grid_interp$x, y = grid_interp$y)
  df_contour$z <- as.vector(grid_interp$z)
  df_contour <- na.omit(df_contour)
  
  #shapefile España
  spain <- ne_countries(scale = "large", country = "Spain", returnclass = "sf")
  spain <- st_transform(spain, st_crs(grid))  # Asegura que tenga el mismo CRS que tu grid

  #interseccion
  df_contour_sf <- st_as_sf(df_contour, coords = c("x", "y"), crs = st_crs(grid))
  df_contour_sf <- st_intersection(df_contour_sf, spain)
  df_contour_coords <- cbind(st_coordinates(df_contour_sf), z = df_contour_sf$z)

  #plot
  g<-ggplot(data = background) + 
    geom_sf(fill = "antiquewhite") + 
    xlab("Longitud (º)") + ylab("Latitud (º)") + ggtitle(bquote( .(g) * .(' (') *  tau *.(' = ') *.(cuantil) *.(')'))) +
    theme(panel.background = element_rect(fill = "aliceblue"),
          axis.text.x=element_text(size = 6),
          axis.text.y=element_text(size = 6, angle = 90),
          axis.title=element_text(size = 10, face = "bold")) + 
    geom_tile(data = grid_coords, aes(X, Y, fill = mu)) +
    #geom_tile(data = grid, ggplot2::aes(x = st_coordinates(grid)[, 1], y = st_coordinates(grid)[, 2], fill = mu)) +
    # scale_fill_gradient2( low = scales::muted("blue"), mid = "white", high = scales::muted("red"),
    #                       space = "Lab", midpoint = mean(mu), limits = range(mu), name = "Distance (km)") +
    # scale_fill_gradientn(
    #   colours = c("#00FF00", "#FFFF00", "#FFA500", "#FF0000", "#800080"),  # verde, amarillo, naranja, rojo, morado
    #   values = scales::rescale(c(min(mu), quantile(mu, 0.25), mean(mu), quantile(mu, 0.75), max(mu))),
    #   name = "Temperatura (ºC)",  # o "Predicción"
    #   limits = range(mu)
    # ) +
    # scale_fill_gradientn(
    #   colours = c("#00FFFF", "#00FF00", "#FFFF00", "#FFA500", "#FF0000", "#800080"),  # cian, verde, amarillo, naranja, rojo, morado
    #   values = scales::rescale(c(0, 10, 20, 30, 40, 48)),  # Ajustando el rango de valores
    #   name = "Temp. (ºC)",
    #   limits = c(0, 48)
    # ) +
    scale_fill_gradientn(
      colors = c('blue',"white", "red"),
      values = scales::rescale(c(-1,0, 1)),
      limits = c(-1, 1),  # Ajustando el rango de valores
      name = "Dif. Temp. (ºC)",
    ) +
    # geom_contour(data = df_contour_coords , aes(x = X, y = Y, z = z),
    #              color = "black", linewidth = 1, alpha = 0.8,
    #              breaks=seq(0,50,by=3)) +
    # geom_text_contour(data = df_contour_coords , aes(x = X, y = Y, z = z),
    #                   size = 3.5, stroke = 0.1, check_overlap = T,
    #                   color='black',fontface='bold',
    #                   breaks = seq(0,50,by=3),skip=0) +
    geom_contour(data = df_contour_coords , aes(x = X, y = Y, z = z),
                 color = "black", linewidth = 1, alpha = 0.8,
                 breaks=seq(-1,1,by=0.5)) +
    geom_text_contour(data = df_contour_coords , aes(x = X, y = Y, z = z),
                      size = 3.5, stroke = 0.1, check_overlap = T,
                      color='black',fontface='bold',
                      breaks = seq(-1,1,by=0.5),skip=0) +
    coord_sf(xlim = st_coordinates(limits)[, 1], ylim = st_coordinates(limits)[, 2]) 
  
  if(!is.null(esta0)){
    g<-g+ geom_point(data = cruces_coords, aes(x = X, y = Y), shape = 4, size = 0.4, stroke = 1, color = "black")
  }
  
  return(g)
}


#------------------------COMPARACION------------------------
f1<-as.character(unique(Y$Date)[2371])
f2<-as.character('2022-07-12')

# CUANTIL 0.50
m1<-mapa_pred(pred_cuantil_q0.5_freq[f1,],grid,paste('Predicciones',f1),'',0.5)
m2<-mapa_pred(pred_cuantil_q0.5_def[f1,],grid,paste('Predicciones',f1),'',0.5)
m3<-mapa_pred(pred_cuantil_q0.5_freq[f2,],grid,paste('Predicciones',f2),'',0.5)
m4<-mapa_pred(pred_cuantil_q0.5_def[f2,],grid,paste('Predicciones',f2),'',0.5)

m1<-m1 + guides(fill = guide_colorbar(barwidth = 15, barheight = 1))
m<-ggarrange(m1,m2,m3,m4,nrow=2,ncol=2, common.legend = T,
          legend='bottom')
png("comp_q0.5.png", width = 3000, height = 2400, res = 300)
print(m)
dev.off()
m

#diferencia
m1<-mapa_pred(pred_cuantil_q0.5_freq[f1,]-pred_cuantil_q0.5_def[f1,], grid, paste('Diferencias',f1),'',0.5)
m2<-mapa_pred(pred_cuantil_q0.5_freq[f2,]-pred_cuantil_q0.5_def[f2,], grid, paste('Diferencias',f2),'',0.5)
m1<-m1 + guides(fill = guide_colorbar(barwidth = 8, barheight = 1))
m<-ggarrange(m1,m2,nrow=2,ncol=, common.legend = T,
             legend='bottom')

png("comp_q0.5_dif.png", width = 3000/2, height = 2400, res = 300)
print(m)
dev.off()


# CUANTIL 0.95
m1<-mapa_pred(pred_cuantil_q0.95_freq[f1,],grid,paste('Predicciones',f1),'',0.95)
m2<-mapa_pred(pred_cuantil_q0.95_def[f1,],grid,paste('Predicciones',f1),'',0.95)
m3<-mapa_pred(pred_cuantil_q0.95_freq[f2,],grid,paste('Predicciones',f2),'',0.95)
m4<-mapa_pred(pred_cuantil_q0.95_def[f2,],grid,paste('Predicciones',f2),'',0.95)

m1<-m1 + guides(fill = guide_colorbar(barwidth = 15, barheight = 1))
m<-ggarrange(m1,m2,m3,m4,nrow=2,ncol=2, common.legend = T,
             legend='bottom')
png("comp_q0.95.png", width = 3000, height = 2400, res = 300)
print(m)
dev.off()
m

#diferencia
m1<-mapa_pred(pred_cuantil_q0.95_freq[f1,]-pred_cuantil_q0.95_def[f1,], grid, paste('Diferencias',f1),'',0.95)
m2<-mapa_pred(pred_cuantil_q0.95_freq[f2,]-pred_cuantil_q0.95_def[f2,], grid, paste('Diferencias',f2),'',0.95)
m1<-m1 + guides(fill = guide_colorbar(barwidth = 8, barheight = 1))
m<-ggarrange(m1,m2,nrow=2,ncol=, common.legend = T,
             legend='bottom')

png("comp_q0.95_dif.png", width = 3000/2, height = 2400, res = 300)
print(m)
dev.off()




#------------------------OLA DE CALOR------------------------
f_ola<-as.Date('2022-07-14')
f1<-as.character(f_ola-3)
f2<-as.character(f_ola-2)
f3<-as.character(f_ola-1)
f4<-as.character(f_ola+1)
f5<-as.character(f_ola+2)
f6<-as.character(f_ola+3)
f7<-as.character(f_ola+4)
f_ola<-as.character(f_ola)


mapa_ola_calor<-function(pred,cuantil){
  m1<-mapa_pred(pred[f1,],grid,paste(f1),'',cuantil)
  m2<-mapa_pred(pred[f2,],grid,paste(f2),'',cuantil)
  m3<-mapa_pred(pred[f3,],grid,paste(f3),'',cuantil)
  m_ola<-mapa_pred(pred[f_ola,],grid,paste(f_ola),'',cuantil)
  m4<-mapa_pred(pred[f4,],grid,paste(f4),'',cuantil)
  m5<-mapa_pred(pred[f5,],grid,paste(f5),'',cuantil)
  m6<-mapa_pred(pred[f6,],grid,paste(f6),'',cuantil)
  m7<-mapa_pred(pred[f7,],grid,paste(f7),'',cuantil)
  
  m1<-m1 + guides(fill = guide_colorbar(barwidth = 15, barheight = 1))
  m<-ggarrange(m1,m2,m3,m_ola,m4,m5,m6,m7,nrow=2,ncol=4, common.legend = T,
               legend='bottom')
  return(m)
}

m_ola_q0.5<-mapa_ola_calor(pred_cuantil_q0.5_def,0.50)
png("mapa_ola_q0.5.png", width = 3000*2, height = 1400*2, res = 300)
print(m_ola_q0.5)
dev.off()
m_ola_q0.5

m_ola_q0.95<-mapa_ola_calor(pred_cuantil_q0.95_def,0.95)
png("mapa_ola_q0.95.png", width = 3000*2, height = 1400*2, res = 300)
print(m_ola_q0.95)
dev.off()
m_ola_q0.95


#------------------------MESES/DECADAS------------------------
#PRIMERA DÉCADA
junio_1dec<- unique(Y$Date[year(Y$Date) >= 1960 
                           & year(Y$Date) <= 1969 
                           & month(Y$Date)=='6'])
julio_1dec<- unique(Y$Date[year(Y$Date) >= 1960 
                           & year(Y$Date) <= 1969 
                           & month(Y$Date)=='7'])
ag_1dec<- unique(Y$Date[year(Y$Date) >= 1960 
                           & year(Y$Date) <= 1969 
                           & month(Y$Date)=='8'])

junio_1dec<-as.character(junio_1dec)
julio_1dec<-as.character(julio_1dec)
ag_1dec<-as.character(ag_1dec)

pred_junio_1dec_q0.5<-t(colMeans(pred_cuantil_q0.5_def[junio_1dec,]))
pred_julio_1dec_q0.5<-t(colMeans(pred_cuantil_q0.5_def[julio_1dec,]))
pred_ag_1dec_q0.5<-t(colMeans(pred_cuantil_q0.5_def[ag_1dec,]))

pred_junio_1dec_q0.95<-t(colMeans(pred_cuantil_q0.95_def[junio_1dec,]))
pred_julio_1dec_q0.95<-t(colMeans(pred_cuantil_q0.95_def[julio_1dec,]))
pred_ag_1dec_q0.95<-t(colMeans(pred_cuantil_q0.95_def[ag_1dec,]))

#SEGUNDA DÉCADA
junio_2dec<- unique(Y$Date[year(Y$Date) >= 2014 
                           & year(Y$Date) <= 2023 
                           & month(Y$Date)=='6'])
julio_2dec<- unique(Y$Date[year(Y$Date) >= 2014
                           & year(Y$Date) <= 2023 
                           & month(Y$Date)=='7'])
ag_2dec<- unique(Y$Date[year(Y$Date) >= 2014 
                        & year(Y$Date) <= 2023 
                        & month(Y$Date)=='8'])

junio_2dec<-as.character(junio_2dec)
julio_2dec<-as.character(julio_2dec)
ag_2dec<-as.character(ag_2dec)

pred_junio_2dec_q0.5<-t(colMeans(pred_cuantil_q0.5_def[junio_2dec,]))
pred_julio_2dec_q0.5<-t(colMeans(pred_cuantil_q0.5_def[julio_2dec,]))
pred_ag_2dec_q0.5<-t(colMeans(pred_cuantil_q0.5_def[ag_2dec,]))

pred_junio_2dec_q0.95<-t(colMeans(pred_cuantil_q0.95_def[junio_2dec,]))
pred_julio_2dec_q0.95<-t(colMeans(pred_cuantil_q0.95_def[julio_2dec,]))
pred_ag_2dec_q0.95<-t(colMeans(pred_cuantil_q0.95_def[ag_2dec,]))


dif_junio_q0.5<-pred_junio_2dec_q0.5-pred_junio_1dec_q0.5
dif_junio_q0.95<-pred_junio_2dec_q0.95-pred_junio_1dec_q0.95

dif_julio_q0.5<-pred_julio_2dec_q0.5-pred_julio_1dec_q0.5
dif_julio_q0.95<-pred_julio_2dec_q0.95-pred_julio_1dec_q0.95

dif_ag_q0.5<-pred_ag_2dec_q0.5-pred_ag_1dec_q0.5
dif_ag_q0.95<-pred_ag_2dec_q0.95-pred_ag_1dec_q0.95

dif_cuant_junio<-dif_junio_q0.95-dif_junio_q0.5
dif_cuant_julio<-dif_julio_q0.95-dif_julio_q0.5
dif_cuant_ag<-dif_ag_q0.95-dif_ag_q0.5

summary(t(rbind(dif_junio_q0.5,dif_julio_q0.5,dif_ag_q0.5))) #0-2.5
summary(t(rbind(dif_junio_q0.95,dif_julio_q0.95,dif_ag_q0.95)))# 0-2.5
summary(t(rbind(dif_cuant_junio,dif_cuant_julio,dif_cuant_ag)))

#mapas
m1<-mapa_pred(dif_junio_q0.5,grid,
          'Junio','',0.50)
m2<-mapa_pred(dif_julio_q0.5,grid,
          'Julio','',0.50)
m3<-mapa_pred(dif_ag_q0.5,grid,
          'Agosto','',0.50,esta0)

m4<-mapa_pred(dif_junio_q0.95,grid,
          'Junio','',0.95)
m5<-mapa_pred(dif_julio_q0.95,grid,
          'Julio','',0.95)
m6<-mapa_pred(dif_ag_q0.95,grid,
          'Agosto','',0.95)

m7<-mapa_pred(dif_cuant_junio,grid,
              '','',0.95) +
  ggtitle('Diferencia junio')
m8<-mapa_pred(dif_cuant_julio,grid,
              '','',0.95)+
  ggtitle('Diferencia julio')
m9<-mapa_pred(dif_cuant_ag,grid,
              '','',0.95)+
  ggtitle('Diferencia agosto')

m1<-m1 + guides(fill = guide_colorbar(barwidth = 15, barheight = 1))
m<-ggarrange(m1,m2,m3,m4,m5,m6,nrow=2,ncol=3,
            common.legend=T,legend='bottom')
m
png("dif_dec_mes.png", width = 2400, height = 1800, res = 300)
print(m)
dev.off()

m7<-m7 + guides(fill = guide_colorbar(barwidth = 15, barheight = 1))
m<-ggarrange(m7,m8,m9,nrow=1,ncol=3,
             common.legend = T, legend='bottom')
png("dif_dec_mes_2.png", width = 2400, height = 1800/2, res = 300)
print(m)
dev.off()

#------------------------BLOCK AVERAGE------------------------
#TODA PENÍNSULA
fechas<-unique(Y$Date)
f1<-as.Date('2002-06-23')
mes_dia_f1<-format(f1,'%m-%d')

fechas_block_avg<-fechas[which(format(fechas,'%m-%d')==mes_dia_f1)]

fechas_block_avg<-as.character(fechas_block_avg)

pred_q0.5_block_avg<-pred_cuantil_q0.5_def[fechas_block_avg,]
pred_q0.95_block_avg<-pred_cuantil_q0.95_def[fechas_block_avg,]

block_avg_q0.5<-rowMeans(pred_q0.5_block_avg)#23-36
block_avg_q0.95<-rowMeans(pred_q0.95_block_avg)#28-40
block_avg<-data.frame(
  Año=unique(year(Y$Date)),
  block_avg_q0.5=block_avg_q0.5,
  block_avg_q0.95=block_avg_q0.95
)

plot(block_avg$Año,block_avg$block_avg_q0.5,type='l',ylim=c(20,40),
     xlab='Año',ylab='Temperatura (ºC)',main='Block Average Península Ibérica 23-06')
lines(block_avg$Año,block_avg$block_avg_q0.95,col='red')

mod1<-lm(block_avg_q0.5~Año,data=block_avg)
lines(block_avg$Año,predict(mod1,block_avg),lty=2)
mod2<-lm(block_avg_q0.95~Año, data=block_avg)
lines(block_avg$Año,predict(mod2,block_avg),col='red',lty=2)

#ARAGON
aragon <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Analisis Exploratorio/ModelosReg/interpolacion/aragon.rds")
indices_aragon <- which(st_within(grid, aragon, sparse = FALSE))

#comprobacion
mapa_pred(valores = pred_cuantil_q0.5_def['2002-06-23', indices_aragon], grid = grid[indices_aragon], 
          g = "Predicción en Aragón", tipobeta = '', cuantil = 0.5)

#BLOCK AVG DE ARAGON
block_avg_q0.5_aragon<-rowMeans(pred_q0.5_block_avg[,indices_aragon])
block_avg_q0.95_aragon<-rowMeans(pred_q0.95_block_avg[,indices_aragon])
block_avg_aragon<-data.frame(
  Año=unique(year(Y$Date)),
  block_avg_q0.5_aragon=block_avg_q0.5_aragon,
  block_avg_q0.95_aragon=block_avg_q0.95_aragon
)

plot(block_avg_aragon$Año,block_avg_aragon$block_avg_q0.5_aragon,type='l',ylim=c(20,40),
     xlab='Año',ylab='Temperatura (ºC)',main='Block average Aragón 23-06')
lines(block_avg_aragon$Año,block_avg_aragon$block_avg_q0.95_aragon,col='red')

mod1<-lm(block_avg_q0.5_aragon~Año,data=block_avg_aragon)
lines(block_avg_aragon$Año,predict(mod1,block_avg_aragon),lty=2)
mod2<-lm(block_avg_q0.95_aragon~Año, data=block_avg_aragon)
lines(block_avg_aragon$Año,predict(mod2,block_avg_aragon),col='red',lty=2)

# GALICIA
galicia <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Analisis Exploratorio/ModelosReg/interpolacion/galicia.rds")
indices_galicia <- which(st_within(grid, galicia, sparse = FALSE))

#comprobacion
mapa_pred(valores = pred_cuantil_q0.5_def['2002-06-23', indices_galicia], grid = grid[indices_galicia], 
          g = "Predicción en Aragón", tipobeta = '', cuantil = 0.5)

#BLOCK AVG DE galicia
block_avg_q0.5_galicia<-rowMeans(pred_q0.5_block_avg[,indices_galicia])
block_avg_q0.95_galicia<-rowMeans(pred_q0.95_block_avg[,indices_galicia])
block_avg_galicia<-data.frame(
  Año=unique(year(Y$Date)),
  block_avg_q0.5_galicia=block_avg_q0.5_galicia,
  block_avg_q0.95_galicia=block_avg_q0.95_galicia
)

plot(block_avg_galicia$Año,block_avg_galicia$block_avg_q0.5_galicia,type='l',ylim=c(20,40),
     xlab='Año',ylab='Temperatura (ºC)',main='Block average Aragón 23-06')
lines(block_avg_galicia$Año,block_avg_galicia$block_avg_q0.95_galicia,col='red')

mod1<-lm(block_avg_q0.5_galicia~Año,data=block_avg_galicia)
lines(block_avg_galicia$Año,predict(mod1,block_avg_galicia),lty=2)
mod2<-lm(block_avg_q0.95_galicia~Año, data=block_avg_galicia)
lines(block_avg_galicia$Año,predict(mod2,block_avg_galicia),col='red',lty=2)


# GUARDADO IMÁGENES
png('block_avg.png',width = 1300,height = 600, res = 150)
par(mfrow=c(1,2))

plot(block_avg_galicia$Año,block_avg_galicia$block_avg_q0.5_galicia,type='l',ylim=c(15,40),
     xlab='Año',ylab='Temperatura (ºC)',main='Block average Galicia 23-06')
lines(block_avg_galicia$Año,block_avg_galicia$block_avg_q0.95_galicia,col='red')
mod1<-lm(block_avg_q0.5_galicia~Año,data=block_avg_galicia)
lines(block_avg_galicia$Año,predict(mod1,block_avg_galicia),lty=2)
mod2<-lm(block_avg_q0.95_galicia~Año, data=block_avg_galicia)
lines(block_avg_galicia$Año,predict(mod2,block_avg_galicia),col='red',lty=2)

plot(block_avg_aragon$Año,block_avg_aragon$block_avg_q0.5_aragon,type='l',ylim=c(15,40),
     xlab='Año',ylab='Temperatura (ºC)',main='Block average Aragón 23-06')
lines(block_avg_aragon$Año,block_avg_aragon$block_avg_q0.95_aragon,col='red')

mod1<-lm(block_avg_q0.5_aragon~Año,data=block_avg_aragon)
lines(block_avg_aragon$Año,predict(mod1,block_avg_aragon),lty=2)
mod2<-lm(block_avg_q0.95_aragon~Año, data=block_avg_aragon)
lines(block_avg_aragon$Año,predict(mod2,block_avg_aragon),col='red',lty=2)

dev.off()

#diferencias
png('block_avg_dif.png',width = 900*1.1,height = 600*1.1, res = 150)
par(mfrow=c(1,1))
plot(block_avg_aragon$Año,block_avg_aragon$block_avg_q0.95_aragon-block_avg_aragon$block_avg_q0.5_aragon,
     type='l',ylim=c(2,6), xlab='Año',ylab='Temperatura (ºC)', main='Diferencia entre cuantiles')
lines(block_avg_galicia$Año,block_avg_galicia$block_avg_q0.95_galicia-block_avg_galicia$block_avg_q0.5_galicia,type='l',col='red')
legend("top", legend = c("Aragón", "Galicia"),
       col = c("black", "red"), lwd = 2,horiz=T)
dev.off()
#extra
library(RcmdrMisc)
numSummary(block_avg$block_avg_q0.95-block_avg$block_avg_q0.5)
numSummary(block_avg_aragon$block_avg_q0.95_aragon-block_avg_aragon$block_avg_q0.5_aragon)
numSummary(block_avg$block_avg_q0.5)
numSummary(block_avg$block_avg_q0.95)
numSummary(block_avg_aragon$block_avg_q0.5_aragon)
numSummary(block_avg_aragon$block_avg_q0.95_aragon)
# hay mucha variabilidad

boxplot(cbind(block_avg[,-1],block_avg_aragon[,-1]))

#------------------------BLOCK AVERAGE MESES------------------------
#TODA PENÍNSULA
fechas<-unique(Y$Date)

#JUNIO
fechas_block_avg<-fechas[which(format(fechas,'%m')=='06')]

fechas_block_avg<-as.character(fechas_block_avg)

pred_q0.5_block_avg<-pred_cuantil_q0.5_def[fechas_block_avg,]
pred_q0.95_block_avg<-pred_cuantil_q0.95_def[fechas_block_avg,]

year_index <- rep(1:64, each = 30)
block_avg_q0.5_jun<-rowMeans(pred_q0.5_block_avg)#23-36
block_avg_q0.5_jun <- tapply(block_avg_q0.5_jun , year_index, mean)
block_avg_q0.95_jun<-rowMeans(pred_q0.95_block_avg)#28-40
block_avg_q0.95_jun <- tapply(block_avg_q0.95_jun , year_index, mean)

block_avg_jun <-data.frame(
  Año=unique(year(Y$Date)),
  block_avg_q0.5=block_avg_q0.5_jun ,
  block_avg_q0.95=block_avg_q0.95_jun 
)


#JULIO
fechas_block_avg<-fechas[which(format(fechas,'%m')=='07')]

fechas_block_avg<-as.character(fechas_block_avg)

pred_q0.5_block_avg<-pred_cuantil_q0.5_def[fechas_block_avg,]
pred_q0.95_block_avg<-pred_cuantil_q0.95_def[fechas_block_avg,]

year_index <- rep(1:64, each = 31)
block_avg_q0.5_jul<-rowMeans(pred_q0.5_block_avg)#23-36
block_avg_q0.5_jul <- tapply(block_avg_q0.5_jul , year_index, mean)
block_avg_q0.95_jul<-rowMeans(pred_q0.95_block_avg)#28-40
block_avg_q0.95_jul <- tapply(block_avg_q0.95_jul , year_index, mean)

block_avg_jul <-data.frame(
  Año=unique(year(Y$Date)),
  block_avg_q0.5=block_avg_q0.5_jul ,
  block_avg_q0.95=block_avg_q0.95_jul 
)


#AGOSTO
fechas_block_avg<-fechas[which(format(fechas,'%m')=='08')]

fechas_block_avg<-as.character(fechas_block_avg)

pred_q0.5_block_avg<-pred_cuantil_q0.5_def[fechas_block_avg,]
pred_q0.95_block_avg<-pred_cuantil_q0.95_def[fechas_block_avg,]

year_index <- rep(1:64, each = 31)
block_avg_q0.5_ag<-rowMeans(pred_q0.5_block_avg)#23-36
block_avg_q0.5_ag <- tapply(block_avg_q0.5_ag , year_index, mean)
block_avg_q0.95_ag<-rowMeans(pred_q0.95_block_avg)#28-40
block_avg_q0.95_ag <- tapply(block_avg_q0.95_ag , year_index, mean)

block_avg_ag <-data.frame(
  Año=unique(year(Y$Date)),
  block_avg_q0.5=block_avg_q0.5_ag ,
  block_avg_q0.95=block_avg_q0.95_ag 
)

#PLOT
png('block_avg_meses.png',width = 1300*1.2,height = 600*1.2, res = 150)
par(mfrow=c(1,2))
plot(block_avg_jun$Año,block_avg_jun$block_avg_q0.5,type='l',ylim=c(20,40),
     xlab='Año',ylab='Temperatura (ºC)',
     main=bquote(.('Block Average Pen. Ib. por meses')* .(' (') * tau *.(' = ') * .(0.50) * .(')')),col='red')
lines(block_avg_jul$Año,block_avg_jul$block_avg_q0.5,col='blue')
lines(block_avg_ag$Año,block_avg_ag$block_avg_q0.5,col='darkgreen')

mod1<-lm(block_avg_q0.5~Año,data=block_avg_jun)
lines(block_avg$Año,predict(mod1,block_avg_jun),lty=2,col='red')
mod3<-lm(block_avg_q0.5~Año,data=block_avg_jul)
lines(block_avg$Año,predict(mod3,block_avg_jul),lty=2,col='blue')
mod5<-lm(block_avg_q0.5~Año,data=block_avg_ag)
lines(block_avg$Año,predict(mod5,block_avg_ag),lty=2,col='darkgreen')
legend("top", legend = c("Junio", "Julio", 'Agosto'),
       col = c("red", "blue",'darkgreen'), lwd = 2,horiz=T)

plot(block_avg_jun$Año,block_avg_jun$block_avg_q0.95,type='l',ylim=c(20,40),
     xlab='Año',ylab='Temperatura (ºC)',
     main=bquote(.('Block Average Pen. Ib. por meses')* .(' (') * tau *.(' = ') * .(0.95) * .(')')),col='red')

lines(block_avg_jul$Año,block_avg_jul$block_avg_q0.95,col='blue')
lines(block_avg_ag$Año,block_avg_ag$block_avg_q0.95,col='darkgreen')
mod2<-lm(block_avg_q0.95~Año, data=block_avg_jun)
lines(block_avg$Año,predict(mod2,block_avg_jun),col='red',lty=2)
mod4<-lm(block_avg_q0.95~Año, data=block_avg_jul)
lines(block_avg$Año,predict(mod4,block_avg_jul),col='blue',lty=2)
mod6<-lm(block_avg_q0.95~Año, data=block_avg_ag)
lines(block_avg$Año,predict(mod6,block_avg_ag),col='darkgreen',lty=2)
legend("top", legend = c("Junio", "Julio", 'Agosto'),
       col = c("red", "blue",'darkgreen'), lwd = 2,horiz=T)

dev.off()

#diferencias
png('block_avg_meses_dif.png',width = 900*1.1,height = 600*1.1, res = 150)
par(mfrow=c(1,1))
plot(block_avg_jun$Año,block_avg_jun$block_avg_q0.95-block_avg_jun$block_avg_q0.5,
     type='l',col='red',ylim=c(3,4),xlab='Año',ylab='Temperatura (ºC)',main = 'Diferencia entre cuantiles')
lines(block_avg_jul$Año,block_avg_jul$block_avg_q0.95-block_avg_jul$block_avg_q0.5,col='blue')
lines(block_avg_ag$Año,block_avg_ag$block_avg_q0.95-block_avg_ag$block_avg_q0.5,col='darkgreen')
legend("top", legend = c("Junio", "Julio", 'Agosto'),
       col = c("red", "blue",'darkgreen'), lwd = 2,horiz=T)
dev.off()


#------------------------MESES/DECADAS INCERTIDUMBRE------------------------
library(qs)
#EXIGENTE COMPUTACIONALMENTE
dif_cuantil_q0.5_jun<-qread('C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Analisis Exploratorio/ModelosReg/interpolacion/dif_cuantil_q0.5_jun.qs')
dif_cuantil_q0.5_jul<-qread('C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Analisis Exploratorio/ModelosReg/interpolacion/dif_cuantil_q0.5_jul.qs')
dif_cuantil_q0.5_ag<-qread('C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Analisis Exploratorio/ModelosReg/interpolacion/dif_cuantil_q0.5_ag.qs')
dif_cuantil_q0.95_jun<-qread('C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Analisis Exploratorio/ModelosReg/interpolacion/dif_cuantil_q0.95_jun.qs')
dif_cuantil_q0.95_jul<-qread('C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Analisis Exploratorio/ModelosReg/interpolacion/dif_cuantil_q0.95_jul.qs')
dif_cuantil_q0.95_ag<-qread('C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Analisis Exploratorio/ModelosReg/interpolacion/dif_cuantil_q0.95_ag.qs')


cuantil_2000<-function(dif){
  index<-rep(1:2000,times=nrow(dif)/2000)
  suma<-rowsum(dif,group=index)
  media<-suma/(nrow(dif)/2000)

  
  IC<-apply(media,2,FUN = quantile, probs=c(0.025,0.975))
  esta0<-IC[1,]<=0 & IC[2,]>=0
  
  return(esta0)
}

esta0_q0.5_jun<-cuantil_2000(dif_cuantil_q0.5_jun)
esta0_q0.5_jul<-cuantil_2000(dif_cuantil_q0.5_jul)
esta0_q0.5_ag<-cuantil_2000(dif_cuantil_q0.5_ag)

esta0_q0.95_jun<-cuantil_2000(dif_cuantil_q0.95_jun)
esta0_q0.95_jul<-cuantil_2000(dif_cuantil_q0.95_jul)
esta0_q0.95_ag<-cuantil_2000(dif_cuantil_q0.95_ag)


m1<-mapa_pred(dif_junio_q0.5,grid,
              'Junio','',0.50,esta0_q0.5_jun)
m2<-mapa_pred(dif_julio_q0.5,grid,
              'Julio','',0.50,esta0_q0.5_jul)
m3<-mapa_pred(dif_ag_q0.5,grid,
              'Agosto','',0.50,esta0_q0.5_ag)
m4<-mapa_pred(dif_junio_q0.95,grid,
              'Junio','',0.95,esta0_q0.95_jun)
m5<-mapa_pred(dif_julio_q0.95,grid,
              'Julio','',0.95,esta0_q0.95_jul)
m6<-mapa_pred(dif_ag_q0.95,grid,
              'Agosto','',0.95,esta0_q0.95_ag)

m1<-m1 + guides(fill = guide_colorbar(barwidth = 15, barheight = 1))
m<-ggarrange(m1,m2,m3,m4,m5,m6,nrow=2,ncol=3,
             common.legend=T,legend='bottom')
m
png("dif_dec_mes_2000.png", width = 2400, height = 1800, res = 300)
print(m)
dev.off()

esta0_dif_jun<-cuantil_2000(dif_cuantil_q0.95_jun-dif_cuantil_q0.5_jun)
esta0_dif_jul<-cuantil_2000(dif_cuantil_q0.95_jul-dif_cuantil_q0.5_jul)
esta0_dif_ag<-cuantil_2000(dif_cuantil_q0.95_ag-dif_cuantil_q0.5_ag)

m7<-mapa_pred(dif_cuant_junio,grid,
              '','',0.95,esta0_dif_jun) +
  ggtitle('Diferencia junio')
m8<-mapa_pred(dif_cuant_julio,grid,
              '','',0.95,esta0_dif_jul)+
  ggtitle('Diferencia julio')
m9<-mapa_pred(dif_cuant_ag,grid,
              '','',0.95,esta0_dif_ag)+
  ggtitle('Diferencia agosto')
m7<-m7 + guides(fill = guide_colorbar(barwidth = 15, barheight = 1))
m<-ggarrange(m7,m8,m9,nrow=1,ncol=3,
             common.legend = T, legend='bottom')
png("dif_dec_mes_2_2000.png", width = 2400, height = 1800/2, res = 300)
print(m)
dev.off()
