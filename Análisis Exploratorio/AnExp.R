# ANALISIS EXPLORATORIO
rm(list=ls())
library(tseries)
library(zoo)
library(forecast)
library(RcmdrMisc)
library(readr)

setwd("C:/Users/jumar/Desktop/TFM/Datos")
data <- read_csv("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Tx_mat.csv")
stations <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/stations.rds")

length(which(is.na(data)))
#------------------------SERIE JJA------------------------
library(lubridate)
library(dplyr)
library(tidyr)

data$fecha <- as.Date(data$Date, format = "%Y-%m-%d")
df <- data %>%
  filter(month(fecha) >= 6 & month(fecha) <= 8)
df_filled <- df
for (col_name in names(df_filled)[-1]){
  df_filled[[col_name]] <- na.approx(df[[col_name]],rule=2)
}
# periodos 32 años
p1<- df %>%
  filter(year(fecha)>=1960 & year(fecha)<=1991)
p2<- df %>%
  filter(year(fecha)>=1992)

data$fecha <- NULL
df$fecha <- NULL
df_filled$fecha<-NULL
p1$fecha <- NULL
p2$fecha <- NULL
which(is.na(df_filled))




#------------------------ANALISIS DE UNA SOLA ESTACION (PRUEBA)------------------------
ts<-ts(data$'229'/10,start=1960,frequency = 365)
plot(ts)
mu<-mean(ts)
abline(h=mu,col=2)

plot(ts,xlim=c(2000,2005))

ggseasonplot(ts)
#no parece haber una tendencia clara 
kpss.test(ts) #se rechaza hipotesis de estaiconariedad




#------------------------RELLENO NULOS Y ANALISIS DE TENDENCIA PARA LAS SERIES ENTERAS------------------------
library(KFAS)
library(gdata)

#tendencia y componente estacional (A MANO) aqui puede haber nulos
mm<- rollmean(ts, k = 365, fill = NA)
plot(ts)
lines(mm,col='red')
sintendencia <- ts - mm 

indest <- rep(c(1:365), length.out = length(ts))
sst <- tapply(sintendencia, INDEX = indest, FUN = mean, na.rm = TRUE)
estst <- ts(rep(sst,length.out=length(sintendencia)),start=1960,frequency=365) #componente estacional
plot(sintendencia,,main='Serie sin tendencia y estimación componente estacional')
lines(estst,col='red')

res <- sintendencia - estst 
plot(res)

par(mfrow=c(1,1))
dt<-ts-estst #x_t-s_t
mm2<-rollmean(dt,k=365,fill=NA)
mm3<-rollmean(dt,k=600,fill=NA)
mm4<-rollmean(dt,k=900,fill=NA)
#segunda aproximacion de la tendencia
plot(dt,xlab='Tiempo (meses)',ylab='Temperatura (°C x10)', main='Estimaciones de tendencia')
lines(mm2,col=2,lwd=2)
lines(mm3,col=3,lwd=2)
lines(mm4,col=4,lwd=2)
legend("topleft", 
       legend = c('MM k=365','MM k=600','MM k=900'),col = c(2,3,4),lty = 1,cex=0.45,xpd=T)
abline(h=23,col='purple')

# se puede obervar una tendencia positiva
plot(ts-mm3-estst, xlab="Tiempo (meses)", ylab="Diferencia",main="Diferencia de la serie original y estimaciones")
abline(h=0,col=2,lwd=2)
kpss.test(ts-mm3-estst)

#con stl
par(mfrow=c(1,2))
tsstl<-stl(ts,s.window='periodic')
plot(mm3,ylab='Temperatura (°C x10)',xlab='Tiempo(meses)',main='Tendencia',col=2)
lines(tsstl$time.series[,2],lwd=1)

plot(estst,ylab='Temperatura (°C x10)',xlab='Tiempo(meses)',main='Comp. estacional',col=2)
lines(tsstl$time.series[,1],col=1,lwd=1)

par(mfrow=c(1,1))
plot(ts,main='Estimacion tendencia X229')
lines(tsstl$time.series[,2],col='red',lwd=2)
abline(h=23,col='blue')

#todas tendencias stl no deja valores nulos
plot(tsstl$time.series[,2],ylim=c(0,50),main='Estimaciones de tendencias por sitio',
     ylab='Temperatura (ºC)',col=stations$color[2-1],xlab='Año')
legend("topright", legend = c("Costa Cantábrica", "Centro Peninsular", "Costa Mediterránea"),
       col = c("forestgreen", "red",'blue'), lwd = 2)
data_filled <- data
ind<-2
aux<-mean(tsstl$time.series[,2]) #para ver cual tiene la tendencia con menores valores

for (col_name in names(data_filled)[-1]){
  data_filled[[col_name]] <- na.approx(data[[col_name]],rule=2) #relleno nulos
}
n<-length(data)
for (i in 3:n){
  datos <- data_filled[[i]]/10
  ts <- ts(datos, start = 1960, frequency = 365)
  tsstl<-stl(ts,s.window='periodic')
  lines(tsstl$time.series[,2],lwd=1,col=stations$color[i-1])
  mu <- mean(tsstl$time.series[,2])
  if(mu<aux){
    aux<-mu
    ind<-i
  }
}



ind
names(data)[ind]
stations$HGHT[stations$STAID==names(data)[ind]]
max(stations$HGHT) # LA QUE MÁs ELEVACIÓN TIENE 

#serie JJA
#STL
par(mfrow=c(1,1))
ts_jja<-ts(df_filled[[2]]/10,start=1960,frequency=92)

tsstl_jja<-stl(ts_jja,s.window='periodic')

plot(tsstl_jja$time.series[,2],ylim=c(15,40),main='Estimaciones de tendencias por sitio JJA (LOESS)',
     ylab='Temperatura (ºC)',xlab='Año',col='red')
legend("top", legend = c("Costa Cantábrica", "Centro Peninsular", "Costa Mediterránea"),
       col = c("forestgreen", "red",'blue'), lwd = 2,ncol=3)
ind<-2
aux<-mean(tsstl_jja$time.series[,2]) #para ver cual tiene la tendencia con menores valores

n<-length(df)
for (i in 3:n){
  datos <- df_filled[[i]]/10
  ts <- ts(datos, start = 1960, frequency = 92)
  tsstl<-stl(ts,s.window='periodic')
  lines(tsstl$time.series[,2],lwd=1,col=stations$color[i-1])
  mu <- mean(tsstl$time.series[,2])
  if(mu<aux){
    aux<-mu
    ind<-i
  }
}

#para guardar imagen
setwd('C:/Users/jumar/Desktop/TFM/Datos/Analisis Exploratorio/Tend_Cuant')

png("todas_tend_jja.png", width = 950, height = 650, res = 150)
plot(tsstl_jja$time.series[,2],ylim=c(15,40),main='Estimaciones de tendencias por sitio JJA (LOESS)',
     ylab='Temperatura (ºC)',xlab='Año',col='red')
legend("top", legend = c("Costa Cantábrica", "Centro Peninsular", "Costa Mediterránea"),
       col = c("forestgreen", "red",'blue'), lwd = 1,ncol=3,cex=0.7)
ind<-2
aux<-mean(tsstl_jja$time.series[,2]) #para ver cual tiene la tendencia con menores valores

n<-length(df)
for (i in 3:n){
  datos <- df_filled[[i]]/10
  ts <- ts(datos, start = 1960, frequency = 92)
  tsstl<-stl(ts,s.window='periodic')
  lines(tsstl$time.series[,2],lwd=1,col=stations$color[i-1])
  mu <- mean(tsstl$time.series[,2])
  if(mu<aux){
    aux<-mu
    ind<-i
  }
}
dev.off()

#MEDIAS MOVILES CON VENTANA AMPLIA PARA UNA TENDENCIA SUAVE. (no hace falta filled)
#VENTANA DE K=92*8=736

setwd('C:/Users/jumar/Desktop/TFM/Datos/Analisis Exploratorio/Tend_Cuant')
png("todas_tend_jja_mm.png", width = 950, height = 650, res = 150)
ts_jja<-ts(df_filled[[2]]/10,start=1960,frequency=92)
mm<- rollmean(ts_jja, k = 92+1, fill = NA)
sintendencia <- ts_jja - mm 

indest <- rep(c(1:92), length.out = length(ts_jja))
sst <- tapply(sintendencia, INDEX = indest, FUN = mean, na.rm = TRUE)
estst <- ts(rep(sst,length.out=length(sintendencia)),start=1960,frequency=92) #componente estacional

dt<-ts_jja-estst #x_t-s_t
mm4<-rollmean(dt,k=736,fill=NA)
#segunda aproximacion de la tendencia
plot(mm4,ylim=c(15,40),main='Estimación de tendencias por sitio JJA (medias móviles)',
     ylab='Temperatura (ºC)',xlab='Año',col=stations$color[2-1])

legend("top", legend = c("Costa Cantábrica", "Centro Peninsular", "Costa Mediterránea"),
       col = c("forestgreen", "red",'blue'), lwd = 1,ncol=3,cex=0.7)

n<-length(df)
for (i in 3:n){
  ts_jja<-ts(df_filled[[i]]/10,start=1960,frequency=92)
  mm<- rollmean(ts_jja, k = 92+1, fill = NA)
  sintendencia <- ts_jja - mm 
  
  indest <- rep(c(1:92), length.out = length(ts_jja))
  sst <- tapply(sintendencia, INDEX = indest, FUN = mean, na.rm = TRUE)
  estst <- ts(rep(sst,length.out=length(sintendencia)),start=1960,frequency=92) #componente estacional
  
  dt<-ts_jja-estst #x_t-s_t
  mm4<-rollmean(dt,k=736,fill=NA)
  
  lines(mm4,col=stations$color[i-1])
} 

dev.off()

# ggplot
# library(ggplot2)
# 
# n_stations <- 40
# 
# # Lista para almacenar los datos procesados
# data_list <- list()
# 
# for (i in 2:(n_stations + 1)) {
#   ts_jja <- ts(df_filled[[i]] / 10, start = 1960, frequency = 92)
#   mm <- rollmean(ts_jja, k = 92 + 1, fill = NA)
#   sintendencia <- ts_jja - mm 
#   
#   indest <- rep(1:92, length.out = length(ts_jja))
#   sst <- tapply(sintendencia, INDEX = indest, FUN = mean, na.rm = TRUE)
#   estst <- ts(rep(sst, length.out = length(sintendencia)), start = 1960, frequency = 92)
#   
#   dt <- ts_jja - estst
#   mm4 <- rollmean(dt, k = 736, fill = NA)
#   
#   years <- seq(1960, length.out = length(mm4), by = 1 / 92)
#   df_temp <- data.frame(Year = years, Temperature = mm4, Station = factor(i - 1))
#   data_list[[i - 1]] <- df_temp
# }
# 
# # Combinar los datos en un solo dataframe
# data_final <- bind_rows(data_list)
# 
# # Graficar con ggplot2
# ggplot(data_final, aes(x = Year, y = Temperature, group = Station, color = Station)) +
#   geom_line(alpha = 0.75) +
#   scale_color_manual(values = stations$color[1:n_stations]) +
#   labs(title = "Estimación de tendencias por sitio JJA (medias móviles)",
#        x = "Año", y = "Temperatura (ºC)") +
#   theme_minimal()+
#   guides(color='none')




#------------------------COMPONENTE ESTACIONAL------------------------

day_month <- unique(format(df$Date, "%d-%m"))

df_comp_est<-matrix(NA,nrow=length(day_month),ncol=40)
df_comp_est<-as.data.frame(df_comp_est,row.names = day_month)
colnames(df_comp_est)<-stations$STAID

for (i in 1:length(day_month)){
  ind<-which(format(df$Date, "%d-%m") == day_month[i])
  df_comp_est[i,]<-colMeans(df[ind,-1],na.rm=T)/10
}

setwd('C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Analisis Exploratorio/Tend_Cuant')
png('comp_estacional_dias.png',width = 950, height = 650, res = 150)
plot(1:92,df_comp_est[,1],type='l',col=stations$color[1],
     xlab='Día',ylab='Temperatura (ºC)',ylim=c(15,40),
     main = 'Estimación de la componente estacional')

for (i in 2:40){
  lines(1:92,df_comp_est[,i],col=stations$color[i])
}

legend("top", legend = c("Costa Cantábrica", "Centro Peninsular", "Costa Mediterránea"),
       col = c("forestgreen", "red",'blue'), lwd = 1,ncol=3,cex=0.7)
dev.off()

#------------------------CUANTILES EMPIRICOS (para dos primeras deja nulos)------------------------
#toda serie
cuantiles <- c(0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95)
cuant_emp <- apply(data[,-1]/10, 2, quantile,na.rm=T, probs = cuantiles)

head(cuant_emp)

boxplot(t(cuant_emp), names = cuantiles, main = "Distribución de los cuantiles empíricos por sitio",
        xlab = "Cuantiles", ylab = "Temperatura", las = 2)

iqr_values <- apply(t(cuant_emp), 2, function(x) IQR(x, na.rm = TRUE))
iqr_values

median(cuant_emp[11,])-median(cuant_emp[1,])

# serie JJA
par(mfrow=c(1,2))
cuant_emp <- apply(df[,-1]/10, 2, quantile,na.rm=T, probs = cuantiles)
cuant_emp<-as.data.frame(cuant_emp, row.names = cuantiles)
boxplot(t(cuant_emp), names = cuantiles, main = "Cuantiles",
        xlab = "Cuantil", ylab = "Temperatura (ºC)", las = 2)

#variabilidad entre cuantiles empiricos en zaragoza 0.5 y 0.95
ind<-which(stations$NAME2=='Zaragoza')
stations$STAID[ind]
q0.05<-cuant_emp[c('0.05'),ind]
q0.5<-cuant_emp[c('0.5'),ind]
q0.95<-cuant_emp[c('0.95'),ind]

lines(x=c(1,6,11),y=c(q0.05,q0.5,q0.95),col='red',type='b',pch=19,lwd=2)

#
iqr_values <- apply(t(cuant_emp), 2, function(x) IQR(x, na.rm = TRUE))
iqr_values

rango<- apply(t(cuant_emp), 2, function(x) max(x)-min(x))
rango

median(cuant_emp[11,])-median(cuant_emp[1,])



# DIFERENCIA ENTRE PERIODOS Y ZONAS
centro<-which(stations$color=='red')+1 #por fecha
med<-which(stations$color=='blue')+1
cant<-which(stations$color=='forestgreen')+1
cuant_emp_p1 <- apply(p1[,-1]/10, 2, quantile,na.rm=T, probs = cuantiles)
cuant_emp_p2 <- apply(p2[,-1]/10, 2, quantile,na.rm=T, probs = cuantiles)
cuant_emp_p1_centro <- apply(p1[,centro]/10, 2, quantile,na.rm=T, probs = cuantiles)
cuant_emp_p2_centro <- apply(p2[,centro]/10, 2, quantile,na.rm=T, probs = cuantiles)
cuant_emp_p1_cant <- apply(p1[,cant]/10, 2, quantile,na.rm=T, probs = cuantiles)
cuant_emp_p2_cant <- apply(p2[,cant]/10, 2, quantile,na.rm=T, probs = cuantiles)
cuant_emp_p1_med <- apply(p1[,med]/10, 2, quantile,na.rm=T, probs = cuantiles)
cuant_emp_p2_med <- apply(p2[,med]/10, 2, quantile,na.rm=T, probs = cuantiles)


dif_cuant<-cuant_emp_p2-cuant_emp_p1
dif_cuant<-as.data.frame(dif_cuant, row.names = cuantiles)

dif_cuant_centro<-cuant_emp_p2_centro-cuant_emp_p1_centro
dif_cuant_centro<-as.data.frame(dif_cuant_centro, row.names = cuantiles)
dif_cuant_med<-cuant_emp_p2_med-cuant_emp_p1_med
dif_cuant_med<-as.data.frame(dif_cuant_med, row.names = cuantiles)
dif_cuant_cant<-cuant_emp_p2_cant-cuant_emp_p1_cant
dif_cuant_cant<-as.data.frame(dif_cuant_cant, row.names = cuantiles)

boxplot(t(dif_cuant),main='Diferencia cuantiles', names =cuantiles, 
        xlab='Cuantil', ylab='Temperatura (ºC)')


library(vioplot)
vioplot(t(dif_cuant[c(6,11),]), main = 'Diferencia Cuantiles', names = c(0.5,0.95),
        xlab = 'Cuantil', ylab = 'Temperatura (ºC)',col='grey')


#boxplot diferencia por zonas
dif_todos<-dif_cuant[c('0.95'),]-dif_cuant[c('0.5'),]
dif_cant<-dif_cuant_cant[c('0.95'),]-dif_cuant_cant[c('0.5'),]
dif_med<-dif_cuant_med[c('0.95'),]-dif_cuant_med[c('0.5'),]
dif_centro<-dif_cuant_centro[c('0.95'),]-dif_cuant_centro[c('0.5'),]

diferencias_q0.5_q0.95<-list(
  unlist(as.vector(dif_todos)),
  unlist(as.vector(dif_centro)),
  unlist(as.vector(dif_med)),
  unlist(as.vector(dif_cant))
  )

par(mfrow=c(1,1))
b<-boxplot(diferencias_q0.5_q0.95,main='Diferencia cuantiles', 
        names =c('Todos','Centro peninsular','Costa mediterránea','Costa cantábrica'), 
        ylab='Temperatura (ºC)',col=c('grey','red','blue','forestgreen'))
b$out[3:4]


png("dif_zonas.png", width =850*5/3, height = 600*5/3, res = 150)
boxplot(diferencias_q0.5_q0.95,main='Diferencia de diferencia de cuantiles 0.50 y 0.95', 
        names =c('Todos','Centro peninsular','Costa mediterránea','Costa cantábrica'), 
        ylab='Temperatura (ºC)',col=c('grey','red','blue','forestgreen'))
dev.off()


# autocorrelacion entre cuantiles. (se usan los datos rellenos)

df_lag <- df_filled %>%
  mutate(across(-Date, lag, .names = "X_{.col}"))

df_lag <- df_lag[-1,] #eliminar primera fila (retardos)
which(is.na(df_lag))

qcor <- function(Y,X,tau){
  QY<- quantile(Y,probs = tau, na.rm =T)
  eps_tau <- ifelse(Y - QY < 0, tau - 1, tau)
  qcov <- mean(eps_tau * (X - mean(X,na.rm=T)),na.rm=T)
  vars <- (tau-tau**2)*var(X,na.rm=T)
  qcor_tau <- qcov/sqrt(vars)
  return(qcor_tau)
}

qcor_results <- sapply(names(df_filled)[-1],function(estacion){
  
  X_col <- paste0('X_',estacion)
  
  if (X_col %in% names(df_lag)) {
    sapply(cuantiles, function(tau) qcor(df_lag[[estacion]], df_lag[[X_col]], tau))
  } else {
    rep(NA, length(cuantiles))
  }
}

)

boxplot(t(qcor_results),names=cuantiles, main='Autocorrelacion cuantílica', 
        xlab = 'Cuantil', ylab='Parámetro')

# GUARDADO IMAGEN CUANTILES EMPIRICOS
png("cuantiles2_zgz.png", width = 1200, height = 600, res = 150)
par(mfrow=c(1,2))
#1
boxplot(t(cuant_emp), names = cuantiles, main = "Cuantiles",
        xlab = "Cuantil", ylab = "Temperatura (ºC)", las = 2)
#2
boxplot(t(dif_cuant),main='Diferencia cuantiles', names =cuantiles, 
        xlab='Cuantil', ylab='Temperatura (ºC)',las=2)

dev.off()



## second order
library(quantreg)

df_lag <- df_filled %>%
  mutate(across(-Date, ~ lag(.x, 1), .names = "X_{.col}")) %>%  # Retardo 1
  mutate(across(names(df_filled)[-1], ~ lag(.x, 2), .names = "Z_{.col}"))      # Retardo 2

df_lag <- df_lag[-c(1,2),]

head(df_lag)

# pruebas con una sola estacion
# Y<-df_lag$X229
# X<-df_lag$X_X229
# Z<-df_lag$Z_X229
# mod1<-lm(X~Z) #efecto lineal de Z en X
#tau = 0.95
# mod2<-rq(Y~Z,tau=tau) #efecto lineal Z en el cuantil de Y
# 
# alpha1 <- coef(mod1)[1]
# beta1 <- coef(mod1)[2]
# 
# alpha2 <- coef(mod2)[1]
# beta2 <- coef(mod2)[2]
# 
# res1<-mod1$residuals
# res2<-mod2$residuals
# 
psi_tau <- function(w, tau) {
  ifelse(w < 0, tau - 1, tau)
}
# 
# mean(psi_tau(Y-alpha2-beta2*Z,tau)*X)/(sqrt((tau-tau**2))*mean((X-alpha1-beta1*Z)**2))


qpcor_tau <- function(estacion, df, taus) {
  sapply(taus, function(tau){
  
  Y <- df[[estacion]]             # Serie original
  X <- df[[paste0("X_", estacion)]]  # Retardo 1
  Z <- df[[paste0("Z_", estacion)]]  # Retardo 2

  mod1 <- lm(X ~ Z)
  mod2 <- rq(Y ~ Z, tau = tau)

  alpha1 <- coef(mod1)[1]
  beta1  <- coef(mod1)[2]
  alpha2 <- coef(mod2)[1]
  beta2  <- coef(mod2)[2]

  return(mean(psi_tau(Y-alpha2-beta2*Z,tau)*X)/(sqrt((tau-tau**2))*mean((X-alpha1-beta1*Z)**2)))
  })
}

qpcor_results <- sapply( names(df_filled)[-1], qpcor_tau, df=df_lag, taus = cuantiles)

par(mfrow=c(1,1))


png("correlaciones.png", width = 1200, height = 600, res = 150)
par(mfrow=c(1,2))
#3
boxplot(t(qcor_results),names=cuantiles, main='Autocorrelacion cuantílica', 
        xlab = 'Cuantil', ylab='Parámetro')
#4
boxplot(t(qpcor_results),names=cuantiles, main='Autocorrelación segundo orden', 
        xlab = 'Cuantil', ylab='Parámetro',ylim=c(0,0.2))
dev.off()

#------------------------EXTRA------------------------

## empirical vs previous para una estacion X229
Y<- df_lag$'229'/10
X<- df_lag$X_229/10
Z<- df_lag$Z_229/10

cuant2<-c(1/3,2/3)
cuant_emp2 <- apply(df[,-1]/10, 2, quantile,na.rm=T, probs = cuant2)
cuant_emp2<- as.data.frame(cuant_emp2, row.names = cuant2)

q1<-cuant_emp2$'229'[1]
q2<-cuant_emp2$'229'[2]

df_X229 <- as.data.frame(cbind(Y,X,Z))

# separacion en sets
library(ggplot2)
df_X229$grupo <- cut(df_X229$Z, breaks=c(-Inf,q1,q2,Inf),labels=c('blue','black','red'))
bin_width <- 2 

df_X229$X_bin <- cut(df_X229$X, 
                     breaks = seq(min(df_X229$X, na.rm=TRUE), 
                                  max(df_X229$X, na.rm=TRUE), 
                                  by = bin_width), 
                     include.lowest = TRUE)

quantiles_df <- df_X229 %>%
  group_by(X_bin, grupo) %>%
  summarise(
    q05 = quantile(Y, probs = 0.05, na.rm = TRUE),
    q50 = quantile(Y, probs = 0.50, na.rm = TRUE),
    q95 = quantile(Y, probs = 0.95, na.rm = TRUE),
    n = n()
  ) %>%
  filter(n >= 30)

quantiles_df <- quantiles_df %>%
  mutate(X_val = as.numeric(gsub("\\[|\\]|\\(|\\)", "", sub(",.*", "", X_bin))))

ggplot(quantiles_df, aes(x = X_val, color = grupo)) +
  geom_line(aes(y = q05)) +
  geom_line(aes(y = q50)) +
  geom_line(aes(y = q95)) +
  geom_point(aes(y=q05)) +
  geom_point(aes(y=q50)) +
  geom_point(aes(y=q95)) +
  scale_color_manual(values = c("blue" = "blue", "black" = "black", "red" = "red")) +
  labs(title = 'X229',
       x = expression(Y[t-1]), 
       y = expression("Quantiles de" ~ Y[t]), 
       color = expression("Grupo según" ~ Y[t-2])) +
  theme_minimal()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(color = "black", fill = NA))

efecto<-function(estacion){
  Y<- df_lag[[estacion]]/10
  X<- df_lag[[paste0('X_',estacion)]]/10
  Z<- df_lag[[paste0('Z_',estacion)]]/10
  
  cuant2<-c(1/3,2/3)
  cuant_emp2 <- apply(df[,-1]/10, 2, quantile,na.rm=T, probs = cuant2)
  cuant_emp2<- as.data.frame(cuant_emp2, row.names = cuant2)
  
  q1<-cuant_emp2[[estacion]][1]
  q2<-cuant_emp2[[estacion]][2]
  
  df_p <- as.data.frame(cbind(Y,X,Z))
  
  # separacion en sets
  df_p$grupo <- cut(df_p$Z, breaks=c(-Inf,q1,q2,Inf),labels=c('blue','black','red'))
  bin_width <- 2 
  
  df_p$X_bin <- cut(df_p$X, 
                       breaks = seq(min(df_p$X, na.rm=TRUE), 
                                    max(df_p$X, na.rm=TRUE), 
                                    by = bin_width), 
                       include.lowest = TRUE)
  
  quantiles_df <- df_p %>%
    group_by(X_bin, grupo) %>%
    summarise(
      q05 = quantile(Y, probs = 0.05, na.rm = TRUE),
      q50 = quantile(Y, probs = 0.50, na.rm = TRUE),
      q95 = quantile(Y, probs = 0.95, na.rm = TRUE),
      n = n()
    ) %>%
    filter(n >= 30)
  
  quantiles_df <- quantiles_df %>%
    mutate(X_val = as.numeric(gsub("\\[|\\]|\\(|\\)", "", sub(",.*", "", X_bin))))
  
  
  ggplot(quantiles_df, aes(x = X_val, color = grupo)) +
    geom_line(aes(y = q05)) +
    geom_line(aes(y = q50)) +
    geom_line(aes(y = q95)) +
    geom_point(aes(y=q05)) +
    geom_point(aes(y=q50)) +
    geom_point(aes(y=q95)) +
    scale_color_manual(values = c("blue" = "blue", "black" = "black", "red" = "red")) +
    labs(title = estacion,
         x = expression(Y[t-1]), 
         y = expression("Quantiles de" ~ Y[t]), 
         color = expression("Grupo según" ~ Y[t-2])) +
    theme_minimal()+
    theme(panel.grid = element_blank(),
          panel.border = element_rect(color = "black", fill = NA))
}

efecto('232')




#------------------------EFECTO ELEVACION Y LATITUD------------------------
stations_dist <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/stations_dist.rds")

library(ggplot2)

plot_elev_lat<-function(zona=2:41){
  cuant_emp_elev<-cuant_emp[c('0.05','0.5','0.95'),zona-1]
  cuant_emp_elev<-rbind(stations$HGHT[zona-1],stations$LAT[zona-1],stations_dist$DIST[zona-1],cuant_emp_elev)
  cuant_emp_elev<-as.data.frame(t(cuant_emp_elev))
  colnames(cuant_emp_elev)[c(1,2,3)]<-c('elev','lat','dist')
  
  
  elev<-ggplot(cuant_emp_elev) +
    geom_point(aes(x = log1p(elev), y = `0.05`),col='blue',size=1.5)+
    geom_smooth(aes(x = log1p(elev), y = `0.05`),method = "lm", se = FALSE, color = "blue")+
    geom_point(aes(x = log1p(elev), y = `0.5`),col='black',size=1.5)+
    geom_smooth(aes(x = log1p(elev), y = `0.5`),method = "lm", se = FALSE, color = "black")+
    geom_point(aes(x = log1p(elev), y = `0.95`),col='red',size=1.5)+
    geom_smooth(aes(x = log1p(elev), y = `0.95`),method = "lm", se = FALSE, color = "red")+
    labs(title='Cuantiles empíricos vs log(1+elev(s)) ',
         x='log(1+elev(s)) (m)',
         y='Quantiles empíricos (ºC)')+
    theme_minimal()+
    theme(panel.grid = element_blank(),
          panel.border = element_rect(color = "black", fill = NA))
  
  lat<-ggplot(cuant_emp_elev) +
    geom_point(aes(x = lat, y = `0.05`),col='blue',size=1.5)+
    geom_smooth(aes(x = lat, y = `0.05`),method = "lm", se = FALSE, color = "blue")+
    geom_point(aes(x = lat, y = `0.5`),col='black',size=1.5)+
    geom_smooth(aes(x = lat, y = `0.5`),method = "lm", se = FALSE, color = "black")+
    geom_point(aes(x = lat, y = `0.95`),col='red',size=1.5)+
    geom_smooth(aes(x = lat, y = `0.95`),method = "lm", se = FALSE, color = "red")+
    labs(title='Cuantiles empíricos vs Latitud',
         x='Latitud (º)',
         y='Quantiles empíricos (ºC)')+
    theme_minimal()+
    theme(panel.grid = element_blank(),
          panel.border = element_rect(color = "black", fill = NA))
  
  dist<-ggplot(cuant_emp_elev) +
    geom_point(aes(x = log1p(dist), y = `0.05`),col='blue',size=1.5)+
    geom_smooth(aes(x = log1p(dist), y = `0.05`),method = "lm", se = FALSE, color = "blue")+
    geom_point(aes(x = log1p(dist), y = `0.5`),col='black',size=1.5)+
    geom_smooth(aes(x = log1p(dist), y = `0.5`),method = "lm", se = FALSE, color = "black")+
    geom_point(aes(x = log1p(dist), y = `0.95`),col='red',size=1.5)+
    geom_smooth(aes(x = log1p(dist), y = `0.95`),method = "lm", se = FALSE, color = "red")+
    labs(title='Cuantiles empíricos vs log(1+dist(s)) ',
         x='log(1+dist(s)) (km)',
         y='Quantiles empíricos (ºC)')+
    theme_minimal()+
    theme(panel.grid = element_blank(),
          panel.border = element_rect(color = "black", fill = NA))
  
  ggpubr::ggarrange(elev,dist,lat,
                    nrow = 1,ncol=3)
}

png("elev_dist_lat_todos.png", width = 1500, height = 600, res = 150)
plot_elev_lat()
dev.off()

png("elev_lat_centro.png", width = 1200, height = 600, res = 150)
plot_elev_lat(centro)
dev.off()

png("elev_lat_cant.png", width = 1200, height = 600, res = 150)
plot_elev_lat(cant)
dev.off()

png("elev_lat_med.png", width = 1200, height = 600, res = 150)
plot_elev_lat(med)
dev.off()

cuant_emp_elev_q0.95<-cuant_emp[c('0.95'),]
cuant_emp_elev_q0.95<-rbind(stations$HGHT,stations$LAT,stations_dist$DIST,cuant_emp_elev_q0.95)
cuant_emp_elev_q0.95<-as.data.frame(t(cuant_emp_elev_q0.95))
colnames(cuant_emp_elev_q0.95)[c(1,2,3)]<-c('elev','lat','dist')

cuant_emp_elev_q0.95_centro<-cuant_emp[c('0.95'),centro-1]
cuant_emp_elev_q0.95_centro<-rbind(stations$HGHT[centro-1],stations$LAT[centro-1],stations_dist$DIST[centro-1],cuant_emp_elev_q0.95_centro)
cuant_emp_elev_q0.95_centro<-as.data.frame(t(cuant_emp_elev_q0.95_centro))
colnames(cuant_emp_elev_q0.95_centro)[c(1,2,3)]<-c('elev','lat','dist')

cuant_emp_elev_q0.95_cant<-cuant_emp[c('0.95'),cant-1]
cuant_emp_elev_q0.95_cant<-rbind(stations$HGHT[cant-1],stations$LAT[cant-1],stations_dist$DIST[cant-1],cuant_emp_elev_q0.95_cant)
cuant_emp_elev_q0.95_cant<-as.data.frame(t(cuant_emp_elev_q0.95_cant))
colnames(cuant_emp_elev_q0.95_cant)[c(1,2,3)]<-c('elev','lat','dist')

cuant_emp_elev_q0.95_med<-cuant_emp[c('0.95'),med-1]
cuant_emp_elev_q0.95_med<-rbind(stations$HGHT[med-1],stations$LAT[med-1],stations_dist$DIST[med-1],cuant_emp_elev_q0.95_med)
cuant_emp_elev_q0.95_med<-as.data.frame(t(cuant_emp_elev_q0.95_med))
colnames(cuant_emp_elev_q0.95_med)[c(1,2,3)]<-c('elev','lat','dist')


library(ggplot2)


cuant_emp_elev_q0.95_combined <- rbind(
  cbind(cuant_emp_elev_q0.95, group = 'Todos'),
  cbind(cuant_emp_elev_q0.95_centro, group = 'Centro peninsular'),
  cbind(cuant_emp_elev_q0.95_cant, group = 'Costa cantábrica'),
  cbind(cuant_emp_elev_q0.95_med,group='Costa mediterránea')
)

elev_juntos <- ggplot(cuant_emp_elev_q0.95_combined) +
  geom_point(aes(x = log1p(elev), y = `0.95`, color = group), size = 1.5) +
  geom_smooth(aes(x = log1p(elev), y = `0.95`, color = group), method = "lm", se = FALSE) +
  labs(title = 'Cuantil 0.95 empírico vs log(1+elev(s))',
       x = 'log(1+elev(s)) (m)',
       y = 'Quantiles empíricos (ºC)') +
  scale_color_manual(values = c('Todos'='black','Centro peninsular' = 'red', 'Costa cantábrica' = 'forestgreen','Costa mediterránea'= 'blue'),
                     guide = guide_legend(title = NULL)) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(color = "black", fill = NA))

elev_juntos

lat_juntos <- ggplot(cuant_emp_elev_q0.95_combined) +
  geom_point(aes(x = lat, y = `0.95`, color = group), size = 1.5) +
  geom_smooth(aes(x = lat, y = `0.95`, color = group), method = "lm", se = FALSE) +
  labs(title = 'Cuantiles 0.95 empírico vs Latitud',
       x = 'Latitud (º)',
       y = 'Quantiles empíricos (ºC)') +
  scale_color_manual(values = c('Todos' = 'black', 'Centro peninsular' = 'red', 'Costa cantábrica' = 'forestgreen','Costa mediterránea'= 'blue'),
                     guide = guide_legend(title = NULL)) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(color = "black", fill = NA))

lat_juntos

dist_juntos <- ggplot(cuant_emp_elev_q0.95_combined) +
  geom_point(aes(x = log1p(dist), y = `0.95`, color = group), size = 1.5) +
  geom_smooth(aes(x = log1p(dist), y = `0.95`, color = group), method = "lm", se = FALSE) +
  labs(title = 'Cuantil 0.95 empírico vs log(1+dist(s))',
       x = 'log(1+dist(s)) (km)',
       y = 'Quantiles empíricos (ºC)') +
  scale_color_manual(values = c('Todos'='black','Centro peninsular' = 'red', 'Costa cantábrica' = 'forestgreen','Costa mediterránea'= 'blue'),
                     guide = guide_legend(title = NULL)) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(color = "black", fill = NA))

dist_juntos

png("logelev_dist_lat_juntos.png", width = 1500, height = 600, res = 150)
ggpubr::ggarrange(elev_juntos,dist_juntos,lat_juntos,ncol=3,common.legend = T,legend='bottom')
dev.off()
