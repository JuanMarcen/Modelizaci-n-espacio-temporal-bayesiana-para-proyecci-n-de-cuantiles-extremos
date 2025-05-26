# ANALISIS EXPLORATORIO
rm(list=ls())
library(tseries)
library(zoo)
library(forecast)
library(RcmdrMisc)
library(readr)

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


#------------------------RELLENO NULOS Y ANALISIS DE TENDENCIA PARA LAS SERIES ENTERAS------------------------
#MEDIAS MOVILES CON VENTANA AMPLIA PARA UNA TENDENCIA SUAVE. (no hace falta filled)
#VENTANA DE K=92*8=736

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

#------------------------COMPONENTE ESTACIONAL------------------------

day_month <- unique(format(df$Date, "%d-%m"))

df_comp_est<-matrix(NA,nrow=length(day_month),ncol=40)
df_comp_est<-as.data.frame(df_comp_est,row.names = day_month)
colnames(df_comp_est)<-stations$STAID

for (i in 1:length(day_month)){
  ind<-which(format(df$Date, "%d-%m") == day_month[i])
  df_comp_est[i,]<-colMeans(df[ind,-1],na.rm=T)/10
}

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

# serie JJA
par(mfrow=c(1,2))
cuant_emp <- apply(df[,-1]/10, 2, quantile,na.rm=T, probs = cuantiles)
cuant_emp<-as.data.frame(cuant_emp, row.names = cuantiles)
boxplot(t(cuant_emp), names = cuantiles, main = "Cuantiles",
        xlab = "Cuantil", ylab = "Temperatura (ºC)", las = 2)


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


## second order
library(quantreg)

df_lag <- df_filled %>%
  mutate(across(-Date, ~ lag(.x, 1), .names = "X_{.col}")) %>%  # Retardo 1
  mutate(across(names(df_filled)[-1], ~ lag(.x, 2), .names = "Z_{.col}"))      # Retardo 2

df_lag <- df_lag[-c(1,2),]

head(df_lag)

psi_tau <- function(w, tau) {
  ifelse(w < 0, tau - 1, tau)
}

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
boxplot(t(qpcor_results),names=cuantiles, main='Autocorrelación segundo orden', 
        xlab = 'Cuantil', ylab='Parámetro',ylim=c(0,0.2))
dev.off()

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

plot_elev_lat()
dev.off()

plot_elev_lat(centro)
dev.off()

plot_elev_lat(cant)
dev.off()

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

ggpubr::ggarrange(elev_juntos,dist_juntos,lat_juntos,ncol=3,common.legend = T,legend='bottom')
dev.off()
