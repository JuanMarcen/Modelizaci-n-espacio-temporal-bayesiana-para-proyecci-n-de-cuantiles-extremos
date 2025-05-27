#data frames
rm(list=ls())
setwd('C:/Users/jumar/OneDrive/Escritorio/TFM/Datos')
X <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/X.rds")
Y <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Y.rds")
stations <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/stations.rds")
stations_dist <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/stations_dist.rds")

# X cuadrado
X_cuadrado<-X
X_cuadrado[, 4:ncol(X)] <- X[, 4:ncol(X)]^2
saveRDS(X_cuadrado,'X_cuadrado.rds')

# X scaled
X_sc<-X
X_sc[,4:ncol(X)]<-scale(X[,4:ncol(X)])
saveRDS(X_sc,'X_sc.rds')

# X cuadrado scaled
X_cuadrado_sc<-X_cuadrado
X_cuadrado_sc[,4:ncol(X)]<-scale(X_cuadrado_sc[,4:ncol(X)])
saveRDS(X_cuadrado_sc,'X_cuadrado_sc.rds')


# data frames que utilizo en modelos
df <- data.frame(Y=Y[,-c(1,2)]/10, X[,-c(1,2,3)])
saveRDS(df,'df.rds')

df_sc<-data.frame(Y=Y[,-c(1,2)]/10, X_sc[,-c(1,2,3)])
saveRDS(df_sc,'df_sc.rds')

#filled
library(zoo)
df_filled_sc<-df_sc
for (col_name in names(df_filled_sc)){
  df_filled_sc[[col_name]] <- na.approx(df_sc[[col_name]],rule=2)
}
which(is.na(df_filled_sc))

elev_sc<-scale(stations_dist$HGHT)
dist_sc<-scale(stations_dist$DIST)

colnames(X_cuadrado_sc)[-c(1,2,3)]<-paste0('I(',colnames(X_cuadrado_sc)[-c(1,2,3)],'^2)')
df_conj_filled_sc<-cbind(df_filled_sc,X_cuadrado_sc[-c(1,2,3)])
df_conj_filled_sc<-cbind(df_conj_filled_sc,rep(elev_sc,each=5888),rep(dist_sc,each=5888))
colnames(df_conj_filled_sc)[c(32,33)]<-c('elev','dist')

saveRDS(df_conj_filled_sc,'df_conj_filled_sc.rds')
