if (!require("remotes")) install.packages("remotes")
remotes::install_github("JorgeCastilloMateo/spTReg")

rm(list=ls())
setwd('C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Analisis Exploratorio/ModelosReg/bay')
df <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/df.rds")
stations <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/stations.rds")
stations_dist <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/stations_dist.rds")
coef_R <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/coef_R.rds")
coef_R_sc <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/coef_R_sc.rds")


library(spTReg)
library(zoo)
library(quantreg)
library(sf)
library(sp)
library("rnaturalearth")
library("rnaturalearthdata")
library(tidyverse)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(coda)

#------------------------PREVIO------------------------
#relleno de nulos
which(is.na(df$Y))
df_filled<-df
for (col_name in names(df_filled)){
  df_filled[[col_name]] <- na.approx(df[[col_name]],rule=2)
}

which(is.na(df_filled))

#coordenadas en km
stations <- st_transform(
  as(
    SpatialPointsDataFrame(
      coords = stations[c("LON", "LAT")], 
      data = stations[c("STAID", "STANAME", "LON", "LAT", "HGHT","color",'NAME1','NAME2')],
      proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")),
    'sf'
  ),
  2062
)

coords_km<-st_coordinates(stations)/1000
#------------------------------------------------

#------------------------MODELO------------------------
#1. MODELO BAYESIANO (para tener todo guardado)
mod_bay_simple<-function(var, df, coords,cuantil = 0.5, burnin = 1000, it = 1000,scale = FALSE){
  
  if (scale==T){
    df[[var]]<-scale(df[[var]])
  }
  
  if (scale==T){
    s_beta<- colMeans(coef_R_sc[,c(paste0('int_q',cuantil,'_',var),paste0('q',cuantil,'_',var))])
    s_alpha <-  as.matrix(sweep(coef_R_sc[,c(paste0('int_q',cuantil,'_',var),paste0('q',cuantil,'_',var))],2,s_beta))
    s_alpha <- s_alpha +rnorm(40*2,0,0.1)
  }else{
    s_beta <- mean(coef_R[[paste0('q',cuantil,'_',var)]])
    s_alpha <-  cbind(coef_R[[paste0('int_q',cuantil,'_',var)]],coef_R[[paste0('q',cuantil,'_',var)]])
  }
  
  formula<-as.formula(paste('Y ~',var))
  
  mod<-spTm(formula,
            data=df,
            v=as.matrix(cbind(1,df[[var]])),
            method='q',
            quantile = cuantil,
            coords=coords,
            priors = list(
              "beta" = c(0, 1 / 100),
              "sigma" = c(0.1, 0.1),
              "phi" = c(2, 100),
              "mu" = c(0, 1 / 100)),
            starting = list(
              "beta" = s_beta,
              "sigma" = 1,
              "alpha" = as.matrix(s_alpha),
              "hp" = c("mu" = 0, "sigma" = 1, "phi" = 3 / 100)),
            n.samples = it,
            n.burnin = burnin
  )
  

    return(mod)

}

#no parece haber convergencia, por eso se realiza un escalado de los datos
mod_bay_g500_q0.5<-mod_bay_simple('g500',df_filled,coords_km)
mod_bay_g700_q0.5<-mod_bay_simple('g700',df_filled,coords_km)

head(mod_bay_g300_q0.5$p.params.samples)
mod_bay_g300_q0.95<-mod_bay_simple('g300',df_filled,coords_km,cuantil=0.95)
mod_bay_g500_q0.95<-mod_bay_simple('g500',df_filled,coords_km,cuantil=0.95)
mod_bay_g700_q0.95<-mod_bay_simple('g700',df_filled,coords_km,cuantil=0.95)

saveRDS(mod_bay_g300_q0.5,'mod_bay_g300_q0.5.rds')
saveRDS(mod_bay_g500_q0.5,'mod_bay_g500_q0.5.rds')
saveRDS(mod_bay_g700_q0.5,'mod_bay_g700_q0.5.rds')
saveRDS(mod_bay_g300_q0.95,'mod_bay_g300_q0.95.rds')
saveRDS(mod_bay_g500_q0.95,'mod_bay_g500_q0.95.rds')
saveRDS(mod_bay_g700_q0.95,'mod_bay_g700_q0.95.rds')


mod_bay_g300_q0.5 <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Analisis Exploratorio/ModelosReg/bay/mod_bay_g300_q0.5.rds")
mod_bay_g500_q0.5 <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Analisis Exploratorio/ModelosReg/bay/mod_bay_g500_q0.5.rds")
mod_bay_g700_q0.5 <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Analisis Exploratorio/ModelosReg/bay/mod_bay_g700_q0.5.rds")
mod_bay_g300_q0.95 <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Analisis Exploratorio/ModelosReg/bay/mod_bay_g300_q0.95.rds")
mod_bay_g500_q0.95 <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Analisis Exploratorio/ModelosReg/bay/mod_bay_g500_q0.95.rds")
mod_bay_g700_q0.95 <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Analisis Exploratorio/ModelosReg/bay/mod_bay_g700_q0.95.rds")


#escalados
mod_bay_g300_q0.5_sc<-mod_bay_simple('g300',df_filled,coords_km,scale=T)
plot(mod_bay_g300_q0.5_sc$p.params.samples)
#
mod_bay_g500_q0.5_sc<-mod_bay_simple('g500',df_filled,coords_km,scale=T)
mod_bay_g700_q0.5_sc<-mod_bay_simple('g700',df_filled,coords_km,scale=T)

mod_bay_g300_q0.95_sc<-mod_bay_simple('g300',df_filled,coords_km,cuantil=0.95,scale=T)
mod_bay_g500_q0.95_sc<-mod_bay_simple('g500',df_filled,coords_km,cuantil=0.95,scale=T)
mod_bay_g700_q0.95_sc<-mod_bay_simple('g700',df_filled,coords_km,cuantil=0.95,scale=T)

saveRDS(mod_bay_g300_q0.5_sc,'mod_bay_g300_q0.5_sc.rds')
saveRDS(mod_bay_g500_q0.5_sc,'mod_bay_g500_q0.5_sc.rds')
saveRDS(mod_bay_g700_q0.5_sc,'mod_bay_g700_q0.5_sc.rds')
saveRDS(mod_bay_g300_q0.95_sc,'mod_bay_g300_q0.95_sc.rds')
saveRDS(mod_bay_g500_q0.95_sc,'mod_bay_g500_q0.95_sc.rds')
saveRDS(mod_bay_g700_q0.95_sc,'mod_bay_g700_q0.95_sc.rds')

mod_bay_g700_q0.95_sc <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Analisis Exploratorio/ModelosReg/bay/mod_bay_g700_q0.95_sc.rds")
mod_bay_g500_q0.95_sc <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Analisis Exploratorio/ModelosReg/bay/mod_bay_g500_q0.95_sc.rds")
mod_bay_g300_q0.95_sc <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Analisis Exploratorio/ModelosReg/bay/mod_bay_g300_q0.95_sc.rds")
mod_bay_g700_q0.5_sc <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Analisis Exploratorio/ModelosReg/bay/mod_bay_g700_q0.5_sc.rds")
mod_bay_g500_q0.5_sc <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Analisis Exploratorio/ModelosReg/bay/mod_bay_g500_q0.5_sc.rds")
mod_bay_g300_q0.5_sc <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Analisis Exploratorio/ModelosReg/bay/mod_bay_g300_q0.5_sc.rds")

#------------------------BETAS------------------------
#2. Guardado de betas en un data.frame
betas_esp<-function(mod,var,scale = F){
  params<-as.data.frame(mod$p.params.samples)
  
  if (scale==T){
    mu<-mean(df_filled[[var]])
    sd<-sd(df_filled[[var]])
    #intercepto
    int<-mean(params[['(Intercept)']])-mean(params[[var]])*mu/sd
    int<-rep(int, length=dim(stations)[1])
    
    #beta
    beta<-mean(params[[var]])/sd
    beta<-rep(beta,length=dim(stations)[1])
    
    #beta1 y beta2
    cols<-grep('beta',names(params),value=T)
    params<-params[,cols]
    mu_b<-apply(params,2,mean)
    betas<-matrix(mu_b,nrow=dim(stations)[1])
    betas<-as.data.frame(betas,row.names = stations$NAME2)
    
    betas<-cbind(int,beta,betas)
    colnames(betas)<-c('intercept','beta','beta1','beta2')
    
    betas$beta1<-betas$beta1-betas$beta2*mu/sd
    betas$beta2<-betas$beta2/sd
    
  }else{
    mu<-mean(df_filled[[var]])
    sd<-sd(df_filled[[var]])
    #intercepto
    int<-mean(params[['(Intercept)']])
    int<-rep(int, length=dim(stations)[1])
    
    #beta
    beta<-mean(params[[var]])
    beta<-rep(beta,length=dim(stations)[1])
    
    #beta1 y beta2
    cols<-grep('beta',names(params),value=T)
    params<-params[,cols]
    mu<-apply(params,2,mean)
    betas<-matrix(mu,nrow=dim(stations)[1])
    betas<-as.data.frame(betas,row.names = stations$NAME2)
    
    betas<-cbind(int,beta,betas)
    colnames(betas)<-c('intercept','beta','beta1','beta2')
  }
  
  
  return(betas)
}

betas_esp_g300_q0.5<-betas_esp(mod_bay_g300_q0.5,'g300')
betas_esp_g500_q0.5<-betas_esp(mod_bay_g500_q0.5,'g500')
betas_esp_g700_q0.5<-betas_esp(mod_bay_g700_q0.5,'g700')

betas_esp_g300_q0.95<-betas_esp(mod_bay_g300_q0.95,'g300')
betas_esp_g500_q0.95<-betas_esp(mod_bay_g500_q0.95,'g500')
betas_esp_g700_q0.95<-betas_esp(mod_bay_g700_q0.95,'g700')

#escalado
betas_esp_g300_q0.5_sc<-betas_esp(mod_bay_g300_q0.5_sc,'g300',scale = T)
betas_esp_g500_q0.5_sc<-betas_esp(mod_bay_g500_q0.5_sc,'g500',scale = T)
betas_esp_g700_q0.5_sc<-betas_esp(mod_bay_g700_q0.5_sc,'g700',scale = T)

betas_esp_g300_q0.95_sc<-betas_esp(mod_bay_g300_q0.95_sc,'g300',scale = T)
betas_esp_g500_q0.95_sc<-betas_esp(mod_bay_g500_q0.95_sc,'g500',scale = T)
betas_esp_g700_q0.95_sc<-betas_esp(mod_bay_g700_q0.95_sc,'g700',scale = T)

saveRDS(betas_esp_g300_q0.5_sc,'betas_esp_g300_q0.5_sc.rds')




#------------------------PREDICCIONES------------------------
# Media ponderada de cada parametro. beta, beta1 y beta2
df_esp <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/df_esp.rds")
Y <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Y.rds")
Y$Value<-Y$Value/10
# colnames(X_cuadrado)[-c(1,2,3)]<-paste0('I(',colnames(X_cuadrado)[-c(1,2,3)],'^2)')
# df_conj<-cbind(df_filled,X_cuadrado[-c(1,2,3)])

predictions<-function(var,tipo,betas=NULL,mod=NULL){
  
  # if (tipo==1){
  #   pred<-matrix(NA,nrow=235520,ncol=1000)
  #   
  #   for (i in 1:dim(stations)[1]){ #estacion
  #     ind<-which(df_esp$station==stations$STAID[i])
  #     for (j in ind){ #dia
  #       for(k in 1:1000){ #iteracion
  #         pred[j,k]<-mod$p.params.samples[k,'(Intercept)'] + 
  #           df_esp[j,var]*mod$p.params.samples[k,var] +
  #           mod$p.params.samples[k,paste0('beta1(s',i,')')] + 
  #           df_esp[j,var]*mod$p.params.samples[k,paste0('beta2(s',i,')')]
  #       }
  #     }
  #   }
  #   pred<-as.data.frame(pred)
  #   pred$Date <- df_esp$Date
  #   pred$station <- df_esp$station
  #   colnames(pred)[1:1000]<-paste0('It',1:1000)
  #   pred<-pred[,c(1001,1002,1:1000)]
  #   
  # }
  # 
  if (tipo==2){
    pred<-NA
    for (i in 1:dim(stations)[1]){
      ind<-which(df_esp$station==stations$STAID[i])
      
      for (j in ind){
        pred[j]<-betas$intercept[i] + df_esp[j,var]*betas$beta[i] + betas$beta1[i] + df_esp[j,var]*betas$beta2[i]
      }
      
    }
  }
  
  return(pred)
}


pred_g300_q0.5<-predictions('g300',2,betas_esp_g300_q0.5_sc)
pred_g500_q0.5<-predictions('g500',2,betas_esp_g500_q0.5_sc)
pred_g700_q0.5<-predictions('g700',2,betas_esp_g700_q0.5_sc)

pred_g300_q0.95<-predictions('g300',2,betas_esp_g300_q0.95_sc)
pred_g500_q0.95<-predictions('g500',2,betas_esp_g500_q0.95_sc)
pred_g700_q0.95<-predictions('g700',2,betas_esp_g700_q0.95_sc)

pred<-cbind(Y,pred_g300_q0.5,pred_g300_q0.95,pred_g500_q0.5,pred_g500_q0.95,pred_g700_q0.5,pred_g700_q0.95)
saveRDS(pred,'pred.rds')


#------------------------R1------------------------
check <- function(u, tau) {
  return(u * (tau - (u < 0)))  # Implements the quantile loss function
}

R1_bay<-function(pred,var,tau){
  pred_clean<-na.omit(pred)
  
  #dataframe para global
  df<-matrix(NA,nrow=1,ncol=1)
  df<-as.data.frame(df)
  colnames(df)<-c('R1_bay_global')
  
  #dataframe para locales
  df_local<-matrix(NA,nrow=dim(stations)[1],ncol=1)
  df_local<-as.data.frame(df_local,row.names = stations$NAME2)
  colnames(df_local)<-c('R1_bay_local')
  
  #modelos nulos, son para todas variables igual
  mod_nulo_f<-rq(Y~station,data=df_esp,tau=tau)
  
  rho_estacion<-rep(NA,dim(stations)[1])
  R1_nulo_est<-rep(NA,dim(stations)[1])
  for (j in 1:length(rho_estacion)){
    ind<-which(pred_clean$station==stations$STAID[j])
    rho_estacion[j]<-sum(check(pred_clean$Value[ind]-pred_clean[ind,paste0('pred_',var,'_q',tau)],tau=tau))
    R1_nulo_est[j]<-sum(check(mod_nulo_f$residuals[ind],tau=tau))
  }
  
  df['R1_bay_global']<-1-sum(rho_estacion)/mod_nulo_f$rho
  df_local['R1_bay_local']<-1-rho_estacion/R1_nulo_est
  
  return(list(R1_globales=df,R1_locales=df_local))
}


R1_bay_g300_q0.5<-R1_bay(pred,'g300',0.5)
R1_bay_g500_q0.5<-R1_bay(pred,'g500',0.5)
R1_bay_g700_q0.5<-R1_bay(pred,'g700',0.5)

R1_bay_g300_q0.95<-R1_bay(pred,'g300',0.95)
R1_bay_g500_q0.95<-R1_bay(pred,'g500',0.95)
R1_bay_g700_q0.95<-R1_bay(pred,'g700',0.95)


saveRDS(R1_bay_g300_q0.5,'R1_bay_g300_q0.5.rds')
saveRDS(R1_bay_g300_q0.95,'R1_bay_g300_q0.95.rds')
saveRDS(R1_bay_g500_q0.5,'R1_bay_g500_q0.5.rds')
saveRDS(R1_bay_g500_q0.95,'R1_bay_g500_q0.95.rds')
saveRDS(R1_bay_g700_q0.5,'R1_bay_g700_q0.5.rds')
saveRDS(R1_bay_g700_q0.95,'R1_bay_g700_q0.95.rds')

#------------------------RHO------------------------
library(lubridate)
pred <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Analisis Exploratorio/ModelosReg/bay/pred.rds")
rho_bay<-function(predicciones,var,tau){
  pred_clean<-na.omit(predicciones)
  
  #global
  #dataframe para global
  df<-matrix(NA,nrow=1,ncol=1)
  df<-as.data.frame(df)
  colnames(df)<-c('rho_bay_global')
  pred<-pred_clean[[paste0('pred_',var,'_q',tau)]]
  dif<-pred_clean$Value-pred
  df['rho_bay_global']<- sum(dif<0,na.rm=T)/(length(pred))
  
  #estaciones
  df_est<-matrix(NA,nrow=dim(stations)[1],ncol=1)
  df_est<-as.data.frame(df_est,row.names = stations$NAME2)
  colnames(df_est)<-c('rho_bay_est')
  for (i in 1:dim(stations)[1]){
    ind<-which(pred_clean$station==stations$STAID[i])
    dif<-pred_clean$Value[ind]-pred[ind]
    df_est[i,]<-sum(dif<0,na.rm=T)/(length(dif))
  }
  
  #dias
  day_month <- unique(format(Y$Date, "%d-%m"))
  df_dia<-matrix(NA,nrow=length(day_month),ncol=1)
  df_dia<-as.data.frame(df_dia,row.names = day_month)
  colnames(df_dia)<-c('rho_bay_dia')
  for (i in 1:length(day_month)){
    ind<-which(format(pred_clean$Date, "%d-%m") == day_month[i])
    dif<-pred_clean$Value[ind]-pred[ind]
    df_dia[i,]<-sum(dif<0,na.rm=T)/(length(dif))
  }
  
  #años
  year<-unique(year(Y$Date))
  df_year<-matrix(NA,nrow=length(year),ncol=1)
  df_year<-as.data.frame(df_year,row.names = year)
  colnames(df_year)<-c('rho_bay_year')
  for (i in 1:length(year)){
    ind<-which(year(pred_clean$Date)==year[i])
    dif<-pred_clean$Value[ind]-pred[ind]
    df_year[i,]<-sum(dif<0,na.rm=T)/(length(dif))
  }

  return(list(rho_globales=df,rho_estaciones=df_est,rho_dias=df_dia,rho_años=df_year))
}

rho_g300_q0.5<-rho_bay(pred,'g300',0.5)
rho_g300_q0.95<-rho_bay(pred,'g300',0.95)

rho_g500_q0.5<-rho_bay(pred,'g500',0.5)
rho_g500_q0.95<-rho_bay(pred,'g500',0.95)

rho_g700_q0.5<-rho_bay(pred,'g700',0.5)
rho_g700_q0.95<-rho_bay(pred,'g700',0.95)

saveRDS(rho_g300_q0.5,'rho_g300_q0.5.rds')
saveRDS(rho_g300_q0.95,'rho_g300_q0.95.rds')
saveRDS(rho_g500_q0.5,'rho_g500_q0.5.rds')
saveRDS(rho_g500_q0.95,'rho_g500_q0.95.rds')
saveRDS(rho_g700_q0.95,'rho_g700_q0.95.rds')
saveRDS(rho_g700_q0.5,'rho_g700_q0.5.rds')

#plots de rhos por dia y año
rho_dia_año<-function(rho_q0.5,rho_q0.95){
  
  par(mfrow=c(1,2))
  #dias
  plot(1:length(rho_q0.5$rho_dias$rho_bay_dia), 
       rho_q0.5$rho_dias$rho_bay_dia, type = "l", xlab = "l", 
       ylab = expression(rho[bay]^dia), main = "Evolución diaria de ρ",
       ylim=c(0,1))
  abline(h=0.5,col='red')
  lines(1:length(rho_q0.95$rho_dias$rho_bay_dia), 
        rho_q0.95$rho_dias$rho_bay_dia)
  abline(h=0.95,col='red')
  
  #años
  plot(1:length(rho_q0.5$rho_años$rho_bay_year), 
       rho_q0.5$rho_años$rho_bay_year, type = "l", xlab = "t", 
       ylab = expression(rho[bay]^dia), main = "Evolución anual de ρ",
       ylim=c(0,1))
  abline(h=0.5,col='red')
  lines(1:length(rho_q0.95$rho_dias$rho_bay_dia), 
        rho_q0.95$rho_dias$rho_bay_dia)
  abline(h=0.95,col='red')
  
}

rho_dia_año(rho_g300_q0.5,rho_g300_q0.95)
rho_dia_año(rho_g500_q0.5,rho_g500_q0.95)
rho_dia_año(rho_g700_q0.5,rho_g700_q0.95)

#RHO CON ITERACIONES (hago el rho para cada simulacion)
rho_bay_tls<-function(modelo,var,scale=F,df,B){
  params<-as.data.frame(modelo$p.params.samples)
  
  rho<-NA
  
  if (scale==T){
    valores<-scale(df[[var]])
  }else{
    valores<-df[[var]]
  }
  
  for (i in 1:dim(stations)[1]){
      subparams<-cbind(params$`(Intercept)`,params[[var]],
                       params[[paste0('beta1(s',i,')')]],
                       params[[paste0('beta2(s',i,')')]])
      
      ind<-which(Y$station==stations$STAID[i])
      
      sub_df<-cbind(1,valores[ind,],1,valores[ind,])
      sub_df<-t(sub_df)
      
      pred<-subparams%*%sub_df
      
      sub_Y<-Y$Value[ind]
      dif <- sweep(pred, 2, sub_Y, FUN = "-")
      dif<- -dif
      rho[ind]<-apply(dif,2,FUN = function(x) sum(x<0))
      rho[ind]<-rho[ind]/B
  }
  
  rho_df<-data.frame(
    Date=Y$Date,
    station=Y$station,
    rho_tls=rho
  )
  
  return(rho_df)
}

rho_g300_q0.5_tls<-rho_bay_tls(mod_bay_g300_q0.5_sc,'g300',scale=T,df_filled,1000)
rho_g300_q0.95_tls<-rho_bay_tls(mod_bay_g300_q0.95_sc,'g300',scale=T,df_filled,1000)
rho_g500_q0.5_tls<-rho_bay_tls(mod_bay_g500_q0.5_sc,'g500',scale=T,df_filled,1000)
rho_g500_q0.95_tls<-rho_bay_tls(mod_bay_g500_q0.95_sc,'g500',scale=T,df_filled,1000)
rho_g700_q0.5_tls<-rho_bay_tls(mod_bay_g700_q0.5_sc,'g700',scale=T,df_filled,1000)
rho_g700_q0.95_tls<-rho_bay_tls(mod_bay_g700_q0.95_sc,'g700',scale=T,df_filled,1000)

library(dplyr)

rho_def <- function(rho) {
  r_all <- rho$rho_tls
  
  datos <- data.frame(
    Date = Y$Date,
    station = Y$station,
    rho = r_all
  )
  
  # Global
  df_global <- data.frame(rho_bay_global = mean(r_all, na.rm = TRUE))
  
  # Estaciones
  df_est <- datos %>%
    group_by(station) %>%
    summarize(rho_bay_est = mean(rho, na.rm = TRUE)) %>%
    as.data.frame()
  df_est <- df_est[match(stations$STAID, df_est$station), , drop = FALSE]
  rownames(df_est) <- stations$NAME2  # Nombres de fila como 'station'
  df_est <- df_est[, -1, drop = FALSE]  # Eliminar la columna extra 'station', manteniendo la estructura de data.frame
  
  # Días
  datos <- datos %>%
    mutate(day_month = format(Date, "%d-%m"))
  
  df_dia <- datos %>%
    group_by(day_month) %>%
    summarize(rho_bay_dia = mean(rho, na.rm = TRUE)) %>%
    as.data.frame()
  
  df_dia$day_month <- as.Date(df_dia$day_month, format = "%d-%m")
  df_dia <- df_dia[order(df_dia$day_month), , drop = FALSE]
  rownames(df_dia) <- df_dia$day_month  # Nombres de fila como 'day_month'
  df_dia <- df_dia[, -1, drop = FALSE]  # Eliminar la columna extra 'day_month', manteniendo la estructura de data.frame
  
  # Años
  datos <- datos %>%
    mutate(year = year(Date))
  
  df_year <- datos %>%
    group_by(year) %>%
    summarize(rho_bay_year = mean(rho, na.rm = TRUE)) %>%
    as.data.frame()
  rownames(df_year) <- df_year$year  # Nombres de fila como 'year'
  df_year <- df_year[, -1, drop = FALSE]  # Eliminar la columna extra 'year', manteniendo la estructura de data.frame
  
  return(list(
    rho_globales = df_global,
    rho_estaciones = df_est,
    rho_dias = df_dia,
    rho_años = df_year
  ))
}

#primeras pruebas
rho_g300_q0.5_2<-rho_def(rho_g300_q0.5_tls)
rho_g300_q0.95_2<-rho_def(rho_g300_q0.95_tls)
rho_g500_q0.5_2<-rho_def(rho_g500_q0.5_tls)
rho_g500_q0.95_2<-rho_def(rho_g500_q0.95_tls)
rho_g700_q0.5_2<-rho_def(rho_g700_q0.5_tls)
rho_g700_q0.95_2<-rho_def(rho_g700_q0.95_tls)

#figura de los tres
par(mfrow=c(1,2))
#dias
plot(1:length(rho_g300_q0.5_2$rho_dias$rho_bay_dia), 
     rho_g300_q0.5_2$rho_dias$rho_bay_dia, type = "l", xlab = "l", 
     ylab = expression(rho[l](tau)),
     ylim=c(0.3,1))
lines(1:length(rho_g500_q0.5_2$rho_dias$rho_bay_dia), 
      rho_g500_q0.5_2$rho_dias$rho_bay_dia,col='blue')
lines(1:length(rho_g700_q0.5_2$rho_dias$rho_bay_dia), 
      rho_g700_q0.5_2$rho_dias$rho_bay_dia,col='#008B00')
abline(h=0.5,col='red')
lines(1:length(rho_g300_q0.95_2$rho_dias$rho_bay_dia), 
      rho_g300_q0.95_2$rho_dias$rho_bay_dia)
lines(1:length(rho_g500_q0.95_2$rho_dias$rho_bay_dia), 
      rho_g500_q0.95_2$rho_dias$rho_bay_dia,col='blue')
lines(1:length(rho_g700_q0.95_2$rho_dias$rho_bay_dia), 
      rho_g700_q0.95_2$rho_dias$rho_bay_dia,col='#008B00')
abline(h=0.95,col='red')

#años
plot(1:length(rho_g300_q0.5_2$rho_años$rho_bay_year), 
     rho_g300_q0.5_2$rho_años$rho_bay_year, type = "l", xlab = "t", 
     ylab = expression(rho[t](tau)),
     ylim=c(0.3,1))
lines(1:length(rho_g500_q0.5_2$rho_años$rho_bay_year), 
      rho_g500_q0.5_2$rho_años$rho_bay_year,col='blue')
lines(1:length(rho_g700_q0.5_2$rho_años$rho_bay_year), 
      rho_g700_q0.5_2$rho_años$rho_bay_year,col='#008B00')
abline(h=0.5,col='red')
lines(1:length(rho_g300_q0.95_2$rho_años$rho_bay_year), 
      rho_g300_q0.95_2$rho_años$rho_bay_year)
lines(1:length(rho_g500_q0.95_2$rho_años$rho_bay_year), 
      rho_g500_q0.95_2$rho_años$rho_bay_year,col='blue')
lines(1:length(rho_g700_q0.95_2$rho_años$rho_bay_year), 
      rho_g700_q0.95_2$rho_años$rho_bay_year,col='#008B00')
abline(h=0.95,col='red')

dev.off()


#------------------------EXTRAS------------------------
# BOXPLOT DE BETA + BETA2(S) segun localizacion
dim(as.data.frame(mod_bay_g300_q0.5$p.params.samples))
orden_dist<-order(stations_dist$DIST)
#g300 q0.5
betas<-mod_bay_g700_q0.5_sc$p.params.samples[,c(paste0('beta1(s',1:40,')'))]-mod_bay_g700_q0.5_sc$p.params.samples[,c(paste0('beta2(s',1:40,')'))]*mean(df_filled[['g700']])/sd(df_filled[['g700']])
betas<-betas+mod_bay_g700_q0.5_sc$p.params.samples[,c('(Intercept)')]-mod_bay_g700_q0.5_sc$p.params.samples[,c('g700')]*mean(df_filled[['g700']])/sd(df_filled[['g700']])

betas<-as.data.frame(betas)
betas<-betas[,order(stations_dist$DIST)]
colnames(betas)<-stations_dist$NAME2[order(stations_dist$DIST)]

boxplot(betas,las=2)
beta_simple<-coef_R$int_q0.5_g700
beta_simple<-beta_simple[order(stations_dist$DIST)]
points(1:40,beta_simple,col='red',pch=19)


# colMeans(mod_bay_g300_q0.5$p.params.samples[,c('sigma1','sigma2')])
# plot(mod_bay_g300_q0.5$p.params.samples[,c('phi1','phi2')])
