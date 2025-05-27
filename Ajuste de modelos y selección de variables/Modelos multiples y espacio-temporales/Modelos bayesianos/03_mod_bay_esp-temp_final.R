rm(list=ls())
setwd('C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Analisis Exploratorio/ModelosReg/bay')


df_esp <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/df_esp.rds")
stations <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/stations.rds")
v_q0.95 <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Analisis Exploratorio/ModelosReg/bay/v_q0.95.rds")
v_q0.5 <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Analisis Exploratorio/ModelosReg/bay/v_q0.5.rds")
vars_q0.5 <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/vars_q0.5.rds")
vars_q0.95 <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/vars_q0.95.rds")
Y <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Y.rds")
Y$Value<-Y$Value/10
modelos_finales_q0.5_sc <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/modelos_finales_q0.5_sc.rds")
modelos_finales_q0.95_sc <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/modelos_finales_q0.95_sc.rds")
stations_dist <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/stations_dist.rds")

#MODELOS CON MAYOR NUMERO DE ITERACIONES. REALIZADOS EN SCRIPT MOD_BAY_FINALES_Y_CONV.R
param_mod_q0.95_def <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Analisis Exploratorio/ModelosReg/bay/param_mod_q0.95_def.rds")
param_mod_q0.5_def <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Analisis Exploratorio/ModelosReg/bay/param_mod_q0.5_def.rds")

library(spTReg)
library(quantreg)
library(zoo)
library(sf)
library(sp)
library(coda)
#------------------------PREVIO------------------------
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


#formulas
formula_q0.95_nueva <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/formula_q0.95_nueva.rds")
formula_q0.5_nueva <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/formula_q0.5_nueva.rds")

# #escalado de distancia y elev
elev_sc<-scale(stations_dist$HGHT)
dist_sc<-scale(stations_dist$DIST)


#------------------------STARTING POINTS------------------------
#obtencion de parametros de inicio
start_beta_q0.5_sc<-apply(modelos_finales_q0.5_sc[,3:18],2,mean)

start_beta_q0.95_sc<-apply(modelos_finales_q0.95_sc[,3:21],2,mean)

mod_q0.5<-lm(modelos_finales_q0.5_sc$intercept~elev_sc+dist_sc)
elev_inic_q0.5<-coef(mod_q0.5)[2]
dist_inic_q0.5<-coef(mod_q0.5)[3]
inic_procesos_q0.5<-as.matrix(cbind(mod_q0.5$residuals,sweep(modelos_finales_q0.5_sc[4:18],2,start_beta_q0.5_sc[-1])))
inic_procesos_q0.5<-inic_procesos_q0.5+rnorm(40*16,0,0.1)

mod_q0.95<-lm(modelos_finales_q0.95_sc$intercept~elev_sc+ dist_sc)
elev_inic_q0.95<-coef(mod_q0.95)[2]
dist_inic_q0.95<-coef(mod_q0.95)[3]
inic_procesos_q0.95<-as.matrix(cbind(mod_q0.95$residuals,sweep(modelos_finales_q0.95_sc[,4:21],2,start_beta_q0.95_sc[-1])))
inic_procesos_q0.95<-inic_procesos_q0.95+rnorm(40*19,0,0.1)


#------------------------MODELOS------------------------
formula_q0.5<-update(formula_q0.5_nueva,.~. + elev + dist)
formula_q0.95<-update(formula_q0.95_nueva,.~. + elev + dist)

#modelos
mod_q0.5_sc_ext<-spTm(formula_q0.5,
                  data=v_q0.5,
                  method = 'q',
                  quantile = 0.5,
                  coords = coords_km,
                  v = as.matrix(cbind(1,v_q0.5[,2:16])),
                  priors = list(
                    "beta" = list(M = rep(0, 18), P = 0.01 * 
                                    diag(18)),
                    "sigma" = c(0.1, 0.1),
                    "phi" = c(2, 100),
                    "mu" = c(0, 1 / 100)),
                  starting = list(
                    "beta" = c(start_beta_q0.5_sc,elev_inic_q0.5,dist_inic_q0.5),
                    "sigma" = 1,
                    "alpha" = inic_procesos_q0.5,
                    "hp" = c("mu" = 0, "sigma" = 50, "phi" = 3 / 100)),
                  n.samples = 5000,
                  n.burnin = 5000,
                  n.thin = 5,
                  n.report=1000
)

plot(mod_q0.5_sc_ext$p.params.samples[,'elev'])
plot(mod_q0.5_sc_ext$p.params.samples[,'dist'])
mod_q0.5_sc_ext$p.params.samples[1,'elev']
mod_q0.5_sc_ext$p.params.samples[1,'dist']

colnames(mod_q0.5_sc_ext$p.params.samples)[2:16]<-c(
  'poly(g500_45_.10, 2)1','poly(g500_45_5, 2)1', 'poly(g700, 2)1',
  'poly(g700_35_.10, 2)1','poly(g700_45_.10, 2)1','g700_45_5','I(g300^2)',
  'I(g300_45_.10^2)','I(g500^2)','poly(g500_45_.10, 2)2','poly(g500_45_5, 2)2',
  'poly(g700, 2)2','poly(g700_35_.10, 2)2','I(g700_35_5^2)','poly(g700_45_.10, 2)2'
)


mod_q0.95_sc_ext<-spTm(formula_q0.95,
                   data=v_q0.95,
                   method = 'q',
                   quantile = 0.95,
                   coords = coords_km,
                   v = as.matrix(cbind(1,v_q0.95[2:19])),
                   priors = list(
                     "beta" = list(M = rep(0, 21), P = 0.01 * 
                                     diag(21)),
                     "sigma" = c(0.1, 0.1),
                     "phi" = c(2, 100),
                     "mu" = c(0, 1 / 100)),
                   starting = list(
                     "beta" = c(start_beta_q0.95_sc,elev_inic_q0.95,dist_inic_q0.95),
                     "sigma" = 1,
                     "alpha" = inic_procesos_q0.95,
                     "hp" = c("mu" = 0, "sigma" = 1, "phi" = 3 / 100)),
                   n.samples = 5000,
                   n.burnin = 5000,
                   n.thin=5,
                   n.report = 1000
)

plot(mod_q0.95_sc_ext$p.params.samples[,'elev'])
plot(mod_q0.95_sc_ext$p.params.samples[,'dist'])
mod_q0.95_sc_ext$p.params.samples[1,'elev']
mod_q0.95_sc_ext$p.params.samples[1,'dist']

# nombres de columna a poly
colnames(mod_q0.95_sc_ext$p.params.samples)[2:19]<-c(
  'poly(g300, 2)1','g300_45_.10','poly(g300_45_5, 2)1','poly(g500, 2)1',
  'g500_45_.10','g500_45_5','poly(g700, 2)1','poly(g700_35_.10, 2)1','poly(g700_35_5, 2)1',
  'poly(g700_45_.10, 2)1','g700_45_5','poly(g300, 2)2','poly(g300_45_5, 2)2',
  'poly(g500, 2)2','poly(g700, 2)2','poly(g700_35_.10, 2)2','poly(g700_35_5, 2)2',
  'poly(g700_45_.10, 2)2'
)

saveRDS(mod_q0.5_sc_ext,'mod_q0.5_sc_ext.rds')
saveRDS(mod_q0.95_sc_ext,'mod_q0.95_sc_ext.rds')

mod_q0.95_sc_ext <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Analisis Exploratorio/ModelosReg/bay/mod_q0.95_sc_ext.rds")
mod_q0.5_sc_ext <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Analisis Exploratorio/ModelosReg/bay/mod_q0.5_sc_ext.rds")


#------------------------MEDIAS BETA------------------------
#reordenacion de columnas y cambio de nombre
traducir_nombres_coef <- function(nombres_coef) { 
  traducidos <- character(length(nombres_coef))
  
  for (i in seq_along(nombres_coef)) {
    nombre <- nombres_coef[i]
    
    if (grepl("^poly\\((.+), 2\\)1$", nombre)) {
      base <- sub("^poly\\((.+), 2\\)1$", "\\1", nombre)
      traducidos[i] <- base
    } else if (grepl("^poly\\((.+), 2\\)2$", nombre)) {
      base <- sub("^poly\\((.+), 2\\)2$", "\\1", nombre)
      traducidos[i] <- paste0("I(", base, "^2)")
    } else {
      traducidos[i] <- nombre
    }
  }
  
  return(traducidos)
}

betas<-function(vars,mod,cuantil){
  if (cuantil==0.5){
    ind<-16
  }else{
    ind<-19
  }
  params<-as.data.frame(mod)
  #params<-as.data.frame(mod$p.params.samples)
  tr<-traducir_nombres_coef(colnames(params)[2:ind])
  colnames(params)[2:ind]<-gsub('`','',tr)
  #intercepto
  int<-mean(params[['(Intercept)']])
  int<-rep(int, length=dim(stations)[1])
  
  #beta_fija
  betas_fijas<-apply(params[,vars],2,mean)
  betas_fijas <- matrix(rep(betas_fijas, each = dim(stations)[1]), nrow = dim(stations)[1])
  if (cuantil==0.5){
    elev<-mean(params[,'elev'])
    elev <- matrix(rep(elev, each = dim(stations)[1]), nrow = dim(stations)[1])
    dist<-mean(params[,'dist'])
    dist <- matrix(rep(dist, each = dim(stations)[1]), nrow = dim(stations)[1])
  }
  
  if (cuantil==0.95){
    elev<-mean(params[,'elev'])
    elev <- matrix(rep(elev, each = dim(stations)[1]), nrow = dim(stations)[1])
    dist<-mean(params[,'dist'])
    dist <- matrix(rep(dist, each = dim(stations)[1]), nrow = dim(stations)[1])
  }
  #betas espaciales (beta1,...,beta16)
  cols<-grep('beta',names(params),value=T)
  mu<-apply(params[,cols],2,mean)
  betas_esp<-matrix(mu,nrow=dim(stations)[1])
  
  #juntar en data frame
  if (cuantil==0.5){
    betas<-cbind(int,elev,dist,betas_fijas,betas_esp)
    betas<-as.data.frame(betas,row.names = stations$NAME2)
    colnames(betas)<-c('intercept','elev','dist',vars,paste0('beta',1:(length(vars)+1)))
  }
  
  if (cuantil==0.95){
    betas<-cbind(int,elev,dist,betas_fijas,betas_esp)
    betas<-as.data.frame(betas,row.names = stations$NAME2)
    colnames(betas)<-c('intercept','elev','dist',vars,paste0('beta',1:(length(vars)+1)))
  }
  
  return(betas)
}

betas_q0.5_sc_ext<-betas(vars_q0.5,mod_q0.5_sc_ext,cuantil = 0.5)
betas_q0.95_sc_ext<-betas(vars_q0.95,mod_q0.95_sc_ext,cuantil = 0.95)


betas_q0.5_sc_def<-betas(vars_q0.5,param_mod_q0.5_def,cuantil = 0.5)
betas_q0.95_sc_def<-betas(vars_q0.95,param_mod_q0.95_def,cuantil = 0.95)


#------------------------PREDICCIONES------------------------
stations_dist <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/stations_dist.rds")
predictions<-function(vars, mod, betas, df, cuantil){
  pred<-NA
  for (i in 1:dim(stations)[1]){
    ind<-which(Y$station==stations$STAID[i])
    for (j in ind){
      #inicializar en interceptos
      comp_esp<-betas[i,'beta1'] #intercepto espacial para la estacion i
      #comp_esp<-mean(mod$p.params.samples[,paste0('beta1(s',i,')')])
      comp_fija<-betas[i,'intercept'] #intercepto fijo para la estacion i
      #comp_fija<-mean(mod$p.params.samples[,'(Intercept)'])
      for (k in 1:(length(vars))){ #beta 1 es la componente espacial del intercepto
        comp_esp<-comp_esp + betas[i,paste0('beta',k+1)]*df[j,vars[k]]
        comp_fija<- comp_fija + betas[i,vars[k]]*df[j,vars[k]]
      }
      
      if (cuantil==0.5){
        pred[j]<-comp_esp+comp_fija + betas[i,'elev']*elev_sc[i] + betas[i,'dist']*dist_sc[i]
      }
      
      if (cuantil == 0.95){
        pred[j]<-comp_esp+comp_fija + betas[i,'elev']*elev_sc[i] + betas[i,'dist']*dist_sc[i]
      }
      
    }
  }
  
  return(pred)
}


# pred_q0.5_sc_ext<-predictions(vars_q0.5,mod_q0.5_sc_ext,betas_q0.5_sc_ext,v_q0.5,cuantil=0.5)
# pred_q0.95_sc_ext<-predictions(vars_q0.95,mod_q0.95_sc_ext,betas_q0.95_sc_ext,v_q0.95,cuantil=0.95)
# 
# pred_mult_ext<-cbind(Y,pred_q0.5_sc_ext,pred_q0.95_sc_ext)
# 
# saveRDS(pred_mult_ext,'pred_mult_ext.rds')
# pred_mult_ext <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Analisis Exploratorio/ModelosReg/bay/pred_mult_ext.rds")

#USO DE MODELOS DEFINITIVOS
pred_q0.5_sc_def<-predictions(vars_q0.5,param_mod_q0.5_def,betas_q0.5_sc_def,v_q0.5,cuantil=0.5)
pred_q0.95_sc_def<-predictions(vars_q0.95,param_mod_q0.95_def,betas_q0.95_sc_def,v_q0.95,cuantil=0.95)

pred_mult_def<-cbind(Y,pred_q0.5_sc_def,pred_q0.95_sc_def)

saveRDS(pred_mult_def,'pred_mult_def.rds')
pred_mult_def <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Analisis Exploratorio/ModelosReg/bay/pred_mult_def.rds")


#------------------------R1------------------------
check <- function(u, tau) {
  return(u * (tau - (u < 0)))  # Implements the quantile loss function
}

R1_bay<-function(pred,tau){
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
    rho_estacion[j]<-sum(check(pred_clean$Value[ind]-pred_clean[ind,paste0('pred_q',tau,'_sc_def')],tau=tau))
    R1_nulo_est[j]<-sum(check(mod_nulo_f$residuals[ind],tau=tau))
  }
  
  df['R1_bay_global']<-1-sum(rho_estacion)/mod_nulo_f$rho
  df_local['R1_bay_local']<-1-rho_estacion/R1_nulo_est
  
  return(list(R1_globales=df,R1_locales=df_local))
}

# R1_bay_q0.5_sc_ext<-R1_bay(pred_mult_ext,0.5)
# R1_bay_q0.95_sc_ext<-R1_bay(pred_mult_ext,0.95)
# 
# saveRDS(R1_bay_q0.5_sc_ext,'R1_bay_q0.5_sc_ext.rds')
# saveRDS(R1_bay_q0.95_sc_ext,'R1_bay_q0.95_sc_ext.rds')

R1_bay_q0.5_sc_def<-R1_bay(pred_mult_def,0.5)
R1_bay_q0.95_sc_def<-R1_bay(pred_mult_def,0.95)

saveRDS(R1_bay_q0.5_sc_def,'R1_bay_q0.5_sc_def.rds')
saveRDS(R1_bay_q0.95_sc_def,'R1_bay_q0.95_sc_def.rds')


#------------------------RHO------------------------
library(lubridate)
rho_bay<-function(predicciones,tau){
  
  #global
  #dataframe para global
  df<-matrix(NA,nrow=1,ncol=1)
  df<-as.data.frame(df)
  colnames(df)<-c('rho_bay_global')
  pred<-predicciones[[paste0('pred_q',tau,'_sc_def')]]
  dif<-Y$Value-pred
  df['rho_bay_global']<- sum(dif<0,na.rm=T)/(40*64*92)
  
  #estaciones
  df_est<-matrix(NA,nrow=dim(stations)[1],ncol=1)
  df_est<-as.data.frame(df_est,row.names = stations$NAME2)
  colnames(df_est)<-c('rho_bay_est')
  for (i in 1:dim(stations)[1]){
    ind<-which(Y$station==stations$STAID[i])
    dif<-Y$Value[ind]-pred[ind]
    df_est[i,]<-sum(dif<0,na.rm=T)/(64*92)
  }
  
  #dias
  df_dia_list <- list() #lista para dias por estacion
  
  day_month <- unique(format(Y$Date, "%d-%m"))
  df_dia<-matrix(NA,nrow=length(day_month),ncol=1)
  df_dia<-as.data.frame(df_dia,row.names = day_month)
  colnames(df_dia)<-c('rho_bay_dia')
  
  for (i in 1:length(day_month)){
    ind<-which(format(Y$Date, "%d-%m") == day_month[i])
    dif<-Y$Value[ind]-pred[ind]
    df_dia[i,]<-sum(dif<0,na.rm=T)/(64*40)
    
    #por estaciones
    for (j in 1:dim(stations)[1]){
      nombre<-stations$NAME2[j]
      
      # Si la estación aún no está en la lista, inicialízala
      if (!(nombre %in% names(df_dia_list))) {
        df_dia_list[[nombre]] <- data.frame(rho_bay_dia = rep(NA, length(day_month)), row.names = day_month)
      }
      df_temp <- df_dia_list[[nombre]]
      
      ind_2<-which(Y$station==stations$STAID[j])
      ind_2<-ind_2[which(ind_2%in%ind)]
      dif<-Y$Value[ind_2]-pred[ind_2]
      
      #guardado
      df_temp[i, 1] <- sum(dif < 0, na.rm = T) / 64
      df_dia_list[[nombre]] <- df_temp
      
      
    }
    
    
  }
  
  #años
  df_year_list<-list()#lista para años por estacion
  
  year<-unique(year(Y$Date))
  df_year<-matrix(NA,nrow=length(year),ncol=1)
  df_year<-as.data.frame(df_year,row.names = year)
  colnames(df_year)<-c('rho_bay_year')
  for (i in 1:length(year)){
    ind<-which(year(Y$Date)==year[i])
    dif<-Y$Value[ind]-pred[ind]
    df_year[i,]<-sum(dif<0,na.rm=T)/(40*92)
    
    for (j in 1:dim(stations)[1]){
      nombre<-stations$NAME2[j]
      
      # Si la estación aún no está en la lista, inicialízala
      if (!(nombre %in% names(df_year_list))) {
        df_year_list[[nombre]] <- data.frame(rho_bay_year = rep(NA, length(year)), row.names = year)
      }
      df_temp <- df_year_list[[nombre]]
      
      ind_2<-which(Y$station==stations$STAID[j])
      ind_2<-ind_2[which(ind_2%in%ind)]
      dif<-Y$Value[ind_2]-pred[ind_2]
      
      #guardado
      df_temp[i, 1] <- sum(dif < 0, na.rm = T) / 92
      df_year_list[[nombre]] <- df_temp
    }
    
  }
  
  return(list(rho_globales=df,
              rho_estaciones=df_est,
              rho_años=df_year,
              rho_dias=df_dia,
              rho_dias_est=df_dia_list,
              rho_años_est=df_year_list))
}



# rho_q0.5_ext<-rho_bay(pred_mult_ext,0.5)
# rho_q0.95_ext<-rho_bay(pred_mult_ext,0.95)
# 
# saveRDS(rho_q0.5_ext,'rho_q0.5_ext.rds')
# saveRDS(rho_q0.95_ext,'rho_q0.95_ext.rds')

rho_q0.5_def<-rho_bay(pred_mult_def,0.5)
rho_q0.95_def<-rho_bay(pred_mult_def,0.95)

saveRDS(rho_q0.5_def,'rho_q0.5_def.rds')
saveRDS(rho_q0.95_def,'rho_q0.95_def.rds')


# plot general de desagregación
rho_dia_año<-function(rho_q0.5,rho_q0.95){
  par(mfrow=c(1,2))
  #dias
  plot(1:length(rho_q0.5$rho_dias$rho_bay_dia), 
       rho_q0.5$rho_dias$rho_bay_dia, type = "l", xlab = "l", 
       ylab = expression(rho[l](tau)),
       ylim=c(0.3,1))
  abline(h=0.5,col='red')
  lines(1:length(rho_q0.95$rho_dias$rho_bay_dia), 
        rho_q0.95$rho_dias$rho_bay_dia)
  abline(h=0.95,col='red')
  
  #años
  plot(1:length(rho_q0.5$rho_años$rho_bay_year), 
       rho_q0.5$rho_años$rho_bay_year, type = "l", xlab = "t", 
       ylab = expression(rho[t](tau)),
       ylim=c(0.3,1))
  abline(h=0.5,col='red')
  lines(1:length(rho_q0.95$rho_dias$rho_bay_dia), 
        rho_q0.95$rho_dias$rho_bay_dia)
  abline(h=0.95,col='red')
  
}

rho_dia_año(rho_q0.5_def,rho_q0.95_def)


# plots de desagregacion de todas estaciones

par(mfrow=c(10,8))
for (i in 1:dim(stations)[1]){
  est<-stations$NAME2[i]
  #dias
  plot(1:92,rho_q0.5_def$rho_dias_est[[i]][, 1], type='l',
       xlab = 'l', ylim=c(0,1),ylab = expression(rho[l](tau)),
       main=paste(est,'(días)'))
  abline(h=0.5,col='red')
  lines(1:92, 
        rho_q0.95_def$rho_dias_est[[i]][,1])
  abline(h=0.95,col='red')
  
  #años
  plot(1:64,rho_q0.5_def$rho_años_est[[i]][, 1], type='l',
       xlab = 't', ylim=c(0,1),ylab = expression(rho[l](tau)),
       main=paste(est,'(años)'))
  abline(h=0.5,col='red')
  lines(1:64, 
        rho_q0.95_def$rho_años_est[[i]][,1])
  abline(h=0.95,col='red')

}


