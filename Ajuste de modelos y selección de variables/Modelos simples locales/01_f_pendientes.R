
library(quantreg)
library(tidyverse)

met_ajuste<-function(mod,mod_nulo){
  return(1-mod$rho/mod_nulo$rho)
}

met_ajuste_adj<-function(mod,mod_nulo,n,p){
  
  R<-met_ajuste(mod,mod_nulo)
  
  R_adj<-1-(1-R)*(n-1)/(n-p-1)
  
  return(R_adj)
}

pendientes_R<-function(stations,Y,X,scale=FALSE){
  
  estaciones<-stations$STAID
  columnas<-colnames(X)[-c(1,2,3)] #columnas menos fecha y estaciones
  
  coef_list<-list()
  
  
  
  for (col in columnas){
    coef_list[[paste0('q0.5_',col)]]<-rep(NA,length(estaciones))
    coef_list[[paste0('int_q0.5_',col)]]<-rep(NA,length(estaciones))
    coef_list[[paste0('q0.95_',col)]]<-rep(NA,length(estaciones))
    coef_list[[paste0('int_q0.95_',col)]]<-rep(NA,length(estaciones))
    coef_list[[paste0('R_q0.5_',col)]]<-rep(NA,length(estaciones))
    coef_list[[paste0('R_q0.95_',col)]]<-rep(NA,length(estaciones))
  }
  
  for (i in 1:length(estaciones)){
    
    ind<-which(Y$station==estaciones[i])
    
    for (col in columnas){
      
      if (scale==T){
        aux<-scale(X[[col]])
      }else{
        aux<-X[[col]]
      }
      
      #modelo nulo
      mod_nulo<-rq(Y$Value/10~1,tau=c(0.5,0.95), subset = ind)
      #modelo
      mod<-rq(Y$Value/10~aux,tau=c(0.5,0.95),subset=ind)
      
      coef_list[[paste0("q0.5_", col)]][i] <- coef(mod)[2, 1]
      coef_list[[paste0('int_q0.5_',col)]][i]<- coef(mod)[1,1]
      coef_list[[paste0("q0.95_", col)]][i] <- coef(mod)[2, 2]
      coef_list[[paste0('int_q0.95_',col)]][i]<- coef(mod)[1,2]
      coef_list[[paste0("R_q0.5_", col)]][i] <- met_ajuste(mod,mod_nulo)[1]
      coef_list[[paste0("R_q0.95_", col)]][i] <- met_ajuste(mod,mod_nulo)[2]
      
    }
  
  }
  
  coef_df <- data.frame(
    station = estaciones, 
    coef_list)
  
  return(coef_df)
}

#obtencion metricas ajuste para modelos multivariantes
R1<-function(df,formula,Y_ref,stations,n=NULL,p=NULL,tipo){
  estaciones<-stations$STAID
  
  if(tipo==1){
    R_list<-list()
    
    for (i in 1:length(estaciones)){
      ind<-which(Y_ref$station==estaciones[i])
      
      mod_nulo<-rq(df[[1]]~1,tau=c(0.5,0.95),subset = ind)
      mod <- rq(formula, data=df[ind,], tau=c(0.5,0.95))
      
      R_list[['R_q0.5']][i]=round(met_ajuste(mod,mod_nulo)[1],3)
      R_list[['R_q0.95']][i]=round(met_ajuste(mod,mod_nulo)[2],3)
    }
    
    R_df<-data.frame(
      station=estaciones,
      row.names = stations$NAME2,
      R_list
    )
  }
  
  if (tipo==2){
    R_list<-list()
    
    for (i in 1:length(estaciones)){
      ind<-which(Y_ref$station==estaciones[i])
      
      mod_nulo<-rq(df[[1]]~1,tau=c(0.5,0.95),subset = ind)
      mod <- rq(formula, data=df[ind,], tau=c(0.5,0.95))
      
      R_list[['R_q0.5']][i]=round(met_ajuste_adj(mod,mod_nulo,n,p)[1],3)
      R_list[['R_q0.95']][i]=round(met_ajuste_adj(mod,mod_nulo,n,p)[2],3)
    }
    
    R_df<-data.frame(
      station=estaciones,
      row.names = stations$NAME2,
      R_list
    )
  }
  
  return(R_df)
  
}


