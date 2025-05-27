rm(list=ls())
setwd("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos")
vars_q0.5 <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/vars_q0.5.rds")
vars_q0.95 <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/vars_q0.95.rds")
vars_esp_q0.95 <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/vars_esp_q0.95.rds")
vars_esp_q0.5 <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/vars_esp_q0.5.rds")

library(quantreg)

#------------------------FORMULA FINAL------------------------
formula_final<-function(vars,vars_esp){
  vars<-sort(vars)
  
  #interacciones
  interacciones<-NA
  j<-1
  for(i in vars){
    interacciones[j] <- paste(paste0(i,":I(", vars_esp[[i]], ")", collapse = " + "))
    j<-j+1
  }
  
  f_int<-paste(interacciones,collapse = '+')
  
  #interceptos
  sub_lista<-vars_esp[startsWith(names(vars_esp),'int_')]
  sueltos <- unique(unlist(sub_lista,use.names = F))
  
  f_sueltos<-paste(sueltos,collapse = '+')
  
  #variables solas
  v<-paste(vars,collapse = '+')
  
  #final
  formula<-paste('Y ~',f_sueltos,'+',v,'+',f_int)
  
  return(as.formula(formula))
}

formula_q0.5<-formula_final(vars_q0.5,vars_esp_q0.5)
formula_q0.95<-formula_final(vars_q0.95,vars_esp_q0.95)



#------------------------MODELO ESPACIAL(FREQ)------------------------
df_esp <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/df_esp.rds")
stations <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/stations.rds")

# mod_global_q0.5<-rq(formula_q0.5,data=df_esp,tau=0.5)
# mod_nulo_f_q0.5<-rq(Y~station,data=df_esp,tau=0.5)
# 
# 1-mod_global_q0.5$rho/mod_nulo_f_q0.5$rho

check <- function(u, tau) {
  return(u * (tau - (u < 0)))  # Implements the quantile loss function
}

modelo_espacial_final<-function(formulas,data,tau){
  #data sin nulos para hacer bien los locales
  data_clean<-na.omit(data)
  
  #dataframe para globales
  df<-matrix(NA,1,ncol=2)
  df<-as.data.frame(df,row.names = names(formulas))
  colnames(df)<-c('R1_global_esp','R1_modificado_esp')
  R1_global_esp<-rep(NA,1)
  R1_modificado_esp<-rep(NA,1)
  
  #dataframe para locales
  df_local<-matrix(NA,nrow=dim(stations)[1],ncol=1)
  df_local<-as.data.frame(df_local,row.names = stations$NAME2)
  colnames(df_local)<-names(formulas)
  
  #modelos nulos, son para todas variables igual
  mod_nulo<-rq(Y~1,data=data,tau=tau)
  mod_nulo_f<-rq(Y~station,data=data,tau=tau)
  
  mod_q<-rq(formulas,data=data,tau=tau)
  #guardado de R1's globales
  R1_global_esp[1]<-1-mod_q$rho/mod_nulo$rho
  R1_modificado_esp[1] <- 1-mod_q$rho/mod_nulo_f$rho
  
  #R1 locales
  rho_estacion<-rep(NA,length=dim(stations)[1])
  for (j in 1:length(rho_estacion)){
    rho_estacion[j]<-sum(check(mod_q$residuals[data_clean$station==stations$STAID[[j]]],tau=tau))
  }
  R1_nulo_est<-rep(NA,length=dim(stations)[1])
  for (j in 1:length(R1_nulo_est)){
    R1_nulo_est[j]<-sum(check(mod_nulo_f$residuals[data_clean$station==stations$STAID[[j]]],tau=tau))
  }
  
  df_local[,1]<-1-rho_estacion/R1_nulo_est
  
  df[['R1_global_esp']]<-R1_global_esp
  df[['R1_modificado_esp']]<-R1_modificado_esp
  
  return(list(R1_globales=df,R1_locales=df_local,modelo=mod_q))
}

R1_esp_f_q0.5<-modelo_espacial_final(formula_q0.5,df_esp,tau=0.5)
saveRDS(R1_esp_f_q0.5,'R1_esp_f_q0.5.rds')
R1_esp_f_q0.95<-modelo_espacial_final(formula_q0.95,df_esp,tau=0.95)
saveRDS(R1_esp_f_q0.95,'R1_esp_f_q0.95.rds')

R1_esp_f_q0.95 <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/R1_esp_f_q0.95.rds")
R1_esp_f_q0.5 <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/R1_esp_f_q0.5.rds")


