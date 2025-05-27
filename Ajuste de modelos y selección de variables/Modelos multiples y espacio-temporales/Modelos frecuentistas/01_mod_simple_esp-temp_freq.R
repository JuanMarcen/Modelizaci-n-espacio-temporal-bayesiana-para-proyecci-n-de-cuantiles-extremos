rm(list=ls())
#portatil
Y <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Y.rds")
X <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/X.rds")
X_cuadrado <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/X_cuadrado.rds")
coef_R <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/coef_R.rds")
coef_R_cuadrados <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/coef_R_cuadrados.rds")
stations <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/stations.rds")
stations_dist<-readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/stations_dist.rds")
vars_q0.5 <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/vars_q0.5.rds")
vars_q0.95 <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/vars_q0.95.rds")


vars_c<-c('g300','g500','g700')

library(quantreg)

#------------------------FUNCIONES------------------------
df_todos<-function(coef,coef_cuadrados, vars, cuantil){
  df<-matrix(NA,nrow=40)
  df<-as.data.frame(df)
  vars<-sort(vars) #ordeno, primero normal y luego cuadrados
  ind<-grep("^I\\(", vars)[1] #sacar indice de la primera variable al cuadrado
  
  if (is.na(ind)){
    ind<-length(vars)+1
  }
  
  if(cuantil == 0.5){
    expr<-'q0.5_'
  }
  
  if(cuantil == 0.95){
    expr<-'q0.95_'
  }
  
  for (i in 1:(ind-1)){
    df[[vars[i]]]<-coef[[paste0(expr,vars[i])]]
    df[[paste0('int_',vars[i])]]<-coef[[paste0('int_',expr,vars[i])]]
  }
  
  if (ind!=length(vars)+1){
    for (i in ind:length(vars)){
      df[[vars[i]]]<-coef_cuadrados[[paste0(expr,gsub("^I\\(|\\^2\\)$", "", vars[i]))]]
      df[[paste0('int_',vars[i])]]<-coef_cuadrados[[paste0('int_',expr,gsub("^I\\(|\\^2\\)$", "", vars[i]))]]
    }
  }
  
  df$lat<-stations_dist$LAT
  df$lon <- stations_dist$LON
  df$elev<-stations_dist$HGHT
  df$dist<-stations_dist$DIST
  df$dist_e1<-stations_dist$DIST_e1
  df$dist_e2<-stations_dist$DIST_e2
  df$dist_e3<-stations_dist$DIST_e3
  df$dist_e4<-stations_dist$DIST_e4
  
  df<-df[,-1]
  return(df)
}

df_q0.5<-df_todos(coef_R,coef_R_cuadrados,vars_q0.5,cuantil=0.5)
df_q0.95<-df_todos(coef_R,coef_R_cuadrados,vars_q0.95,cuantil=0.95)
df_c_q0.5<-df_todos(coef_R,coef_R_cuadrados,vars_c,cuantil=0.5)
df_c_q0.95<-df_todos(coef_R,coef_R_cuadrados,vars_c,cuantil=0.95)

sel_var_esp<-function(data,vars){
  vars_esp<-list()
  vars<-sort(vars)
  ind<-grep("^I\\(", vars)[1]
  if (is.na(ind)){
    ind<-length(vars)+1
  }
  variables<-seq(1,(ind-1)*2,by=2)
  variables_int<-variables + 1
  

  #selección de variables
  for (i in variables){
    mod1<-lm(data[[i]]~lat+lon+log1p(elev)+log1p(dist),data=data)
    mod2<-step(mod1,direction='both',trace=F)
    vars_esp[[colnames(data)[i]]]<-names(mod2$coefficients)[-1]
  }
  
  for (i in variables_int){
    mod1<-lm(data[[i]]~lat+lon+elev+dist,data=data)
    mod2<-step(mod1,direction='both',trace=F)
    vars_esp[[colnames(data)[i]]]<-names(mod2$coefficients)[-1]
  }
  
  if(ind!=length(vars)+1){
    variables_sq<-seq((ind-1)*2 + 1,length(vars)*2,by=2) 
    variables_sq_int<-variables_sq+1
    
    for (i in variables_sq){
      mod1<-lm(data[[i]]~lat+lon+log1p(elev)+log1p(dist),data=data)
      mod2<-step(mod1,direction='both',trace=F)
      vars_esp[[colnames(data)[i]]]<-names(mod2$coefficients)[-1]
    }
    
    for (i in variables_sq_int){
      mod1<-lm(data[[i]]~lat+lon+elev+dist,data=data)
      mod2<-step(mod1,direction='both',trace=F)
      vars_esp[[colnames(data)[i]]]<-names(mod2$coefficients)[-1]
    }
  }
  
  return(vars_esp)
}

vars_esp_q0.5<-sel_var_esp(df_q0.5,vars_q0.5)
saveRDS(vars_esp_q0.5,'vars_esp_q0.5.rds')
vars_esp_q0.95<-sel_var_esp(df_q0.95,vars_q0.95)
saveRDS(vars_esp_q0.95,'vars_esp_q0.95.rds')
vars_esp_c_q0.5<-sel_var_esp(df_c_q0.5,vars_c)
vars_esp_c_q0.95<-sel_var_esp(df_c_q0.95,vars_c)


#------------------------PREVIO MODELO ESPACIo-TEMPORAL------------------------
n_est<-dim(stations_dist)[1]
n_rep<-dim(X)[1]/n_est

elev<-rep(stations_dist$HGHT,each=n_rep)
dist<-rep(stations_dist$DIST,each=n_rep)
lat<-rep(stations_dist$LAT, each=n_rep)
lon<-rep(stations_dist$LON,each=n_rep)
id<-rep(stations_dist$STAID, each=n_rep)
id<-factor(id)

df_esp<-data.frame(
  Date=X$Date,
  station = id,
  Y = Y$Value/10,
  lat = lat,
  lon = lon,
  elev = elev,
  dist = dist
)

df_esp<-cbind(df_esp,X[,-c(1,2,3)])
saveRDS(df_esp,'df_esp.rds')


#formula para cada variable
crear_formulas <- function(vars,vars_esp) {
  formulas<-list()
  vars<-sort(vars)
  
  for(i in vars){
    interacciones <- paste(paste0(i,":I(", vars_esp[[i]], ")", collapse = " + "))
    otros <- paste(vars_esp[[paste0('int_',i)]], collapse = " + ")
    formula<- paste("Y ~ ",i," +", interacciones, "+", otros)
    
    formulas[[i]]<-as.formula(formula)
  }
  
  return(formulas)
}

formulas_q0.5<-crear_formulas(vars_q0.5,vars_esp_q0.5)
formulas_q0.95<-crear_formulas(vars_q0.95,vars_esp_q0.95)
formulas_c_q0.5<-crear_formulas(vars_c,vars_esp_c_q0.5)
formulas_c_q0.95<-crear_formulas(vars_c,vars_esp_c_q0.95)



#------------------------MODELOS ESPACIO-TEMPORALES SIMPLES------------------------
#funcion de perdida para R1 locales
check <- function(u, tau) {
  return(u * (tau - (u < 0)))  # Implements the quantile loss function
}

df_esp_clean<-na.omit(df_esp)
modelo_espacial<-function(formulas,data,tau){
  #data sin nulos para hacer bien los locales
  data_clean<-na.omit(data)
  
  #dataframe para globales
  df<-matrix(NA,nrow=length(formulas),ncol=2)
  df<-as.data.frame(df,row.names = names(formulas))
  colnames(df)<-c('R1_global_esp','R1_modificado_esp')
  R1_global_esp<-rep(NA,length(formulas))
  R1_modificado_esp<-rep(NA,length(formulas))
  
  #dataframe para locales
  df_local<-matrix(NA,nrow=dim(stations)[1],ncol=length(formulas))
  df_local<-as.data.frame(df_local,row.names = stations$NAME2)
  colnames(df_local)<-names(formulas)
  
  # lista para guardar coeficientes
  lista_coeficientes <- list()
  
  #modelos nulos, son para todas variables igual
  mod_nulo<-rq(Y~1,data=data,tau=tau)
  mod_nulo_f<-rq(Y~station,data=data,tau=tau)
  
  for (i in 1:length(formulas)){
    mod_q<-rq(formulas[[i]],data=data,tau=tau)
    #guardado de R1's globales
    R1_global_esp[i]<-1-mod_q$rho/mod_nulo$rho
    R1_modificado_esp[i] <- 1-mod_q$rho/mod_nulo_f$rho
    
    #R1 locales
    rho_estacion<-rep(NA,length=dim(stations_dist)[1])
    for (j in 1:length(rho_estacion)){
      rho_estacion[j]<-sum(check(mod_q$residuals[df_esp_clean$station==stations_dist$STAID[[j]]],tau=tau))
    }
    R1_nulo_est<-rep(NA,length=dim(stations_dist)[1])
    for (j in 1:length(R1_nulo_est)){
      R1_nulo_est[j]<-sum(check(mod_nulo_f$residuals[df_esp_clean$station==stations_dist$STAID[[j]]],tau=tau))
    }
    
    df_local[,i]<-1-rho_estacion/R1_nulo_est
    
    lista_coeficientes[[names(formulas)[i]]] <- coef(mod_q)
    
  }
  
  df[['R1_global_esp']]<-R1_global_esp
  df[['R1_modificado_esp']]<-R1_modificado_esp
  
  return(list(R1_globales=df,R1_locales=df_local,coeficientes_modelos = lista_coeficientes))
}

R1_esp_q0.5<-modelo_espacial(formulas_q0.5,df_esp,tau=0.5) 
saveRDS(R1_esp_q0.5,'R1_esp_q0.5.rds')
R1_esp_q0.95<-modelo_espacial(formulas_q0.95,df_esp,tau=0.95)
saveRDS(R1_esp_q0.95,'R1_esp_q0.95.rds')

R1_esp_c_q0.5<-modelo_espacial(formulas_c_q0.5,df_esp,tau=0.5)
saveRDS(R1_esp_c_q0.5,'R1_esp_c_q0.5.rds')
R1_esp_c_q0.95<-modelo_espacial(formulas_c_q0.95,df_esp,tau=0.95)
saveRDS(R1_esp_c_q0.95,'R1_esp_c_q0.95.rds')




