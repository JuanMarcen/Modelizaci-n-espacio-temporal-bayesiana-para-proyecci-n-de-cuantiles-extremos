rm(list=ls())

library("sf")
library("sp")
library("tidyverse")
library("rnaturalearth")
library("rnaturalearthdata")

setwd('C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Analisis Exploratorio/ModelosReg/interpolacion')
grid_dist <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Analisis Exploratorio/ModelosReg/interpolacion/grid_dist.rds")
grid_elev <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Analisis Exploratorio/ModelosReg/interpolacion/grid_elev.rds")
grid_grados <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Analisis Exploratorio/ModelosReg/interpolacion/grid_grados.rds")
grid <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Analisis Exploratorio/ModelosReg/interpolacion/grid.rds")
limits <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Analisis Exploratorio/ModelosReg/interpolacion/limits.rds")
background <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Analisis Exploratorio/ModelosReg/interpolacion/background.rds")
#OBTENIDOS DEL SCRIPT 02_mod_esp-temp_final_freq.R (OCUPAN DEMASIADO)
R1_esp_f_q0.95 <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/R1_esp_f_q0.95.rds")
R1_esp_f_q0.5 <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/R1_esp_f_q0.5.rds")
vars_q0.5 <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/vars_q0.5.rds")
vars_q0.95 <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/vars_q0.95.rds")
Y <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Y.rds")


#------------------------INTERPOLACION------------------------

pred_freq_final<-function(modelo, vars){
  ind<-length(vars)+1
  coef<-modelo[["modelo"]][["coefficients"]]
  nombres<-names(coef)
  
  #intercepto
  nombres_sin_g <- nombres[!grepl("g", nombres)]
  
  b1<-coef['(Intercept)']
  
  for (j in 2:length(nombres_sin_g)){
    aux<-nombres_sin_g[j]
    if(aux=='lat'){
      aux = grid_grados[,2]
    }else if (aux=='lon'){
      aux = grid_grados[,1]
    }else if (aux=='dist'){
      aux = grid_dist
    }else if(aux=='elev'){
      aux = grid_elev
    }else if(aux=='log1p(dist)'){
      aux = log1p(grid_dist)
    }else if (aux=='log1p(elev)'){
      aux = log1p(grid_dist)
    }
    
    b1<- b1 + coef[nombres_sin_g][j]*aux
    
  }
  
  for (i in 2:ind){ #coeficientes de regresion
    nombres_aux<- nombres[grepl(paste0(vars[i-1],':'),fixed = T, nombres)]
    nombres_aux
    vars_esp <- sub(".*:I\\((.*)\\)", "\\1", nombres_aux)
    
    beta<-coef[vars[i-1]]
    
    for (j in 1:length(vars_esp)){
      aux<-vars_esp[j]
      if(aux=='lat'){
        aux = grid_grados[,2]
      }else if (aux=='lon'){
        aux = grid_grados[,1]
      }else if (aux=='dist'){
        aux = grid_dist
      }else if(aux=='elev'){
        aux = grid_elev
      }else if(aux=='log1p(dist)'){
        aux = log1p(grid_dist)
      }else if (aux=='log1p(elev)'){
        aux = log1p(grid_dist)
      }
      
      beta<-beta + coef[nombres_aux][j]*aux
      
    }
    
    assign(paste0('b',i),beta)
    
  }
  
  return(
    setNames(
      mget(paste0("b", 1:ind)), 
      paste0("beta", 1:ind)
    )
  )
}


pred_freq_final_q0.5<-pred_freq_final(R1_esp_f_q0.5,vars_q0.5)
pred_freq_final_q0.95<-pred_freq_final(R1_esp_f_q0.95,vars_q0.95)

saveRDS(pred_freq_final_q0.5,'pred_freq_final_q0.5.rds')
saveRDS(pred_freq_final_q0.95,'pred_freq_final_q0.95.rds')


#------------------------PREDICCIÓN CUANTILES------------------------
# DEMASIADO GRANDE: SCRIPT 02_proy_geop_grid.R
X_grid <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Analisis Exploratorio/ModelosReg/interpolacion/X_grid.rds")

pred_cuantil<-function(X_grid,vars,pred_betas){
  
  pred_cuantil<-matrix(NA,nrow=length(unique(Y$Date)),ncol=790)
  pred_cuantil<-as.data.frame(pred_cuantil)
  
  for (i in 1:length(unique(Y$Date))){
    fecha<-unique(Y$Date)[i]
    ind<-which(X_grid$Date==fecha)
    G<-X_grid[ind,-c(1,2,3)]
    # Extraer las variables cuadráticas (entre I(...) )
    cuad_vars <- gsub("^I\\((.*)\\^2\\)$", "\\1", vars[grepl("^I\\(.*\\^2\\)$", vars)])
    # Crear las columnas cuadradas a partir de G
    cuad_df <- as.data.frame(sapply(cuad_vars, function(var) G[[var]]^2))
    names(cuad_df) <- paste0("I(", cuad_vars, "^2)")
    # Extraer las variables normales
    norm_vars <- vars[!grepl("^I\\(.*\\^2\\)$", vars)]
    norm_df <- G[, norm_vars, drop = FALSE]
    # Combinar en orden según vars
    df_final <- cbind(norm_df, cuad_df)[, vars]
    
    pred<-pred_betas[['beta1']]
    
    for (j in 1:length(vars)){
      pred<-pred + matrix(pred_betas[[paste0('beta',j+1)]],nrow=1) * t(df_final[,vars[j]])
    }
    
    pred_cuantil[i,]<-pred
    print(paste(fecha,pred_cuantil[i,1]))
  }
  
  rownames(pred_cuantil)<-unique(Y$Date)
  
  return(pred_cuantil)
  
}

pred_cuantil_q0.5_freq<-pred_cuantil(X_grid,vars_q0.5,pred_freq_final_q0.5)
pred_cuantil_q0.95_freq<-pred_cuantil(X_grid,vars_q0.95,pred_freq_final_q0.95)

saveRDS(pred_cuantil_q0.5_freq,'pred_cuantil_q0.5_freq.rds')
saveRDS(pred_cuantil_q0.95_freq,'pred_cuantil_q0.95_freq.rds')
