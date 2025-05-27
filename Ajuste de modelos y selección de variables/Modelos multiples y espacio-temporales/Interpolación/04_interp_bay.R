rm(list=setdiff(ls(),'grid'))

setwd('C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Analisis Exploratorio/ModelosReg/bay')
df <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/df.rds")
stations <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/stations.rds")
stations_dist <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/stations_dist.rds")
grid_km <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Analisis Exploratorio/ModelosReg/interpolacion/grid_km.rds")
grid <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Analisis Exploratorio/ModelosReg/interpolacion/grid.rds")
background <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Analisis Exploratorio/ModelosReg/interpolacion/background.rds")
limits <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Analisis Exploratorio/ModelosReg/interpolacion/limits.rds")
vars_q0.5 <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/vars_q0.5.rds")
vars_q0.95 <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/vars_q0.95.rds")
Y <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Y.rds")
X_cuadrado <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/X_cuadrado.rds")

#MODELOS BAYESIANOS DEFINITIVOS
param_mod_q0.95_def <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Analisis Exploratorio/ModelosReg/bay/param_mod_q0.95_def.rds")
param_mod_q0.5_def <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Analisis Exploratorio/ModelosReg/bay/param_mod_q0.5_def.rds")

library(lubridate)
library(spTReg)
library(zoo)
library("sf")
library("sp")
library("tidyverse")
library("rnaturalearth")
library("rnaturalearthdata")
library(stringr)
library(polynom)
library(coda)

#------------------------PREVIO------------------------
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


colnames(X_cuadrado)[-c(1,2,3)]<-paste0('I(',colnames(X_cuadrado)[-c(1,2,3)],'^2)')
df_conj_filled<-cbind(df_filled,X_cuadrado[-c(1,2,3)])

#conjunto de datos para medias y desviaciones tipicas
#ya ordenados segun variables
v_q0.5_no_sc <- data.frame(
  
  poly_g500_45_.10_1 = poly(df_conj_filled$g500_45_.10, 2)[, 1],
  poly_g500_45_5_1 = poly(df_conj_filled$g500_45_5, 2)[, 1],
  
  poly_g700_1 = poly(df_conj_filled$g700,2)[,1],
  
  poly_g700_35_.10_1 = poly(df_conj_filled$g700_35_.10, 2)[, 1],
  poly_g700_45_.10_1 = poly(df_conj_filled$g700_45_.10, 2)[, 1],
  
  g700_45_5=df_conj_filled$g700_45_5,
  
  `I(g300^2)`= df_conj_filled$`I(g300^2)`,     #(df_conj_filled$g300)^2,
  `I(g300_45_.10^2)`=df_conj_filled$`I(g300_45_.10^2)`, #(df_conj_filled$g300_45_.10)^2,
  `I(g500^2)`=df_conj_filled$`I(g500^2)`,        #(df_conj_filled$g500)^2,
  
  poly_g500_45_.10_2 = poly(df_conj_filled$g500_45_.10, 2)[, 2],
  poly_g500_45_5_2 = poly(df_conj_filled$g500_45_5, 2)[, 2],
  poly_g700_2 = poly(df_conj_filled$g700,2)[,2],
  poly_g700_35_.10_2 = poly(df_conj_filled$g700_35_.10, 2)[, 2],
  `I(g700_35_5^2)`=  df_conj_filled$`I(g700_35_5^2)`, #(df_conj_filled$g700_35_5)^2,
  poly_g700_45_.10_2 = poly(df_conj_filled$g700_45_.10, 2)[, 2]
  
)

v_q0.95_no_sc <- data.frame(

  poly_g300_1 = poly(df_conj_filled$g300, 2)[, 1],
  g300_45_.10 = df_conj_filled$g300_45_.10,
  poly_g300_45_5_1 = poly(df_conj_filled$g300_45_5, 2)[, 1],
  
  poly_g500_1 = poly(df_conj_filled$g500, 2)[, 1],
  g500_45_.10 = df_conj_filled$g500_45_.10,
  g500_45_5 = df_conj_filled$g500_45_5,
  
  poly_g700_1 = poly(df_conj_filled$g700, 2)[, 1],
  poly_g700_35_.10_1 = poly(df_conj_filled$g700_35_.10, 2)[, 1],
  poly_g700_35_5_1 = poly(df_conj_filled$g700_35_5, 2)[, 1],
  poly_g700_45_.10_1 = poly(df_conj_filled$g700_45_.10, 2)[, 1],
  g700_45_5 = df_conj_filled$g700_45_5,
  
  poly_g300_2 = poly(df_conj_filled$g300, 2)[, 2],
  poly_g300_45_5_2 = poly(df_conj_filled$g300_45_5, 2)[, 2],
  poly_g500_2 = poly(df_conj_filled$g500, 2)[, 2],
  poly_g700_2 = poly(df_conj_filled$g700, 2)[, 2],
  poly_g700_35_.10_2 = poly(df_conj_filled$g700_35_.10, 2)[, 2],
  poly_g700_35_5_2 = poly(df_conj_filled$g700_35_5, 2)[, 2],
  poly_g700_45_.10_2 = poly(df_conj_filled$g700_45_.10, 2)[, 2]
  
)

#------------------------------------------------
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
#------------------------INTERPOLACION------------------------

pred_nuevos_final_2<-function(modelo,coords, grid, scale=FALSE, df, vars, df_medias){
  
  ind<-length(vars)+1
  
  #prediccion de todos betai, del 1 a la longitud de variables + 1
  # parametros <- modelo$p.params.samples
  parametros <- modelo
  for (i in 1:ind) {
    v<-krigeBayes(parametros[,paste0('beta',i,'(s',1:40,')')],
                  hp=parametros[,paste0(c('mu','sigma','phi'),i)],
                  coords = coords, newcoords= grid)
    assign(paste0("b", i), v)
  }
  
  #OBJETIVO: b_i = b_i + b_i(s)
  
  #los beta 1 al beta ind estan ordenados segun vars
  params<-as.data.frame(parametros)
  params_c<-params
  colnames(params_c)[2:ind]<-gsub('`','',colnames(params_c)[2:ind])
  tr<-traducir_nombres_coef(colnames(params)[2:ind])
  tr<-gsub('`','',tr) #quito comas
  colnames(params)[2:ind]<-tr
  betas_fijos<-params[,c('(Intercept)',vars)]
  
  #dataframe con nombres poly para saber localizarlos
  nombres <- colnames(params_c)[match(vars, tr)+1]
  
  #ya esta todo ordenados, tanto nombres como betas_fijos. Es decir beta_fijo[,1] se correpsonde con beta1 ppredicho
  
  #ahora tegno que ver, que variables estan con poly
  variables_poly <- str_match(nombres, "poly\\(([^,]+),")[,2]
  variables_poly <- variables_poly[!is.na(variables_poly)]
  variables_poly<- sort(unique(variables_poly))
  # y sacar sus coeficientes del polinomio ortogonal (a,b,c,d,e)
  coef_orth<-data.frame(matrix(NA,ncol=length(variables_poly),nrow=5),row.names = c('a','b','c','d','e'))
  colnames(coef_orth)<-variables_poly
  for (var in variables_poly){
    p<-poly.orth(df[,var],2)
    coef_orth['a',var]<-coef(p[[2]])[1]
    coef_orth['b',var]<-coef(p[[2]])[2]
    coef_orth['c',var]<-coef(p[[3]])[1]
    coef_orth['d',var]<-coef(p[[3]])[2]
    coef_orth['e',var]<-coef(p[[3]])[3]
  }
  
  coef_orth<-cbind(coef_orth,coef_orth) #duplicado para luego bucle entero mejor
  colnames(coef_orth)[(length(variables_poly)+1):ncol(coef_orth)]<-paste0('I(',colnames(coef_orth)[1:length(variables_poly)],'^2)')
  
  #medias y desviaciones típicas ORDENADAS NO SE INCLUYE EL INTERCEPTO. 
  # Tambien se trata el caso en que solo se de una variable
  if (length(vars)==1){
    medias<-mean(df[,vars])
    desv<-sd(df[,vars])
  }else{
    # medias<-colMeans(df[,vars])
    # desv<-apply(df[,vars],2,sd)
    medias<-colMeans(df_medias)
    desv<-apply(df_medias,2,sd)
  }
  
  elev_mu<-mean(stations_dist$HGHT)
  dist_mu<-mean(stations_dist$DIST)
  elev_sd<-sd(stations_dist$HGHT)
  dist_sd<-sd(stations_dist$DIST)
  
  #betas espaciales BUCLE ENTERO
  if (scale==FALSE){
    for (i in 1:ind){
      assign(paste0('b',i),betas_fijos[,i]+get(paste0('b',i)))
    }
  }else{ #scale ==true --> sacar coeficiente que van multiplicando a cada covariable SIN ESCALAR
    for (i in 1:ind){
      
      if (i==1){ #intercepto
        #print('INTERCEPTO')
        res <- betas_fijos[,1] + get("b1") #intercepto
        # añadir coeficientes de elev y dist
        res<- res - parametros[,'elev']*elev_mu/elev_sd - parametros[,'dist']*dist_mu/dist_sd
        cont<-1
        for (j in 2:ind) {
          if (vars[j-1] %in% colnames(coef_orth)){ #poly
            if (cont<=length(variables_poly)){ #polys lineals
              #print('poly')
              res <- res + (betas_fijos[,j] + get(paste0("b", j)))*(coef_orth['a',vars[j-1]]-medias[j-1])/desv[j-1]
              cont<-cont +1
            }else{
              #print('poly_sq')
              res <- res + (betas_fijos[,j] + get(paste0("b", j)))*(coef_orth['c',vars[j-1]]-medias[j-1])/desv[j-1]
              cont<-cont +1
            }
          }else{ #normales
            #print('normal')
            res <- res - betas_fijos[,j]* medias[j-1] / desv[j-1] - get(paste0("b", j))* medias[j-1] / desv[j-1]
          }
        }
        
        assign(paste0('b',1),res)
        
        #print('NORMALES')
        
      }else{
        
        if(vars[i-1] %in% colnames(coef_orth)){#poly
          #distincion en si quiero coeficinete de sgundo grado o lineal
          if(grepl('I',vars[i-1])==F){#lineal
            #print('poly lineal')
            var_aux<-paste0('I(',vars[i-1],'^2)')
            index<-which(var_aux==vars)
            beta<-(betas_fijos[,i]+get(paste0('b',i)))*coef_orth['b',vars[i-1]]/desv[i-1] + (betas_fijos[,index+1]+get(paste0('b',index+1)))*coef_orth['d',var_aux]/desv[index]
            assign(paste0('b',i), beta )
          }else{#cuadratico
            #print('poly sq')
            beta<-(betas_fijos[,i]+get(paste0('b',i)))*coef_orth['e',vars[i-1]]/desv[i-1]
            assign(paste0('b',i), beta )
          }
        }else{
          #print('elementos no poly')
          assign(paste0('b',i),(betas_fijos[,i]+get(paste0('b',i)))/desv[i-1])
        }
        
      }
    }
  }
  
  return(
    setNames(
      mget(paste0("b", 1:ind)), 
      paste0("beta", 1:ind)
    )
  )
  
}

#MUCHO TIEMPO DE EJECUCIÓN
pred_nuevos_q0.5_def<-pred_nuevos_final_2(param_mod_q0.5_def,coords_km,grid_km,scale=T,df=df_conj_filled,vars_q0.5,df_medias=v_q0.5_no_sc)
pred_nuevos_q0.95_def<-pred_nuevos_final_2(param_mod_q0.95_def,coords_km,grid_km,scale=T,df=df_conj_filled,vars_q0.95,df_medias=v_q0.95_no_sc)

saveRDS(pred_nuevos_q0.5_def,'pred_nuevos_q0.5_def.rds')
saveRDS(pred_nuevos_q0.95_def,'pred_nuevos_q0.95_def.rds')


#------------------------MAPAS------------------------

library(viridis)
mapa_pred<-function(valores,grid,g,tipobeta,cuantil){
  mu<-colMeans(valores,na.rm=T)
  grid_coords <- cbind(st_coordinates(grid), mu = mu)
  grid_coords <- na.omit(grid_coords)
  grid <- st_sf(mu = round(as.vector(mu), 3), geometry = grid)
  ggplot(data = background) + 
    geom_sf(fill = "antiquewhite") + 
    xlab("Longitud (º)") + ylab("Latitud (º)") + ggtitle(bquote( .(g) * .(' (') * tau *.(' = ') *.(cuantil) *.(')'))) +
    theme(panel.background = element_rect(fill = "aliceblue"),
          axis.text.x=element_text(size = 6),
          axis.text.y=element_text(size = 6, angle = 90),
          axis.title=element_text(size = 10, face = "bold")) + 
    geom_tile(data = grid_coords, aes(X, Y, fill = mu)) +
    geom_tile(data = grid, ggplot2::aes(x = st_coordinates(grid)[, 1], y = st_coordinates(grid)[, 2], fill = mu)) +
    scale_fill_gradient2( low = scales::muted("blue"), mid = "white", high = scales::muted("red"),
                          space = "Lab", midpoint = mean(mu), limits = range(mu), name = "Distance (km)") +
    # scale_fill_gradientn(
    #   colours = c("#00FF00", "#FFFF00", "#FFA500", "#FF0000", "#800080"),  # verde, amarillo, naranja, rojo, morado
    #   values = scales::rescale(c(min(mu), quantile(mu, 0.25), mean(mu), quantile(mu, 0.75), max(mu))),
    #   name = "Temperatura (ºC)",  # o "Predicción"
    #   limits = range(mu)
    # ) +
    # scale_fill_gradientn(
    #   colours = c("#00FFFF","#00FF00", "#FFFF00", "#FFA500", "#FF0000", "#800080"),  # verde, amarillo, naranja, rojo, morado
  #   values = scales::rescale(c(18,25, 31, 37, 42, 48)),
  #   name = "Temperatura (ºC)",  # o "Predicción"
  #   limits = c(18,48)
  # ) +
  # scale_fill_gradientn(
  #   colours = c("#00FFFF", "#00FF00", "#FFFF00", "#FFA500", "#FF0000", "#800080"),  # cian, verde, amarillo, naranja, rojo, morado
  #   values = scales::rescale(c(0, 10, 20, 30, 40, 48)),  # Ajustando el rango de valores
  #   name = "Temp. (ºC)",
  #   limits = c(0, 48)
  # ) +
    coord_sf(xlim = st_coordinates(limits)[, 1], ylim = st_coordinates(limits)[, 2])
  
}

mapa_pred(pred_nuevos_q0.95_def[['beta1']],grid,'Predicciones',2,0.95)
mapa_pred(pred_nuevos_q0.5_def[['beta2']],grid,'Predicciones',2,0.5)
# mapa_pred(pred_nuevos_prueba_2[['beta2']],grid,'Predicciones G300',2,0.5)

#------------------------PREDICCIÓN CUANTILES ------------------------
X_grid <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Analisis Exploratorio/ModelosReg/interpolacion/X_grid.rds")
grid_elev <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Analisis Exploratorio/ModelosReg/interpolacion/grid_elev.rds")
grid_dist <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Analisis Exploratorio/ModelosReg/interpolacion/grid_dist.rds")

pred_cuantil_2<-function(X_grid,vars,pred_betas, cuantil){
  
  pred_cuantil<-matrix(NA,nrow=length(unique(Y$Date)),ncol=790)
  pred_cuantil<-as.data.frame(pred_cuantil)
  
  # betas elevacion y distancia
  if (cuantil==0.5){
    elev <- mean(param_mod_q0.5_def[,'elev'])
    dist <- mean(param_mod_q0.5_def[,'dist'])
    # elev <- mean(mod_prueba$p.params.samples[,'elev'])
    # dist <- mean(mod_prueba$p.params.samples[,'dist'])
  }
  if (cuantil==0.95){
    elev <- mean(param_mod_q0.95_def[,'elev'])
    dist <- mean(param_mod_q0.95_def[,'dist'])
  }
  
  elev_sd<-sd(stations_dist$HGHT)
  dist_sd<-sd(stations_dist$DIST)
  
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
    
    pred<-matrix(colMeans(pred_betas[['beta1']]),nrow=1)
    
    for (j in 1:length(vars)){
      pred<-pred + matrix(colMeans(pred_betas[[paste0('beta',j+1)]],na.rm=T),nrow=1) * t(df_final[,vars[j]])
    }
    
    if (cuantil==0.5){
      pred_cuantil[i,]<-pred + elev * grid_elev/elev_sd + dist*grid_dist/dist_sd
    }
    
    if (cuantil==0.95){
      pred_cuantil[i,]<-pred + elev * grid_elev/elev_sd +  dist*grid_dist/dist_sd
    }
    #pred_cuantil[i,]<-pred #añadir media beta*altura + beta*dist distincion por cuantiles
    print(paste(fecha,pred_cuantil[i,1]))
  }
  
  rownames(pred_cuantil)<-unique(Y$Date)
  
  return(pred_cuantil)
}

pred_cuantil_q0.5_ext<-pred_cuantil_2(X_grid,vars_q0.5,pred_nuevos_q0.5_ext, cuantil = 0.5)
pred_cuantil_q0.95_ext<-pred_cuantil_2(X_grid,vars_q0.95,pred_nuevos_q0.95_ext,cuantil=0.95)
pred_cuantil_prueba<-pred_cuantil_2(X_grid,vars_q0.5,pred_nuevos_prueba, cuantil = 0.5)

pred_cuantil_q0.5_def<-pred_cuantil_2(X_grid,vars_q0.5,pred_nuevos_q0.5_def, cuantil = 0.5)
pred_cuantil_q0.95_def<-pred_cuantil_2(X_grid,vars_q0.95,pred_nuevos_q0.95_def,cuantil=0.95)

setwd('C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Analisis Exploratorio/ModelosReg/interpolacion')
saveRDS(pred_cuantil_q0.5_ext,'pred_cuantil_q0.5_ext.rds')
saveRDS(pred_cuantil_q0.95_ext,'pred_cuantil_q0.95_ext.rds')
saveRDS(pred_cuantil_prueba,'pred_cuantil_prueba.rds')
saveRDS(pred_cuantil_q0.95_def,'pred_cuantil_q0.95_def.rds')
saveRDS(pred_cuantil_q0.5_def,'pred_cuantil_q0.5_def.rds')


#PREDICCION PARA CADA ITERACION DE LAS CADENAS
#COMPUTACIONALMENTE MUY EXIGENTE
pred_cuantil_total<-function(X_grid,vars,pred_betas,cuantil){
  
  if (cuantil==0.5){
    elev <- mean(param_mod_q0.5_def[,'elev'])
    dist <- mean(param_mod_q0.5_def[,'dist'])
    # elev <- mean(mod_prueba$p.params.samples[,'elev'])
    # dist <- mean(mod_prueba$p.params.samples[,'dist'])
  }
  if (cuantil==0.95){
    elev <- mean(param_mod_q0.95_def[,'elev'])
    dist <- mean(param_mod_q0.95_def[,'dist'])
  }
  
  elev_sd<-sd(stations_dist$HGHT)
  dist_sd<-sd(stations_dist$DIST)
  
  lista_preds <- list()
  
  fechas_unicas <- unique(Y$Date)
  
  for (i in seq_along(fechas_unicas)){
    fecha <- fechas_unicas[i]
    ind<-which(X_grid$Date==fecha)
    G<-X_grid[ind,-c(1,2,3)]
    # Extraer las variables cuadráticas (entre I(...) )
    cuad_vars_q0.5 <- gsub("^I\\((.*)\\^2\\)$", "\\1", vars[grepl("^I\\(.*\\^2\\)$", vars)])
    # Crear las columnas cuadradas a partir de G
    cuad_df <- as.data.frame(sapply(cuad_vars_q0.5, function(var) G[[var]]^2))
    names(cuad_df) <- paste0("I(", cuad_vars_q0.5, "^2)")
    # Extraer las variables normales
    norm_vars_q0.5 <- vars[!grepl("^I\\(.*\\^2\\)$", vars)]
    norm_df <- G[, norm_vars_q0.5, drop = FALSE]
    # Combinar en orden según vars
    df_final <- cbind(norm_df, cuad_df)[, vars]
    
    pred<-pred_betas[['beta1']]
    
    for (j in 1:length(vars)){
      pred<-pred + sweep(pred_betas[[paste0('beta',j+1)]],2,FUN='*',t(df_final[,vars[j]]))
    }
    
    
    pred<-sweep(pred,2,FUN ='+',elev * grid_elev/elev_sd)
    pred<-sweep(pred,2,FUN='+',dist*grid_dist/dist_sd)
    
    lista_preds[[i]] <- pred
    
    print(i)
  }
  
  return(lista_preds)
}

pred_cuantil_q0.5_total<-pred_cuantil_total(X_grid,vars_q0.5,pred_nuevos_q0.5_def, cuantil=0.5)
pred_cuantil_q0.95_total<-pred_cuantil_total(X_grid,vars_q0.95,pred_nuevos_q0.95_def, cuantil=0.95)


#PREDICCION TODAS SIMULACIONES POR DECADAS
#SE USA PARA INTERVALOS DE CREDIBILIDAD
pred_cuantil_total_decadas <- function(X_grid, vars, pred_betas, cuantil, año1, año2, mes) {
  
  if (cuantil == 0.5) {
    elev <- mean(param_mod_q0.5_def[,'elev'])
    dist <- mean(param_mod_q0.5_def[,'dist'])
  } else if (cuantil == 0.95) {
    elev <- mean(param_mod_q0.95_def[,'elev'])
    dist <- mean(param_mod_q0.95_def[,'dist'])
  }
  
  elev_sd <- sd(stations_dist$HGHT)
  dist_sd <- sd(stations_dist$DIST)
  
  fechas <- unique(Y$Date[year(Y$Date) >= año1 &
                            year(Y$Date) <= año2 &
                            month(Y$Date) == mes])
  
  # Prealoca una lista para almacenar predicciones
  pred_list <- vector("list", length(fechas))
  
  # Detectar variables cuadradas y normales
  cuad_vars <- gsub("^I\\((.*)\\^2\\)$", "\\1", vars[grepl("^I\\(.*\\^2\\)$", vars)])
  norm_vars <- vars[!grepl("^I\\(.*\\^2\\)$", vars)]
  
  for (i in seq_along(fechas)) {
    fecha <- fechas[i]
    ind <- which(X_grid$Date == fecha)
    G <- X_grid[ind, -c(1,2,3), drop = FALSE]
    
    # Crea data frame de variables cuadradas
    cuad_df <- as.data.frame(lapply(cuad_vars, function(var) G[[var]]^2))
    names(cuad_df) <- paste0("I(", cuad_vars, "^2)")
    
    # Combina y ordena columnas
    norm_df <- G[, norm_vars, drop = FALSE]
    df_final <- cbind(norm_df, cuad_df)[, vars, drop = FALSE]
    
    # Inicializa pred con beta1
    pred <- pred_betas[['beta1']]
    
    # Agrega el efecto de las variables
    for (j in seq_along(vars)) {
      pred <- pred + sweep(pred_betas[[paste0('beta', j + 1)]], 2, t(df_final[[vars[j]]]), FUN = '*')
    }
    
    # Agrega términos de elevación y distancia
    pred <- sweep(pred, 2, elev * grid_elev / elev_sd, FUN = '+')
    pred <- sweep(pred, 2, dist * grid_dist / dist_sd, FUN = '+')
    
    pred_list[[i]] <- pred
    if (i %% 10 == 0 || i == 1) cat("Progreso:", i, "/", length(fechas), "\n")
  }
  
  # Une todas las matrices al final
  pred_todas <- do.call(rbind, pred_list)
  return(pred_todas)
}

#MUY EXIGENTE COMPUTACIONALMENTE. EJECUTAR AQUI Y GUARDAR
library(qs)
setwd('C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Analisis Exploratorio/ModelosReg/interpolacion')
#cuantil 0.5
#junio
pred_cuantil_q0.5_jun_1dec<-pred_cuantil_total(X_grid,vars_q0.5,pred_nuevos_q0.5_def,cuantil=0.5,1960,1969,'6')
gc()
pred_cuantil_q0.5_jun_2dec<-pred_cuantil_total(X_grid,vars_q0.5,pred_nuevos_q0.5_def,cuantil=0.5,2014,2023,'6')
gc()
dif_cuantil_q0.5_jun<-pred_cuantil_q0.5_jun_2dec-pred_cuantil_q0.5_jun_1dec
gc()
qsave(dif_cuantil_q0.5_jun, "dif_cuantil_q0.5_jun.qs", preset = "high")
colMeans(dif_cuantil_q0.5_jun)
rm(pred_cuantil_q0.5_jun_1dec)
rm(pred_cuantil_q0.5_jun_2dec)
rm(dif_cuantil_q0.5_jun)
gc()

#julio
pred_cuantil_q0.5_jul_1dec<-pred_cuantil_total(X_grid,vars_q0.5,pred_nuevos_q0.5_def,cuantil=0.5,1960,1969,'7')
gc()
pred_cuantil_q0.5_jul_2dec<-pred_cuantil_total(X_grid,vars_q0.5,pred_nuevos_q0.5_def,cuantil=0.5,2014,2023,'7')
gc()
dif_cuantil_q0.5_jul<-pred_cuantil_q0.5_jul_2dec-pred_cuantil_q0.5_jul_1dec
gc()
qsave(dif_cuantil_q0.5_jul, "dif_cuantil_q0.5_jul.qs", preset = "high")
rm(pred_cuantil_q0.5_jul_1dec)
rm(pred_cuantil_q0.5_jul_2dec)
rm(dif_cuantil_q0.5_jul)
gc()

#agosto
pred_cuantil_q0.5_ag_1dec<-pred_cuantil_total(X_grid,vars_q0.5,pred_nuevos_q0.5_def,cuantil=0.5,1960,1969,'8')
gc()
pred_cuantil_q0.5_ag_2dec<-pred_cuantil_total(X_grid,vars_q0.5,pred_nuevos_q0.5_def,cuantil=0.5,2014,2023,'8')
gc()
dif_cuantil_q0.5_ag<-pred_cuantil_q0.5_ag_2dec-pred_cuantil_q0.5_ag_1dec
gc()
qsave(dif_cuantil_q0.5_ag, "dif_cuantil_q0.5_ag.qs", preset = "high")
rm(pred_cuantil_q0.5_ag_1dec)
rm(pred_cuantil_q0.5_ag_2dec)
rm(dif_cuantil_q0.5_ag)
gc()

#cuantil 0.95
#junio
pred_cuantil_q0.95_jun_1dec<-pred_cuantil_total(X_grid,vars_q0.95,pred_nuevos_q0.95_def,cuantil=0.95,1960,1969,'6')
gc()
pred_cuantil_q0.95_jun_2dec<-pred_cuantil_total(X_grid,vars_q0.95,pred_nuevos_q0.95_def,cuantil=0.95,2014,2023,'6')
gc()
dif_cuantil_q0.95_jun<-pred_cuantil_q0.95_jun_2dec-pred_cuantil_q0.95_jun_1dec
gc()
qsave(dif_cuantil_q0.95_jun, "dif_cuantil_q0.95_jun.qs", preset = "high")
colMeans(dif_cuantil_q0.95_jun)
rm(pred_cuantil_q0.95_jun_1dec)
rm(pred_cuantil_q0.95_jun_2dec)
rm(dif_cuantil_q0.95_jun)
gc()

#julio
pred_cuantil_q0.95_jul_1dec<-pred_cuantil_total(X_grid,vars_q0.95,pred_nuevos_q0.95_def,cuantil=0.95,1960,1969,'7')
gc()
pred_cuantil_q0.95_jul_2dec<-pred_cuantil_total(X_grid,vars_q0.95,pred_nuevos_q0.95_def,cuantil=0.95,2014,2023,'7')
gc()
dif_cuantil_q0.95_jul<-pred_cuantil_q0.95_jul_2dec-pred_cuantil_q0.95_jul_1dec
gc()
qsave(dif_cuantil_q0.95_jul, "dif_cuantil_q0.95_jul.qs", preset = "high")
rm(pred_cuantil_q0.95_jul_1dec)
rm(pred_cuantil_q0.95_jul_2dec)
rm(dif_cuantil_q0.95_jul)
gc()

#agosto
pred_cuantil_q0.95_ag_1dec<-pred_cuantil_total(X_grid,vars_q0.95,pred_nuevos_q0.95_def,cuantil=0.95,1960,1969,'8')
gc()
pred_cuantil_q0.95_ag_2dec<-pred_cuantil_total(X_grid,vars_q0.95,pred_nuevos_q0.95_def,cuantil=0.95,2014,2023,'8')
gc()
dif_cuantil_q0.95_ag<-pred_cuantil_q0.95_ag_2dec-pred_cuantil_q0.95_ag_1dec
gc()
qsave(dif_cuantil_q0.95_ag, "dif_cuantil_q0.95_ag.qs", preset = "high")
rm(pred_cuantil_q0.95_ag_1dec)
rm(pred_cuantil_q0.95_ag_2dec)
rm(dif_cuantil_q0.95_ag)
gc()