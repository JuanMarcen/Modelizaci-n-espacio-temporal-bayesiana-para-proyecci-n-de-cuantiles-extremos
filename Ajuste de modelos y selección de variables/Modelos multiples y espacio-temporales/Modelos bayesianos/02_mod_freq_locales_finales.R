# MODELOS ESCOGIDOS EN NEGRITA EN EL DOCUMENTO

rm(list=ls())
setwd('C:/Users/jumar/OneDrive/Escritorio/TFM/Datos')
stations <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/stations.rds")
stations_dist <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/stations_dist.rds")
df_sc <-  readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/df_sc.rds")
Y <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Y.rds")

df_conj_filled_sc <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/df_conj_filled_sc.rds")

library(quantreg)
met_ajuste<-function(mod,mod_nulo){
  return(1-mod$rho/mod_nulo$rho)
}

# ELECCION DE VARIABLES. MIRAR ENTRE LOS DATOS SUBIDOS
# vars_q0.5<-errores_s_q0.5[[7]]$vars
# vars_q0.5<-sort(vars_q0.5)
# vars_q0.5
# setwd("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos")
# saveRDS(vars_q0.5,'vars_q0.5.rds')
# vars_q0.95<-sort(errores_p_q0.95[[7]]$vars)
# vars_q0.95
# saveRDS(vars_q0.95,'vars_q0.95.rds')

# vars_q0.5 <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/vars_q0.5.rds")
# vars_q0.95 <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/vars_q0.95.rds")

#creacion de formulas. Si aparece al cuadrado y normal como polinomio ortogonal

generar_formula_poly <- function(vars) {
  # Extraer nombres de las variables cuadráticas
  quad_vars <- gsub("^I\\((.*)\\^2\\)$", "\\1", grep("^I\\(.*\\^2\\)$", vars, value = TRUE))
  
  # Extraer nombres de las variables lineales
  lin_vars <- setdiff(vars, grep("^I\\(.*\\^2\\)$", vars, value = TRUE))
  
  # Lista de variables finales para la fórmula
  vars_finales <- c()
  
  for (var in lin_vars) {
    if (var %in% quad_vars) {
      # Si hay tanto lineal como cuadrático → usar poly
      vars_finales <- c(vars_finales, paste0("poly(", var, ", 2)"))
    } else {
      vars_finales <- c(vars_finales, var)
    }
  }
  
  # Añadir las cuadráticas que no tienen la parte lineal
  quad_solo <- setdiff(quad_vars, lin_vars)
  for (var in quad_solo) {
    vars_finales <- c(vars_finales, paste0("I(", var, "^2)"))
  }
  
  # Crear fórmula
  formula<-as.formula(paste('Y', "~", paste(vars_finales, collapse = " + ")))
  return(formula)
}

formula_q0.5<-generar_formula_poly(vars_q0.5)
formula_q0.95<-generar_formula_poly(vars_q0.95)
saveRDS(formula_q0.5,'formula_q0.5.rds')
saveRDS(formula_q0.95,'formula_q0.95.rds')
formula_q0.5<-as.formula('Y ~ poly(g500_45_.10, 2) + poly(g500_45_5, 2) + poly(g700, 2) + 
    poly(g700_35_.10, 2) + poly(g700_45_.10, 2) + g700_45_5 + 
    `I(g300^2)` + `I(g300_45_.10^2)` + `I(g500^2)` + `I(g700_35_5^2)`')

#CONJUNTOS DE DATOS BUENOS. PARA SU POSTERIOR GUARDADO
v_q0.5 <- data.frame(
  Y=df_conj_filled_sc$Y,
  
  poly_g500_45_.10_1 = scale(poly(df_conj_filled_sc$g500_45_.10, 2)[, 1]),
  poly_g500_45_5_1 = scale(poly(df_conj_filled_sc$g500_45_5, 2)[, 1]),
  
  poly_g700_1 = scale(poly(df_conj_filled_sc$g700,2)[,1]),
  
  poly_g700_35_.10_1 = scale(poly(df_conj_filled_sc$g700_35_.10, 2)[, 1]),
  poly_g700_45_.10_1 = scale(poly(df_conj_filled_sc$g700_45_.10, 2)[, 1]),
  
  g700_45_5=df_conj_filled_sc$g700_45_5,
  
  `I(g300^2)`= df_conj_filled_sc$`I(g300^2)`,     #(df_conj_filled_sc$g300)^2,
  `I(g300_45_.10^2)`=df_conj_filled_sc$`I(g300_45_.10^2)`, #(df_conj_filled_sc$g300_45_.10)^2,
  `I(g500^2)`=df_conj_filled_sc$`I(g500^2)`,        #(df_conj_filled_sc$g500)^2,
  
  poly_g500_45_.10_2 = scale(poly(df_conj_filled_sc$g500_45_.10, 2)[, 2]),
  poly_g500_45_5_2 = scale(poly(df_conj_filled_sc$g500_45_5, 2)[, 2]),
  poly_g700_2 = scale(poly(df_conj_filled_sc$g700,2)[,2]),
  poly_g700_35_.10_2 = scale(poly(df_conj_filled_sc$g700_35_.10, 2)[, 2]),
  `I(g700_35_5^2)`=  df_conj_filled_sc$`I(g700_35_5^2)`, #(df_conj_filled_sc$g700_35_5)^2,
  poly_g700_45_.10_2 = scale(poly(df_conj_filled_sc$g700_45_.10, 2)[, 2]),
  
  elev = df_conj_filled_sc$elev,
  dist = df_conj_filled_sc$dist
  
)

colnames(v_q0.5)[2:16]<-vars_q0.5
#v_q0.5 tiene todo lo necesario, poly y todo escalado y elevacion y distancia escalado.
# entonces en la formula no hay que poner polys
setwd('C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Analisis Exploratorio/ModelosReg/bay')
saveRDS(v_q0.5,'v_q0.5.rds')

v_q0.95 <- data.frame(
  Y=df_conj_filled_sc$Y,
  # Primeros componentes de polinomios
  poly_g300_1 = scale(poly(df_sc$g300, 2)[, 1]),
  g300_45_.10 = df_sc$g300_45_.10,
  poly_g300_45_5_1 = scale(poly(df_sc$g300_45_5, 2)[, 1]),
  
  poly_g500_1 = scale(poly(df_sc$g500, 2)[, 1]),
  g500_45_.10 = df_sc$g500_45_.10,
  g500_45_5 = df_sc$g500_45_5,
  
  poly_g700_1 = scale(poly(df_sc$g700, 2)[, 1]),
  poly_g700_35_.10_1 = scale(poly(df_sc$g700_35_.10, 2)[, 1]),
  poly_g700_35_5_1 = scale(poly(df_sc$g700_35_5, 2)[, 1]),
  poly_g700_45_.10_1 = scale(poly(df_sc$g700_45_.10, 2)[, 1]),
  g700_45_5 = df_sc$g700_45_5,
  
  poly_g300_2 = scale(poly(df_sc$g300, 2)[, 2]),
  poly_g300_45_5_2 = scale(poly(df_sc$g300_45_5, 2)[, 2]),
  poly_g500_2 = scale(poly(df_sc$g500, 2)[, 2]),
  poly_g700_2 = scale(poly(df_sc$g700, 2)[, 2]),
  poly_g700_35_.10_2 = scale(poly(df_sc$g700_35_.10, 2)[, 2]),
  poly_g700_35_5_2 = scale(poly(df_sc$g700_35_5, 2)[, 2]),
  poly_g700_45_.10_2 = scale(poly(df_sc$g700_45_.10, 2)[, 2]),
  
  elev = df_conj_filled_sc$elev,
  dist = df_conj_filled_sc$dist
)

colnames(v_q0.95)[2:19]<-vars_q0.95
saveRDS(v_q0.95,'v_q0.95.rds')

#NUEVAS FORMULAS PARA QUE CONCUERDE CON NOMBRES DE COLUMNAS ED V_Q0.5 Y V_Q0.95
setwd('C:/Users/jumar/OneDrive/Escritorio/TFM/Datos')
formula_q0.5_nueva<-as.formula(paste('Y ~', paste(vars_q0.5,collapse = '+')))
formula_q0.5_nueva<-as.formula('Y ~ g500_45_.10 + g500_45_5 + g700 + g700_35_.10 + g700_45_.10 + 
    g700_45_5 + `I(g300^2)` + `I(g300_45_.10^2)` + `I(g500^2)` + `I(g500_45_.10^2)` + 
    `I(g500_45_5^2)` + `I(g700^2)` + `I(g700_35_.10^2)` + `I(g700_35_5^2)` + 
    `I(g700_45_.10^2)`')
formula_q0.95_nueva<-as.formula(paste('Y ~', paste(vars_q0.95,collapse = '+')))
formula_q0.95_nueva<-as.formula('Y ~ g300 + g300_45_.10 + g300_45_5 + g500 + g500_45_.10 + g500_45_5 + 
    g700 + g700_35_.10 + g700_35_5 + g700_45_.10 + g700_45_5 + 
    `I(g300^2)` + `I(g300_45_5^2)` + `I(g500^2)` + `I(g700^2)` + `I(g700_35_.10^2)` + 
    `I(g700_35_5^2)` + `I(g700_45_.10^2)`')
saveRDS(formula_q0.5_nueva,'formula_q0.5_nueva.rds')
saveRDS(formula_q0.95_nueva,'formula_q0.95_nueva.rds')

#para convertir los nombres de poly y su correcto guardado (no necesario pero no cambia nada)
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


modelos_finales<-function(vars,formula,tau,data){
  
  modelos<-matrix(NA,nrow=dim(stations)[1],ncol = length(vars)+4)
  modelos<-as.data.frame(modelos)
  colnames(modelos)<-c('stations','NAME2','intercept',vars,'R1')
  modelos$stations<-stations$STAID
  modelos$NAME2<-stations$NAME2
  
  environment(formula) <- environment() #para correcta evaluacion de poly
  
  for (i in 1:dim(stations)[1]){
    ind<-which(Y$station==stations$STAID[i])
    mod<-rq(formula, data=data, subset=ind, tau=tau)
    mod_nulo<-rq(Y~1,data=data, subset=ind, tau = tau)
    
    #transformacion de nombres y ordenacion según el orden de vars original
    coef_modelo<-mod$coefficients[-1]
    coef_tr<-traducir_nombres_coef(names(coef_modelo))
    coef_tr<-gsub('`','',coef_tr)
    coef_df<-setNames(as.numeric(coef_modelo),coef_tr)
    coef_ord<-coef_df[vars]
    
    modelos[i,3:(2+length(vars)+1)]<-c(mod$coefficients[1],coef_ord)
    
    R1<-met_ajuste(mod,mod_nulo)
    
    modelos[i,'R1']<-R1
  }
  
  return(modelos)
}


modelos_finales_q0.5<-modelos_finales(vars_q0.5,formula_q0.5,0.5)
modelos_finales_q0.95<-modelos_finales(vars_q0.95,formula_q0.95,0.95)
modelos_finales_q0.5_sc<-modelos_finales(vars_q0.5,formula_q0.5_nueva,0.5,data=v_q0.5)
modelos_finales_q0.95_sc<-modelos_finales(vars_q0.95,formula_q0.95_nueva,0.95,data = v_q0.95)


saveRDS(modelos_finales_q0.5,'modelos_finales_q0.5.rds')
saveRDS(modelos_finales_q0.95,'modelos_finales_q0.95.rds')
saveRDS(modelos_finales_q0.5_sc,'modelos_finales_q0.5_sc.rds')
saveRDS(modelos_finales_q0.95_sc,'modelos_finales_q0.95_sc.rds')


