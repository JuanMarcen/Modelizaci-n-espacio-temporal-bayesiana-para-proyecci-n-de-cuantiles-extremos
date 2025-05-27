rm(list=ls())
setwd("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos")

#CARGAR DATOS DESDE LOS PROGRAMAS SOBRE AJUSTE DE MODELOS (DEMASIADO PESADOS PARA GITHUB)

vars_q0.5 <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/vars_q0.5.rds")
vars_q0.95 <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/vars_q0.95.rds")
coef_R <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/coef_R.rds")
coef_R_cuadrados <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/coef_R_cuadrados.rds")
R1_esp_q0.95 <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/R1_esp_q0.95.rds")
R1_esp_q0.5 <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/R1_esp_q0.5.rds")
R1_esp_c_q0.95 <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/R1_esp_c_q0.95.rds")
R1_esp_c_q0.5 <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/R1_esp_c_q0.5.rds")
stations <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/stations.rds")

#bayesianos
R1_bay_g700_q0.5 <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Analisis Exploratorio/ModelosReg/bay/R1_bay_g700_q0.5.rds")
R1_bay_g700_q0.95 <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Analisis Exploratorio/ModelosReg/bay/R1_bay_g700_q0.95.rds")
R1_bay_g300_q0.5 <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Analisis Exploratorio/ModelosReg/bay/R1_bay_g300_q0.5.rds")
R1_bay_g300_q0.95 <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Analisis Exploratorio/ModelosReg/bay/R1_bay_g300_q0.95.rds")
R1_bay_g500_q0.5 <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Analisis Exploratorio/ModelosReg/bay/R1_bay_g500_q0.5.rds")
R1_bay_g500_q0.95 <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Analisis Exploratorio/ModelosReg/bay/R1_bay_g500_q0.95.rds")

#rhos
rho_g300_q0.5 <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Analisis Exploratorio/ModelosReg/bay/rho_g300_q0.5.rds")
rho_g300_q0.95 <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Analisis Exploratorio/ModelosReg/bay/rho_g300_q0.95.rds")
rho_g500_q0.5 <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Analisis Exploratorio/ModelosReg/bay/rho_g500_q0.5.rds")
rho_g500_q0.95 <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Analisis Exploratorio/ModelosReg/bay/rho_g500_q0.95.rds")
rho_g700_q0.5 <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Analisis Exploratorio/ModelosReg/bay/rho_g700_q0.5.rds")
rho_g700_q0.95 <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Analisis Exploratorio/ModelosReg/bay/rho_g700_q0.95.rds")

vars_c<-c('g300','g500','g700')

#------------------------R1 LOCALES PARA MODELOS SIMPLES------------------------
R1_locales_simples<-function(coef,coef_cuadrados, vars, cuantil){
  df<-matrix(NA,nrow=40)
  df<-as.data.frame(df,row.names=stations$NAME2)
  vars<-sort(vars) #ordeno, primero normal y luego cuadrados
  ind<-grep("^I\\(", vars)[1] #sacar indice de la primera variable al cuadrado
  
  if(is.na(ind)){
    ind<-length(vars)+1
  }
  
  if(cuantil == 0.5){
    expr<-'R_q0.5_'
  }
  
  if(cuantil == 0.95){
    expr<-'R_q0.95_'
  }
  
  for (i in 1:(ind-1)){
    df[[vars[i]]]<-coef[[paste0(expr,vars[i])]]
  }
  
  if(ind!=length(vars)+1){
    for (i in ind:length(vars)){
      df[[vars[i]]]<-coef_cuadrados[[paste0(expr,gsub("^I\\(|\\^2\\)$", "", vars[i]))]]
    }
  }
    
  df<-df[,-1]
  
  return(df)
}

R1_locales_simples_q0.5<-R1_locales_simples(coef_R,coef_R_cuadrados,vars_q0.5,0.5)
R1_locales_simples_q0.95<-R1_locales_simples(coef_R,coef_R_cuadrados,vars_q0.95,0.95)
R1_locales_esp_q0.5<-R1_esp_q0.5$R1_locales
R1_locales_esp_q0.95<-R1_esp_q0.95$R1_locales

R1_locales_simples_c_q0.5<-R1_locales_simples(coef_R,coef_R_cuadrados,vars_c,0.5)
R1_locales_simples_c_q0.95<-R1_locales_simples(coef_R,coef_R_cuadrados,vars_c,0.95)
R1_locales_esp_c_q0.5<-R1_esp_c_q0.5$R1_locales
R1_locales_esp_c_q0.95<-R1_esp_c_q0.95$R1_locales

simple_mayor_esp<-function(simples, esp){
  v<-rep(NA,length=dim(simples)[2])
  for (i in 1:dim(simples)[2]){
    v[i]<-sum(simples[[i]]>esp[[i]])*100/dim(stations)[1]
  }
  return(v)
}

simple_mayor_esp_q0.5<-simple_mayor_esp(R1_locales_simples_q0.5,R1_locales_esp_q0.5)
simple_mayor_esp_q0.5
simple_mayor_esp_q0.95<-simple_mayor_esp(R1_locales_simples_q0.95,R1_locales_esp_q0.95)
simple_mayor_esp_q0.95
#EN TODAS VARIBLES AJUSTA MEJOR LOCALMENTE EL SIMPLE, LO CUAL TIENE SENTIDO



# ------------------------R1 GLOBAL PARA MODELOS SIMPLES------------------------
library(quantreg)
df <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/df.rds")
Y <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Y.rds")

R1_global_simple<-function(vars,tau){
  R1_g_s<-rep(NA,length(vars))
  
  for (j in 1:length(vars)){
    
    rho<-rep(NA,length=dim(stations)[1])
    rho_nulo<-rep(NA,length=dim(stations)[1])
    
    for (i in 1:length(rho)){
      
      ind<-which(Y$station==stations$STAID[[i]])
      mod<-rq(as.formula(paste("Y ~", vars[j])),data=df,subset=ind,tau=tau)
      mod_nulo<-rq(Y~1,data=df,subset=ind,tau=tau)
      rho[i]<-mod$rho
      rho_nulo[i]<-mod_nulo$rho
      
    }
    
    R1_g_s[j]<-1-sum(rho)/sum(rho_nulo)
    
  }
  
  return(R1_g_s)
}

R1_global_simple_q0.5<-R1_global_simple(vars_q0.5,0.5)
R1_global_simple_q0.95<-R1_global_simple(vars_q0.95,0.95)
R1_global_simple_q0.5
R1_global_simple_q0.95


R1_global_simple_c_q0.5<-R1_global_simple(vars_c,0.5)
R1_global_simple_c_q0.95<-R1_global_simple(vars_c,0.95)
R1_global_simple_c_q0.5
R1_global_simple_c_q0.95


#------------------------TABLA FINAL PARA COMPARACION ------------------------
orden <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/orden.rds")
#COMPARACION G300,G500,G700
#cuantil 0.50
colnames(R1_locales_simples_c_q0.5)<-paste0('s_',colnames(R1_locales_simples_c_q0.5))
colnames(R1_locales_esp_c_q0.5)<-paste0('e_',colnames(R1_locales_esp_c_q0.5))
R1_local_bay_q0.5<-cbind(R1_bay_g300_q0.5$R1_locales,R1_bay_g500_q0.5$R1_locales,R1_bay_g700_q0.5$R1_locales)
colnames(R1_local_bay_q0.5)<-c('b_g300','b_g500','b_g700')
tabla_comp_c_q0.5<-cbind(R1_locales_simples_c_q0.5,R1_locales_esp_c_q0.5,R1_local_bay_q0.5)
tabla_comp_c_q0.5<-tabla_comp_c_q0.5[orden,c(1,4,7,2,5,8,3,6,9)]

R1_global_bay_q0.5<-cbind(unlist(R1_bay_g300_q0.5$R1_globales),unlist(R1_bay_g500_q0.5$R1_globales),unlist(R1_bay_g700_q0.5$R1_globales))
globales_c_q0.5<-c(R1_global_simple_c_q0.5,unlist(R1_esp_c_q0.5$R1_globales[2]),R1_global_bay_q0.5)
globales_c_q0.5<-globales_c_q0.5[c(1,4,7,2,5,8,3,6,9)]

tabla_comp_c_q0.5<-rbind(tabla_comp_c_q0.5,globales_c_q0.5)
rownames(tabla_comp_c_q0.5)[nrow(tabla_comp_c_q0.5)]<-'R1 global'

saveRDS(tabla_comp_c_q0.5,'tabla_comp_c_q0.5.rds')

#cuantil 0.95
colnames(R1_locales_simples_c_q0.95)<-paste0('s_',colnames(R1_locales_simples_c_q0.95))
colnames(R1_locales_esp_c_q0.95)<-paste0('e_',colnames(R1_locales_esp_c_q0.95))
R1_local_bay_q0.95<-cbind(R1_bay_g300_q0.95$R1_locales,R1_bay_g500_q0.95$R1_locales,R1_bay_g700_q0.95$R1_locales)
colnames(R1_local_bay_q0.95)<-c('b_g300','b_g500','b_g700')
tabla_comp_c_q0.95<-cbind(R1_locales_simples_c_q0.95,R1_locales_esp_c_q0.95,R1_local_bay_q0.95)
tabla_comp_c_q0.95<-tabla_comp_c_q0.95[orden,c(1,4,7,2,5,8,3,6,9)]

R1_global_bay_q0.95<-cbind(unlist(R1_bay_g300_q0.95$R1_globales),unlist(R1_bay_g500_q0.95$R1_globales),unlist(R1_bay_g700_q0.95$R1_globales))
globales_c_q0.95<-c(R1_global_simple_c_q0.95,unlist(R1_esp_c_q0.95$R1_globales[2]),R1_global_bay_q0.95)
globales_c_q0.95<-globales_c_q0.95[c(1,4,7,2,5,8,3,6,9)]

tabla_comp_c_q0.95<-rbind(tabla_comp_c_q0.95,globales_c_q0.95)
rownames(tabla_comp_c_q0.95)[nrow(tabla_comp_c_q0.95)]<-'R1 global'

saveRDS(tabla_comp_c_q0.95,'tabla_comp_c_q0.95.rds')



#------------------------RHOS------------------------
#simples
setwd('C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Analisis Exploratorio/ModelosReg/bay')
rho_gloables<-cbind(rho_g300_q0.5$rho_globales,rho_g300_q0.95$rho_globales,
                    rho_g500_q0.5$rho_globales,rho_g500_q0.95$rho_globales,
                    rho_g700_q0.5$rho_globales,rho_g700_q0.95$rho_globales)

rho_locales<-cbind(rho_g300_q0.5$rho_estaciones,rho_g300_q0.95$rho_estaciones,
                   rho_g500_q0.5$rho_estaciones,rho_g500_q0.95$rho_estaciones,
                   rho_g700_q0.5$rho_estaciones,rho_g700_q0.95$rho_estaciones)


rho_locales<-rho_locales[orden,]
colnames(rho_gloables) <- colnames(rho_locales)
rho_todos<-rbind(rho_locales,rho_gloables)
rownames(rho_todos)[nrow(rho_todos)]<-'rho_final'

saveRDS(rho_locales,'rho_locales.rds')
saveRDS(rho_todos,'rho_todos.rds')

#finales
rho_q0.95_def <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Analisis Exploratorio/ModelosReg/bay/rho_q0.95_def.rds")
rho_q0.5_def <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Analisis Exploratorio/ModelosReg/bay/rho_q0.5_def.rds")
rho_gloables_f<-cbind(rho_q0.5_def$rho_globales,rho_q0.95_def$rho_globales)

rho_locales_f<-cbind(rho_q0.5_def$rho_estaciones,rho_q0.95_def$rho_estaciones)

rho_locales_f<-rho_locales_f[orden,]
colnames(rho_gloables_f) <- colnames(rho_locales_f)
rho_todos_f<-rbind(rho_locales_f,rho_gloables_f)
rownames(rho_todos_f)[nrow(rho_todos_f)]<-'rho_final'

saveRDS(rho_locales_f,'rho_locales_f.rds')
saveRDS(rho_todos_f,'rho_todos_f.rds')


#------------------------TABLA MODELOS FINALES ------------------------
R1_esp_f_q0.95 <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/R1_esp_f_q0.95.rds")
R1_esp_f_q0.5 <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/R1_esp_f_q0.5.rds")
R1_bay_q0.5_sc_def <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Analisis Exploratorio/ModelosReg/bay/R1_bay_q0.5_sc_def.rds")
R1_bay_q0.95_sc_def <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Analisis Exploratorio/ModelosReg/bay/R1_bay_q0.95_sc_def.rds")

orden <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/orden.rds")

tabla_R_final<-data.frame(
  R1_esp_q0.5=R1_esp_f_q0.5$R1_locales,
  R1_bay_q0.5=R1_bay_q0.5_sc_def$R1_locales,
  R1_esp_q0.95=R1_esp_f_q0.95$R1_locales,
  R1_bay_q0.95=R1_bay_q0.95_sc_def$R1_locales,
  row.names = stations$NAME2
)

globales<-c(R1_esp_f_q0.5$R1_globales[2],R1_bay_q0.5_sc_def$R1_globales,R1_esp_f_q0.95$R1_globales[2],R1_bay_q0.95_sc_def$R1_globales)
globales<-unlist(globales)

tabla_R_final<-tabla_R_final[orden,]

tabla_R_final<-rbind(tabla_R_final,globales)
rownames(tabla_R_final)[nrow(tabla_R_final)]<-'R1 global'

saveRDS(tabla_R_final,'tabla_R_final.rds')
