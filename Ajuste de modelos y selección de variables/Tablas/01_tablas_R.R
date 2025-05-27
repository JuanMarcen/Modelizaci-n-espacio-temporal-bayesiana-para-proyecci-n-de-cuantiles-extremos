setwd("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos")
df <- readRDS("D:/TFM/Datos/df.rds")
coef_R_sc <- readRDS("D:/TFM/Datos/coef_R_sc.rds")
stations <- readRDS("D:/TFM/Datos/stations.rds")
Y <- readRDS("D:/TFM/Datos/Y.rds")

orden <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/orden.rds")


#------------------------MODELOS SIMPLES------------------------
tabla_R_cercanos<-data.frame(
  R_q0.5_g300=round(coef_R_sc$R_q0.5_g300,3),
  R_q0.95_g300=round(coef_R_sc$R_q0.95_g300,3),
  R_q0.5_g500=round(coef_R_sc$R_q0.5_g500,3),
  R_q0.95_g500=round(coef_R_sc$R_q0.95_g500,3),
  R_q0.5_g700=round(coef_R_sc$R_q0.5_g700,3),
  R_q0.95_g700=round(coef_R_sc$R_q0.95_g700,3),
  row.names = stations$NAME2
)

tabla_R_cercanos<-tabla_R_cercanos[orden,]

media_cant<-round(apply(tabla_R_cercanos[1:6,],FUN = mean,MARGIN = 2),3)
media_cant
media_med<-round(apply(tabla_R_cercanos[7:18,],FUN = mean,MARGIN = 2),3)
media_med
media_centro<-round(apply(tabla_R_cercanos[19:40,],FUN = mean,MARGIN = 2),3)
media_centro
media_todo<-round(apply(tabla_R_cercanos,FUN = mean,MARGIN = 2),3)
media_todo
saveRDS(tabla_R_cercanos,'tabla_R_cercanos.rds')


tabla_R_45_.10<-data.frame(
  R_q0.5_g300_45_.10=round(coef_R_sc$R_q0.5_g300_45_.10,3),
  R_q0.95_g300_45_.10=round(coef_R_sc$R_q0.95_g300_45_.10,3),
  R_q0.5_g300_45_.10=round(coef_R_sc$R_q0.5_g500_45_.10,3),
  R_q0.95_g500_45_.10=round(coef_R_sc$R_q0.95_g500_45_.10,3),
  R_q0.5_g700_45_.10=round(coef_R_sc$R_q0.5_g700_45_.10,3),
  R_q0.95_g700_45_.10=round(coef_R_sc$R_q0.95_g700_45_.10,3),
  row.names = stations$NAME2
)

tabla_R_45_.10<-tabla_R_45_.10[orden,]

tabla_R_45_5<-data.frame(
  R_q0.5_g300_45_5=round(coef_R_sc$R_q0.5_g300_45_5,3),
  R_q0.95_g300_45_5=round(coef_R_sc$R_q0.95_g300_45_5,3),
  R_q0.5_g300_45_5=round(coef_R_sc$R_q0.5_g500_45_5,3),
  R_q0.95_g500_45_5=round(coef_R_sc$R_q0.95_g500_45_5,3),
  R_q0.5_g700_45_5=round(coef_R_sc$R_q0.5_g700_45_5,3),
  R_q0.95_g700_45_5=round(coef_R_sc$R_q0.95_g700_45_5,3),
  row.names = stations$NAME2
)

tabla_R_45_5<-tabla_R_45_5[orden,]

tabla_R_35_5<-data.frame(
  R_q0.5_g300_35_5=round(coef_R_sc$R_q0.5_g300_35_5,3),
  R_q0.95_g300_35_5=round(coef_R_sc$R_q0.95_g300_35_5,3),
  R_q0.5_g300_35_5=round(coef_R_sc$R_q0.5_g500_35_5,3),
  R_q0.95_g500_35_5=round(coef_R_sc$R_q0.95_g500_35_5,3),
  R_q0.5_g700_35_5=round(coef_R_sc$R_q0.5_g700_35_5,3),
  R_q0.95_g700_35_5=round(coef_R_sc$R_q0.95_g700_35_5,3),
  row.names = stations$NAME2
)

tabla_R_35_5<-tabla_R_35_5[orden,]

tabla_R_35_.10<-data.frame(
  R_q0.5_g300_35_.10=round(coef_R_sc$R_q0.5_g300_35_.10,3),
  R_q0.95_g300_35_.10=round(coef_R_sc$R_q0.95_g300_35_.10,3),
  R_q0.5_g300_35_.10=round(coef_R_sc$R_q0.5_g500_35_.10,3),
  R_q0.95_g500_35_.10=round(coef_R_sc$R_q0.95_g500_35_.10,3),
  R_q0.5_g700_35_.10=round(coef_R_sc$R_q0.5_g700_35_.10,3),
  R_q0.95_g700_35_.10=round(coef_R_sc$R_q0.95_g700_35_.10,3),
  row.names = stations$NAME2
)

tabla_R_35_.10<-tabla_R_35_.10[orden,]

#------------------------R1 modelos finales junto a R1 BIC ------------------------
setwd('C:/Users/jumar/OneDrive/Escritorio/TFM/Datos')
modelosq0.95_BIC_sin_int_R1 <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/modelosq0.95_BIC_sin_int_R1.rds")
modelosq0.5_BIC_sin_int_R1 <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/modelosq0.5_BIC_sin_int_R1.rds")
modelos_finales_q0.95_sc <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/modelos_finales_q0.95_sc.rds")
modelos_finales_q0.5_sc <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/modelos_finales_q0.5_sc.rds")

tabla_R_BIC_finales<-data.frame(
  R1_BIC_q0.5<-round(modelosq0.5_BIC_sin_int_R1$R1,3),
  R1_final_q0.5<-round(modelos_finales_q0.5_sc$R1,3),
  R1_BIC_q0.95<-round(modelosq0.95_BIC_sin_int_R1$R1,3),
  R1_final_q0.95<-round(modelos_finales_q0.95_sc$R1,3),
  row.names = stations$NAME2
)

tabla_R_BIC_finales<-tabla_R_BIC_finales[orden,]
saveRDS(tabla_R_BIC_finales,'tabla_R_BIC_finales.rds')
