rm(list = setdiff(ls(), c( "stations",'X','Y')))

Y <- readRDS("C:/Users/jumar/Desktop/TFM/Datos/Y.rds")
X <- readRDS("C:/Users/jumar/Desktop/TFM/Datos/X.rds")
stations<-readRDS("C:/Users/jumar/Desktop/TFM/Datos/stations.rds")

library(quantreg)

estaciones <- stations$STAID

vars <- colnames(X)[-c(1,2,3)]  # Excluir las primeras columnas
formula <- as.formula(paste("Y ~ (", paste(vars, collapse = " + "), ")^2 +", 
                            paste(paste0("I(", vars, "^2)"), collapse = " + ")))
formula

df <- data.frame(Y=Y[,-c(1,2)]/10, X[,-c(1,2,3)])


modelos_tau<-function(tau,formula=formula,pen=2){
  estaciones<-stations$STAID
  ind<-which(Y$station==estaciones[1])
  mod <- rq(formula,data=df[ind,])
  coef<-coef(mod)[-1] #intercepto nos da igual SOLO LO QUEREMOS PARA LOS NOMBRES
  #data frame en el que guardamos los valores de los coeficientes escogidos
  modelos<-matrix(rep(0,length(estaciones)*length(coef)),nrow=length(estaciones))
  modelos<-as.data.frame(modelos,row.names = stations$STAID)
  colnames(modelos)<-names(coef)
  
  
  for (s in 1:length(estaciones)){
    ind<- which(Y$station==estaciones[s])#subset
    
    model_null<-rq(Y~1, data=df[ind,],tau=tau)#modelo nulo
    
    #añadir penalizacion
    mod_step<-step(model_null, scope = formula, direction = 'forward',k=pen)# step de modelo nulo hasta nuestra formula
    coef_sel<-mod_step$coefficients[-1] #intercepto no interesa
    
    for (i in 1:length(coef_sel)){
      name_sel <- names(coef_sel)[i]
      for (j in 1:length(coef)){
        name <- names(coef)[j]
        if (name_sel == name){
          modelos[s,j]=coef_sel[i]
        }
      }
    }
    
  }
  
  return(modelos)
}

# BIC SIN INTERACCIONES
n<-5888
formula_sin_int<-as.formula(paste("Y ~ ", paste(vars, collapse = " + "), "+", 
                                  paste(paste0("I(", vars, "^2)"), collapse = " + ")))

formula_sin_int

modelosq0.5_BIC_sin_int<-modelos_tau(0.5,formula = formula_sin_int,pen=log(n))
modelosq0.5_BIC_sin_int$stations<-estaciones
modelosq0.5_BIC_sin_int<-modelosq0.5_BIC_sin_int[,c(31,1:30)]
saveRDS(modelosq0.5_BIC_sin_int,'modelosq0-5_BIC_sin_int.rds')

modelosq0.95_BIC_sin_int<-modelos_tau(0.95,formula = formula_sin_int,pen=log(n))
modelosq0.95_BIC_sin_int$stations<-estaciones
modelosq0.95_BIC_sin_int<-modelosq0.95_BIC_sin_int[,c(31,1:30)]
saveRDS(modelosq0.95_BIC_sin_int,'modelosq0-95_BIC_sin_int.rds')

