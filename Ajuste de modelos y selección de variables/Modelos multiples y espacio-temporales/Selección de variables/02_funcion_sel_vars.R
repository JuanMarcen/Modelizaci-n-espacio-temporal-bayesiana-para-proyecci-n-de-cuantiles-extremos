rm(list=setdiff(ls(),c('met_ajuste','R1_todos','contador','cont_coef_mod')))

#portatil
stations <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/stations.rds")
modelosq0.5_BIC_sin_int <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/modelosq0-5_BIC_sin_int.rds")
df <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/df.rds")
Y <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Y.rds")
modelosq0.95_BIC_sin_int <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/modelosq0-95_BIC_sin_int.rds")


library(quantreg)

#------------------------PASO 1:R1 TODOS------------------------
met_ajuste<-function(mod,mod_nulo){
  return(1-mod$rho/mod_nulo$rho)
}

R1_todos<-function(modelos,tau){
  R1<-rep(NA,length=dim(stations)[1])
  for (i in 1:dim(stations)[1]){
    vars<-names(modelos[i,-c(1,2)][which(modelos[i,-c(1,2)]!=0)])
    
    formula<-as.formula(paste('Y ~ ',paste(vars,collapse = '+')))
    
    ind<-which(Y$station==stations$STAID[i])
    
    mod_nulo<-rq(Y~1,data=df,tau=tau,subset=ind)
    
    mod<-rq(formula,data=df,tau=tau,subset=ind)
    
    R1[i]<-met_ajuste(mod,mod_nulo)
  }

  return(R1)
}

#------------------------PASO3/4: SELECCION VARIABLES------------------------
contador<-function(x){
  sum(x!=0)
}
cont_coef_mod<-function(df,p){
  
  vars<-list()
  
  #grupos
  for (s in unique(df[[3]])){
    
    ind <- which(df[,3]==s)
    
    coef_no_ceros<-apply(df[ind,-c(1,2,3,34)],MARGIN=2, FUN=contador)
    
    c<-as.integer(p*dim(df[ind,])[1])
    
    v<-names(which(coef_no_ceros>c))
    
    vars[[paste0('g',s)]]<-v
    
  }
  
  vars[['juntos']]<-unique(unlist(vars))
  
  return(vars[['juntos']])
}

#------------------------FUNCIÓN FINAL (TODOS PASOS JUNTOS)------------------------

error_modelo<-function(modelos, tau, k, p,metodo){
  #añadir columna de nombres de estaciones
  modelos$NAME2<-stations$NAME2
  modelos<-modelos[,c(1,32,2:31)]
  
  #paso 1
  R1<-R1_todos(modelos,tau)
  modelos$R1<-R1
  
  #paso 2
  t_modelos <- t(modelos)
  
  t_modelos<-data.frame(
    t_modelos
  )
  colnames(t_modelos)<-stations$NAME2
  
  matrix<-as.matrix(t_modelos[3:32,])
  matrix <- apply(matrix, 2, as.numeric)
  cor<-cor(matrix,method = metodo)
  
  clust<-cutree(hclust(dist(cor)), k = k)
  
  modelos$grupo<-clust
  modelos<-modelos[,c(1,2,34,3:33)]
  
  #paso 3 y 4
  vars<-cont_coef_mod(modelos,p)
  
  #paso 5 y 6
  formula_juntos<-as.formula(paste('Y ~ ',paste(vars,collapse = '+')))
  
  R1_juntos<-rep(NA,dim(modelos)[1])
  for (i in 1:dim(modelos)[1]){
    ind<-which(Y$station==stations$STAID[i])
    mod_nulo<-rq(Y~1,data=df,subset=ind,tau=tau)
    mod<-rq(formula_juntos,data=df,subset = ind,tau=tau)
    R1_juntos[i]<-met_ajuste(mod,mod_nulo)
  }
  
  modelos$R1_juntos<-R1_juntos
  
  error<-1-R1_juntos/R1
  modelos$error<-error
  
  max_error<-max(modelos$error)
  mean_error<-mean(modelos$error)
  min_error<-min(modelos$error)
  
  ciudad<-modelos$NAME2[which.max(modelos$error)]
  
  n.vars<-length(vars)
  
  
  return(list(
    k = k,
    p = p,
    metodo = metodo,
    n.vars = n.vars,
    min_error = min_error,
    mean_error = mean_error,
    max_error = max_error,
    ciudad = ciudad,
    vars = vars
  ))
}


prop<-seq(80/100,95/100,by=0.05)
k<-c(2,3)
#pearson
errores_p_q0.5 <- vector("list", length(prop)*length(k))
i<-1
for (kk in k){
  for (p in prop){
    errores_p_q0.5[[i]]<-error_modelo(modelosq0.5_BIC_sin_int,tau=0.5,k=kk,p=p,metodo='pearson')
    i<-i+1
  }
}

ciudades_p_q0.5 <- sapply(errores_p_q0.5, function(x) x[[8]])
ciudades_p_q0.5

#spearman
errores_s_q0.5 <- vector("list", length(prop)*length(k))
i<-1
for (kk in k){
  for (p in prop){
    errores_s_q0.5[[i]]<-error_modelo(modelosq0.5_BIC_sin_int,tau=0.5,k=kk,p=p,metodo='spearman')
    i<-i+1
  }
}

ciudades_s_q0.5 <- sapply(errores_s_q0.5, function(x) x[[8]])

#como data frames
#pearson
errores_df_p_q0.5 <- do.call(rbind, lapply(errores_p_q0.5, function(x) as.numeric(x[-length(x)]))) #quito vars, para mostrar resultado mejor
colnames(errores_df_p_q0.5) <- names(errores_p_q0.5[[1]])[-length(errores_p_q0.5[[1]])]  # Mantiene los nombres correctos
errores_df_p_q0.5<-as.data.frame(errores_df_p_q0.5)
errores_df_p_q0.5$metodo<-rep('pearson',length=length(errores_p_q0.5))
errores_df_p_q0.5$ciudad <- ciudades_p_q0.5

#spearman
errores_df_s_q0.5 <- do.call(rbind, lapply(errores_s_q0.5, function(x) as.numeric(x[-length(x)]))) #quito vars, para mostrar resultado mejor
colnames(errores_df_s_q0.5) <- names(errores_s_q0.5[[1]])[-length(errores_s_q0.5[[1]])]  # Mantiene los nombres correctos
errores_df_s_q0.5<-as.data.frame(errores_df_s_q0.5)
errores_df_s_q0.5$metodo<-rep('spearman',length=length(errores_s_q0.5))
errores_df_s_q0.5$ciudad <- ciudades_s_q0.5

saveRDS(errores_p_q0.5,'errores_p_q0.5.rds') #DE AQUI SE SACAN VARS_Q0.5 Y VARS_Q0.95 (ESCRITO EN TFM)
saveRDS(errores_s_q0.5,'errores_s_q0.5.rds')
saveRDS(errores_df_p_q0.5,'errores_df_p_q0.5.rds')
saveRDS(errores_df_s_q0.5,'errores_df_s_q0.5.rds')

#escritura
library(xtable)
xtable(errores_df_p_q0.5, digits = c(0, rep(3, ncol(errores_df_p_q0.5))))
xtable(errores_df_s_q0.5, digits = c(0, rep(3, ncol(errores_df_p_q0.5))))

#CUANTIL 0.95
prop<-seq(80/100,95/100,by=0.05)
k<-c(2,3)
#pearson
errores_p_q0.95 <- vector("list", length(prop)*length(k))
i<-1
for (kk in k){
  for (p in prop){
    errores_p_q0.95[[i]]<-error_modelo(modelosq0.95_BIC_sin_int,tau=0.95,k=kk,p=p,metodo='pearson')
    i<-i+1
  }
}

ciudades_p_q0.95 <- sapply(errores_p_q0.95, function(x) x[[8]])
ciudades_p_q0.95

#spearman
errores_s_q0.95 <- vector("list", length(prop)*length(k))
i<-1
for (kk in k){
  for (p in prop){
    errores_s_q0.95[[i]]<-error_modelo(modelosq0.95_BIC_sin_int,tau=0.95,k=kk,p=p,metodo='spearman')
    i<-i+1
  }
}

ciudades_s_q0.95 <- sapply(errores_s_q0.95, function(x) x[[8]])

#como data frames
#pearson
errores_df_p_q0.95 <- do.call(rbind, lapply(errores_p_q0.95, function(x) as.numeric(x[-length(x)]))) #quito vars, para mostrar resultado mejor
colnames(errores_df_p_q0.95) <- names(errores_p_q0.95[[1]])[-length(errores_p_q0.95[[1]])]  # Mantiene los nombres correctos
errores_df_p_q0.95<-as.data.frame(errores_df_p_q0.95)
errores_df_p_q0.95$metodo<-rep('pearson',length=length(errores_p_q0.95))
errores_df_p_q0.95$ciudad <- ciudades_p_q0.95

#spearman
errores_df_s_q0.95 <- do.call(rbind, lapply(errores_s_q0.95, function(x) as.numeric(x[-length(x)]))) #quito vars, para mostrar resultado mejor
colnames(errores_df_s_q0.95) <- names(errores_s_q0.95[[1]])[-length(errores_s_q0.95[[1]])]  # Mantiene los nombres correctos
errores_df_s_q0.95<-as.data.frame(errores_df_s_q0.95)
errores_df_s_q0.95$metodo<-rep('spearman',length=length(errores_s_q0.95))
errores_df_s_q0.95$ciudad <- ciudades_s_q0.95

saveRDS(errores_p_q0.95,'errores_p_q0.95.rds')
saveRDS(errores_s_q0.95,'errores_s_q0.95.rds')
saveRDS(errores_df_p_q0.95,'errores_df_p_q0.95.rds')
saveRDS(errores_df_s_q0.95,'errores_df_s_q0.95.rds')

xtable(errores_df_p_q0.95, digits = c(0, rep(3, ncol(errores_df_p_q0.5))))
xtable(errores_df_s_q0.95, digits = c(0, rep(3, ncol(errores_df_p_q0.5))))


