rm(list=ls())

load("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Analisis Exploratorio/Jorge/models.RData")
stations_dist <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/stations_dist.rds")

library(spTReg)
library(quantreg)
library(zoo)
library(sf)
library(sp)
library(coda)

# USAR STARTING POINTS, DF Y FORMULAS DEL SCRIPT 03_MOD_BAY...

#------------------------MODELOS------------------------
# CUANTIL 0.5
set.seed(11111)
while( !exists("mod_q0.5_sc_conv") || class(mod_q0.5_sc_conv) == "try-error" ) {
mod_q0.5_sc_conv <- try(spTm(formula_q0.5,
                      data=v_q0.5,
                      method = 'q',
                      quantile = 0.5,
                      coords = coords_km,
                      v = as.matrix(cbind(1,v_q0.5[,2:16])),
                      priors = list(
                        "beta" = list(M = rep(0, 18), P = 0.0001 * 
                                        diag(18)),
                        "sigma" = c(0.1, 0.1),
                        "phi" = c(38, 7400),
                        "mu" = c(0, 0.0001)),
                      starting = list(
                        "beta" = c(start_beta_q0.5_sc,elev_inic_q0.5,dist_inic_q0.5),
                        "sigma" = 1,
                        "alpha" = inic_procesos_q0.5,
                        "hp" = c("mu" = 0, "sigma" = 1, "phi" = 3 / 600)),
                      n.samples = 100000,
                      n.burnin = 100000,
                      n.thin = 100,
                      n.report = 1000
))
}

while( !exists("mod_q0.5_sc_conv_2") || class(mod_q0.5_sc_conv_2) == "try-error" ) {
mod_q0.5_sc_conv_2<-try(spTm(formula_q0.5,
                         data=v_q0.5,
                         method = 'q',
                         quantile = 0.5,
                         coords = coords_km,
                         v = as.matrix(cbind(1,v_q0.5[,2:16])),
                         priors = list(
                           "beta" = list(M = rep(0, 18), P = 0.0001 * 
                                           diag(18)),
                           "sigma" = c(0.1, 0.1),
                           "phi" = c(38, 7400),
                           "mu" = c(0, 0.0001)),
                         starting = list(
                           "beta" = 0.1,
                           "sigma" = 1,
                           "alpha" = 0,
                           "hp" = c("mu" = 0, "sigma" = 1, "phi" = 3 / 600)),
                         n.samples = 100000,
                         n.burnin = 100000,
                         n.thin = 100,
                         n.report = 1000
))
}

#CUANTIL 0.95
while( !exists("mod_q0.95_sc_conv") || class(mod_q0.95_sc_conv) == "try-error" ) {
mod_q0.95_sc_conv<-try(spTm(formula_q0.95,
                       data=v_q0.95,
                       method = 'q',
                       quantile = 0.95,
                       coords = coords_km,
                       v = as.matrix(cbind(1,v_q0.95[2:19])),
                       priors = list(
                         "beta" = list(M = rep(0, 21), P = 0.0001 * 
                                         diag(21)),
                         "sigma" = c(0.1, 0.1),
                         "phi" = c(38, 7400),
                         "mu" = c(0, 0.0001)),
                       starting = list(
                         "beta" = c(start_beta_q0.95_sc,elev_inic_q0.95,dist_inic_q0.95),
                         "sigma" = 1,
                         "alpha" = inic_procesos_q0.95,
                         "hp" = c("mu" = 0, "sigma" = 1, "phi" = 3 / 600)),
                       n.samples = 100000,
                       n.burnin = 100000,
                       n.thin = 100,
                       n.report = 1000
))
}

while( !exists("mod_q0.95_sc_conv_2") || class(mod_q0.95_sc_conv_2) == "try-error" ) {
mod_q0.95_sc_conv_2<-try(spTm(formula_q0.95,
                        data=v_q0.95,
                        method = 'q',
                        quantile = 0.95,
                        coords = coords_km,
                        v = as.matrix(cbind(1,v_q0.95[2:19])),
                        priors = list(
                          "beta" = list(M = rep(0, 21), P = 0.0001 * 
                                          diag(21)),
                          "sigma" = c(0.1, 0.1),
                          "phi" = c(38, 7400),
                          "mu" = c(0, 0.0001)),
                        starting = list(
                          "beta" = 0.1,
                          "sigma" = 1,
                          "alpha" = 0,
                          "hp" = c("mu" = 0, "sigma" = 1, "phi" = 3 / 600)),
                        n.samples = 100000,
                        n.burnin = 100000,
                        n.thin = 100,
                        n.report = 1000
))
}

# save(mod_q0.5_sc_conv, mod_q0.5_sc_conv_2,
#      mod_q0.95_sc_conv, mod_q0.95_sc_conv_2, 
#      file = "models.RData")

#JUNTADO DE MODELOS DEFINITIVOS. LOS QUE SE USAN EN EL SCRIPT 03_MOD_BAY...
param_mod_q0.5_def<-rbind(mod_q0.5_sc_conv$p.params.samples,mod_q0.5_sc_conv_2$p.params.samples)
param_mod_q0.95_def<-rbind(mod_q0.95_sc_conv$p.params.samples,mod_q0.95_sc_conv_2$p.params.samples)

colnames(param_mod_q0.5_def)[2:16]<-c(
  'poly(g500_45_.10, 2)1','poly(g500_45_5, 2)1', 'poly(g700, 2)1',
  'poly(g700_35_.10, 2)1','poly(g700_45_.10, 2)1','g700_45_5','I(g300^2)',
  'I(g300_45_.10^2)','I(g500^2)','poly(g500_45_.10, 2)2','poly(g500_45_5, 2)2',
  'poly(g700, 2)2','poly(g700_35_.10, 2)2','I(g700_35_5^2)','poly(g700_45_.10, 2)2'
)

colnames(param_mod_q0.95_def)[2:19]<-c(
  'poly(g300, 2)1','g300_45_.10','poly(g300_45_5, 2)1','poly(g500, 2)1',
  'g500_45_.10','g500_45_5','poly(g700, 2)1','poly(g700_35_.10, 2)1','poly(g700_35_5, 2)1',
  'poly(g700_45_.10, 2)1','g700_45_5','poly(g300, 2)2','poly(g300_45_5, 2)2',
  'poly(g500, 2)2','poly(g700, 2)2','poly(g700_35_.10, 2)2','poly(g700_35_5, 2)2',
  'poly(g700_45_.10, 2)2'
)

saveRDS(param_mod_q0.5_def,'param_mod_q0.5_def.rds')
saveRDS(param_mod_q0.95_def,'param_mod_q0.95_def.rds')
#------------------------CONVERGENCIA------------------------
#CUANTIL 0.50
#INTERCEPTOS
for (i in 1:40){
  chains <- list(
    mod_q0.5_sc_conv$p.params.samples[,"(Intercept)"] + 
      mod_q0.5_sc_conv$p.params.samples[,"elev"] * scale(stations_dist$HGHT)[i] + 
      mod_q0.5_sc_conv$p.params.samples[,"dist"] * scale(stations_dist$DIST)[i] + 
      mod_q0.5_sc_conv$p.params.samples[,paste0("beta1(s",i,")")],
    mod_q0.5_sc_conv_2$p.params.samples[,"(Intercept)"] + 
      mod_q0.5_sc_conv_2$p.params.samples[,"elev"] * scale(stations_dist$HGHT)[i] + 
      mod_q0.5_sc_conv_2$p.params.samples[,"dist"] * scale(stations_dist$DIST)[i] + 
      mod_q0.5_sc_conv_2$p.params.samples[,paste0("beta1(s",i,")")]
  )
  conv<-gelman.diag(chains,multivariate = F)
  cat(paste0("beta1(s",i,")"),": ", round(conv$psrf[1],3), "\n")
}

# COEFICIENTES
for (j in 2:16){
  for (i in 1:40){
    chains <- list(
      mod_q0.5_sc_conv$p.params.samples[,j] + 
        mod_q0.5_sc_conv$p.params.samples[,paste0("beta",j,"(s",i,")")],
      mod_q0.5_sc_conv_2$p.params.samples[,j] + 
        mod_q0.5_sc_conv_2$p.params.samples[,paste0("beta",j,"(s",i,")")]
    )
    conv<-gelman.diag(chains,multivariate = F)
    cat(paste0("beta",j,"(s",i,")"),": ", round(conv$psrf[1],3), "\n")
  }
}
#todo ok

#CUANTIL 0.95
#INTERCEPTOS
for (i in 1:40){
  chains <- list(
    mod_q0.95_sc_conv$p.params.samples[,"(Intercept)"] + 
      mod_q0.95_sc_conv$p.params.samples[,"elev"] * scale(stations_dist$HGHT)[i] + 
      mod_q0.95_sc_conv$p.params.samples[,"dist"] * scale(stations_dist$DIST)[i] + 
      mod_q0.95_sc_conv$p.params.samples[,paste0("beta1(s",i,")")],
    mod_q0.95_sc_conv_2$p.params.samples[,"(Intercept)"] + 
      mod_q0.95_sc_conv_2$p.params.samples[,"elev"] * scale(stations_dist$HGHT)[i] + 
      mod_q0.95_sc_conv_2$p.params.samples[,"dist"] * scale(stations_dist$DIST)[i] + 
      mod_q0.95_sc_conv_2$p.params.samples[,paste0("beta1(s",i,")")]
  )
  conv<-gelman.diag(chains,multivariate = F)
  cat(paste0("beta1(s",i,")"),": ", round(conv$psrf[1],3), "\n")
}

# COEFICIENTES
for (j in 2:19){
  for (i in 1:40){
    chains <- list(
      mod_q0.95_sc_conv$p.params.samples[,j] + 
        mod_q0.95_sc_conv$p.params.samples[,paste0("beta",j,"(s",i,")")],
      mod_q0.95_sc_conv_2$p.params.samples[,j] + 
        mod_q0.95_sc_conv_2$p.params.samples[,paste0("beta",j,"(s",i,")")]
    )
    conv<-gelman.diag(chains,multivariate = F)
    cat(paste0("beta",j,"(s",i,")"),": ", round(conv$psrf[1],3), "\n")
  }
}
#todo ok


#------------------------ESS------------------------
param_mod_q0.5_def<-as.mcmc(param_mod_q0.5_def)
param_mod_q0.95_def<-as.mcmc(param_mod_q0.95_def)

#CUANTIL 0.50
#INTERCEPTOS
for (i in 1:40){
  chains <- list(
    param_mod_q0.5_def[,"(Intercept)"] + 
      param_mod_q0.5_def[,"elev"] * scale(stations_dist$HGHT)[i] + 
      param_mod_q0.5_def[,"dist"] * scale(stations_dist$DIST)[i] + 
      param_mod_q0.5_def[,paste0("beta1(s",i,")")]
  )
  ess<-effectiveSize(chains)
  cat(paste0("beta1(s",i,")"),": ", round(ess,3), "\n")
}

# COEFICIENTES
ess_df<-as.data.frame(matrix(NA,ncol=2,nrow=15*40))
cont<-1
for (j in 2:16){
  for (i in 1:40){
    chains <- list(
      param_mod_q0.5_def[,j] + 
        param_mod_q0.5_def[,paste0("beta",j,"(s",i,")")]
    )
    ess<-effectiveSize(chains)
    ess_df[cont,1]<-paste0("beta",j,"(s",i,")")
    ess_df[cont,2]<-round(ess,3)
    cont<-cont+1
  }
}
ess_df
dim(ess_df[which(ess_df[,2]<=200),])[1]
ess_df[which.min(ess_df[,2]),]

#CUANTIL 0.95
#INTERCEPTOS
for (i in 1:40){
  chains <- list(
    param_mod_q0.95_def[,"(Intercept)"] + 
      param_mod_q0.95_def[,"elev"] * scale(stations_dist$HGHT)[i] + 
      param_mod_q0.95_def[,"dist"] * scale(stations_dist$DIST)[i] + 
      param_mod_q0.95_def[,paste0("beta1(s",i,")")]
  )
  ess<-effectiveSize(chains)
  cat(paste0("beta1(s",i,")"),": ", round(ess,3), "\n")
}

# COEFICIENTES
ess_df<-as.data.frame(matrix(NA,ncol=2,nrow=18*40))
cont<-1
for (j in 2:19){
  for (i in 1:40){
    chains <- list(
      param_mod_q0.95_def[,j] + 
        param_mod_q0.95_def[,paste0("beta",j,"(s",i,")")]
    )
    ess<-effectiveSize(chains)
    ess_df[cont,1]<-paste0("beta",j,"(s",i,")")
    ess_df[cont,2]<-round(ess,3)
    cont<-cont+1
  }
}
ess_df
dim(ess_df[which(ess_df[,2]<=200),])[1]
ess_df[which.min(ess_df[,2]),]

#------------------------TRACEPLOTS------------------------

#CUANTIL 0.5O
#INTERCEPTO
par(mfrow=c(5,8))
for (i in 1:40){
  plot(c(mod_q0.5_sc_conv$p.params.samples[,"(Intercept)"] + 
           mod_q0.5_sc_conv$p.params.samples[,"elev"] * scale(stations_dist$HGHT)[i] +
           mod_q0.5_sc_conv$p.params.samples[,"dist"] * scale(stations_dist$DIST)[i] +
           mod_q0.5_sc_conv$p.params.samples[,paste0('beta1(s',i,')')]), type = "l",
       xlab='Iteración',ylab=paste0('beta1 + beta1(s',i,')'))
  lines(c(mod_q0.5_sc_conv_2$p.params.samples[,"(Intercept)"] + 
            mod_q0.5_sc_conv_2$p.params.samples[,"elev"] * scale(stations_dist$HGHT)[i] +
            mod_q0.5_sc_conv_2$p.params.samples[,"dist"] * scale(stations_dist$DIST)[i] +
            mod_q0.5_sc_conv_2$p.params.samples[,paste0('beta1(s',i,')')]), col = "gray")
  
}
dev.off()


#COEFICIENTES
for (j in 2:16){
  filename<-paste0('beta',j,'_q0.5.png')
  png(filename,height = 2000,width = 4000,res=150)
  par(mfrow=c(5,8))
  for (i in 1:40){
    plot(c(mod_q0.5_sc_conv$p.params.samples[,j] + 
             mod_q0.5_sc_conv$p.params.samples[,paste0('beta',j,'(s',i,')')]), type = "l",
         xlab='Iteración',ylab=paste0('beta',j,' + beta',j,'(s',i,')'))
    lines(c(mod_q0.5_sc_conv_2$p.params.samples[,j] + 
              mod_q0.5_sc_conv_2$p.params.samples[,paste0('beta',j,'(s',i,')')]), col = "gray")
    
  }
  dev.off()
}


#CUANTIL 0.95
#INTERCEPTO
par(mfrow=c(5,8))
for (i in 1:40){
  plot(c(mod_q0.95_sc_conv$p.params.samples[,"(Intercept)"] + 
           mod_q0.95_sc_conv$p.params.samples[,"elev"] * scale(stations_dist$HGHT)[i] +
           mod_q0.95_sc_conv$p.params.samples[,"dist"] * scale(stations_dist$DIST)[i] +
           mod_q0.95_sc_conv$p.params.samples[,paste0('beta1(s',i,')')]), type = "l",
       xlab='Iteración',ylab=paste0('beta1 + beta1(s',i,')'))
  lines(c(mod_q0.95_sc_conv_2$p.params.samples[,"(Intercept)"] + 
            mod_q0.95_sc_conv_2$p.params.samples[,"elev"] * scale(stations_dist$HGHT)[i] +
            mod_q0.95_sc_conv_2$p.params.samples[,"dist"] * scale(stations_dist$DIST)[i] +
            mod_q0.95_sc_conv_2$p.params.samples[,paste0('beta1(s',i,')')]), col = "gray")
  
}
dev.off()


#COEFICIENTES
for (j in 2:19){
  filename<-paste0('beta',j,'_q0.95.png')
  png(filename,height = 2000,width = 4000,res=150)
  par(mfrow=c(5,8))
  for (i in 1:40){
    plot(c(mod_q0.95_sc_conv$p.params.samples[,j] + 
             mod_q0.95_sc_conv$p.params.samples[,paste0('beta',j,'(s',i,')')]), type = "l",
         xlab='Iteración',ylab=paste0('beta',j,' + beta',j,'(s',i,')'))
    lines(c(mod_q0.95_sc_conv_2$p.params.samples[,j] + 
              mod_q0.95_sc_conv_2$p.params.samples[,paste0('beta',j,'(s',i,')')]), col = "gray")
    
  }
  dev.off()
}


#------------------------INCERTIDUMBRE------------------------
for (i in 1:40){
  q<-quantile(
    param_mod_q0.5_def[,"(Intercept)"] + 
      param_mod_q0.5_def[,"elev"] * scale(stations_dist$HGHT)[i] + 
      param_mod_q0.5_def[,"dist"] * scale(stations_dist$DIST)[i] + 
      param_mod_q0.5_def[,paste0("beta1(s",i,")")]
    ,probs=c(0.025,0.975))
  cat(paste0("beta1(s",i,")"),": ", round(q,3), "\n")
}

# COEFICIENTES
ic_df<-as.data.frame(matrix(NA,ncol=3,nrow=15*40))
colnames(ic_df)<-c('beta','2.5%','97.5%')
cont<-1
for (j in 2:16){
  for (i in 1:40){
    q <- quantile(
      param_mod_q0.5_def[,j] + 
        param_mod_q0.5_def[,paste0("beta",j,"(s",i,")")]
      ,probs=c(0.025,0.975))
    ic_df[cont,1]<-paste0("beta",j,"(s",i,")")
    ic_df[cont,2]<-round(q[1],3)
    ic_df[cont,3]<-round(q[2],3)
    cont<-cont+1
  }
}
#INTERCEPTOS
for (i in 1:40){
  q<-quantile(
    param_mod_q0.95_def[,"(Intercept)"] + 
      param_mod_q0.95_def[,"elev"] * scale(stations_dist$HGHT)[i] + 
      param_mod_q0.95_def[,"dist"] * scale(stations_dist$DIST)[i] + 
      param_mod_q0.95_def[,paste0("beta1(s",i,")")]
  ,probs=c(0.025,0.975))
  cat(paste0("beta1(s",i,")"),": ", round(q,3), "\n")
}

# COEFICIENTES
ic_df<-as.data.frame(matrix(NA,ncol=3,nrow=18*40))
colnames(ic_df)<-c('beta','2.5%','97.5%')
cont<-1
for (j in 2:19){
  for (i in 1:40){
    q <- quantile(
      param_mod_q0.95_def[,j] + 
        param_mod_q0.95_def[,paste0("beta",j,"(s",i,")")]
    ,probs=c(0.025,0.975))
    ic_df[cont,1]<-paste0("beta",j,"(s",i,")")
    ic_df[cont,2]<-round(q[1],3)
    ic_df[cont,3]<-round(q[2],3)
    cont<-cont+1
  }
}
