
rm(list=ls())
par(mfrow=c(1,1))

library(RcmdrMisc)

stations <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/stations.rds")
stations_dist <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/stations_dist.rds")
coef_R_sc <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/coef_R_sc.rds")

dist<-stations_dist$DIST #distancias costa
ldist<-log1p(dist)
elev<-stations_dist$HGHT
lelev<-log1p(elev)
par(mfrow=c(1,1))



pend_costa<-function(dist,coef,col0.5,col0.95, nombre){
  orden <- order(dist)
  
    plot(dist,coef[[col0.5]],col='blue',pch=19,xlab='Distancia costa (km)', ylab='Coeficiente',
         main=paste( nombre,'~ dist. costa '))
    points(dist,coef[[col0.95]],col='red',pch=19)
  
  legend("bottomright", title = "Cuantil", legend = c("0.5", "0.95"),
         col = c("blue", "red"), lwd = 2)
  
  mod1<-lm(coef[[col0.5]]~I(log(dist)))
  lines(dist[orden], predict(mod1)[orden], col = "blue", lwd = 2)
  
  mod2<-lm(coef[[col0.95]]~I(log(dist)))
  lines(dist[orden], predict(mod2)[orden], col = "red", lwd = 2)
  
}

pend_costa_log<-function(dist,coef,col0.5,col0.95, nombre){
  orden <- order(dist)
  
  plot(dist,coef[[col0.5]],col='blue',pch=19,xlab='log(1+elev(s)) (m)', ylab='Coeficiente',
       main=paste( nombre,'~ log(1+elev(s)) '))
  points(dist,coef[[col0.95]],col='red',pch=19)
  
  legend("bottomright", title = "Cuantil", legend = c("0.5", "0.95"),
         col = c("blue", "red"), lwd = 2)
  
  mod1<-lm(coef[[col0.5]]~dist)
  lines(dist[orden], predict(mod1)[orden], col = "blue", lwd = 2)
  
  mod2<-lm(coef[[col0.95]]~dist)
  lines(dist[orden], predict(mod2)[orden], col = "red", lwd = 2)
  
}

pend_costa_log(lelev,coef_R_sc,'q0.5_g300','q0.95_g300','G300')


pend_costa(dist,coef_R_sc,'q0.5_g300','q0.95_g300','G300')
pend_costa(dist,coef_R_sc,'q0.5_g300_45_.10','q0.95_g300_45_.10','G300 45ºN 10ºW')
pend_costa(dist,coef_R_sc,'q0.5_g300_45_5','q0.95_g300_45_5','G300 45ºN 5ºE')
pend_costa(dist,coef_R_sc,'q0.5_g300_35_.10','q0.95_g300_35_.10','G300 35ºN 10ºW')
pend_costa(dist,coef_R_sc,'q0.5_g300_35_5','q0.95_g300_35_5','G300 35ºN 5ºE')

pend_costa(dist,coef_R_sc,'q0.5_g500','q0.95_g500','G500')
pend_costa(dist,coef_R_sc,'q0.5_g500_45_5','q0.95_g500_45_5','G500 45ºN 5ºE')
pend_costa(dist,coef_R_sc,'q0.5_g500_35_.10','q0.95_g500_35_.10','G500 35ºN 10ºW')
pend_costa(dist,coef_R_sc,'q0.5_g500_35_5','q0.95_g500_35_5','G500 35ºN 5ºE')

pend_costa(dist,coef_R_sc,'q0.5_g700','q0.95_g700','G700')
pend_costa(dist,coef_R_sc,'q0.5_g700_45_.10','q0.95_g700_45_.10','G700 45ºN 10ºW')
pend_costa(dist,coef_R_sc,'q0.5_g700_45_5','q0.95_g700_45_5','G700 45ºN 5ºE')
pend_costa(dist,coef_R_sc,'q0.5_g700_35_.10','q0.95_g700_35_.10','G700 35ºN 10ºW')
pend_costa(dist,coef_R_sc,'q0.5_g700_35_5','q0.95_g700_35_5','G700 35ºN 5ºE')

par(mfrow=c(1,3))
pend_costa(dist,coef_R_sc,'q0.5_g300','q0.95_g300','G300')
pend_costa(dist,coef_R_sc,'q0.5_g500','q0.95_g500','G500')
pend_costa(dist,coef_R_sc,'q0.5_g700','q0.95_g700','G700')

par(mfrow=c(1,3))
pend_costa_log(ldist,coef_R_sc,'q0.5_g300','q0.95_g300','G300')
pend_costa_log(ldist,coef_R_sc,'q0.5_g500','q0.95_g500','G500')
pend_costa_log(ldist,coef_R_sc,'q0.5_g700','q0.95_g700','G700')

par(mfrow=c(1,3))
pend_costa_log(ldist,coef_R_sc,'int_q0.5_g300','int_q0.95_g300','G300 (Int)')
pend_costa_log(ldist,coef_R_sc,'int_q0.5_g500','int_q0.95_g500','G500 (Int)')
pend_costa_log(ldist,coef_R_sc,'int_q0.5_g700','int_q0.95_g700','G700 (Int)')

par(mfrow=c(1,3))
pend_costa_log(lelev,coef_R_sc,'int_q0.5_g300','int_q0.95_g300','G300 (Int)')
pend_costa_log(lelev,coef_R_sc,'int_q0.5_g500','int_q0.95_g500','G500 (Int)')
pend_costa_log(lelev,coef_R_sc,'int_q0.5_g700','int_q0.95_g700','G700 (Int)')

par(mfrow=c(1,3))
pend_costa_log(lelev,coef_R_sc,'q0.5_g300','q0.95_g300','G300')
pend_costa_log(lelev,coef_R_sc,'q0.5_g500','q0.95_g500','G500')
pend_costa_log(lelev,coef_R_sc,'q0.5_g700','q0.95_g700','G700')


