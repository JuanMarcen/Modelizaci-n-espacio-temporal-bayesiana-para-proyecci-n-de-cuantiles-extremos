#estacionalidad pendientes
rm(list=ls())


#portatil
setwd('C:/Users/jumar/OneDrive/Escritorio/TFM/Datos')
coef_R_sc <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/coef_R_sc.rds")
coef_R_cuadrados <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/coef_R_cuadrados.rds")
coef_junio <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/coef_junio.rds")
coef_julio <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/coef_julio.rds")
coef_ag <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/coef_ag.rds")
coef_84_03 <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/coef_84_03.rds")
coef_64_83 <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/coef_64_83.rds")
coef_04_23 <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/coef_04_23.rds")
stations <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/stations.rds")

#tipo 1 meses, tipo 2 años
coef_df<-function(tipo,q0.5,q0.95,extension){ 
  # extension para indicar esquinas
  # q0.5 y q0.95 para indicar el nombre de columna
  # q0.5_g300, q0.95_g300
  if (tipo==1){
    df<-data.frame(
      station=coef_R_sc$station,
      color = stations$color,
      junio_q0.5=coef_junio[[paste0(q0.5,extension)]], 
      julio_q0.5= coef_julio[[paste0(q0.5,extension)]],
      agosto_q0.5 = coef_ag[[paste0(q0.5,extension)]], 
      junio_q0.95=coef_junio[[paste0(q0.95,extension)]],
      julio_q0.95= coef_julio[[paste0(q0.95,extension)]], 
      agosto_q0.95 = coef_ag[[paste0(q0.95,extension)]]
    )
  }
  
  if (tipo==2){
    df<-data.frame(
      station=coef_R_sc$station,
      color = stations$color,
      a64_83_q0.5=coef_64_83[[paste0(q0.5,extension)]], 
      a84_03_q0.5= coef_84_03[[paste0(q0.5,extension)]],
      a04_23_q0.5 = coef_04_23[[paste0(q0.5,extension)]],
      a64_83_q0.95=coef_64_83[[paste0(q0.95,extension)]],
      a84_03_q0.95= coef_84_03[[paste0(q0.95,extension)]], 
      a04_23_q0.95 = coef_04_23[[paste0(q0.95,extension)]]
    )
  }
  
  df$mu_q0.5<-apply(df[,c(3,4,5)],MARGIN = 1,FUN = mean)
  df$mu_q0.95<-apply(df[,c(6,7,8)],MARGIN = 1,FUN = mean)
  
  if (tipo==1){
    df[,c("junio_q0.5_sinmu","julio_q0.5_sinmu","agosto_q0.5_sinmu")]<-df[,c("junio_q0.5","julio_q0.5","agosto_q0.5")]-df$mu_q0.5
    df[,c("junio_q0.95_sinmu","julio_q0.95_sinmu","agosto_q0.95_sinmu")]<-df[,c("junio_q0.95","julio_q0.95","agosto_q0.95")]-df$mu_q0.95
  }
  
  if (tipo==2){
    df[,c("a64_83_q0.5_sinmu","a84_03_q0.5_sinmu","a04_23_q0.5_sinmu")]<-df[,c("a64_83_q0.5","a84_03_q0.5","a04_23_q0.5")]-df$mu_q0.5
    df[,c("a64_83_q0.95_sinmu","a84_03_q0.95_sinmu","a04_23_q0.95_sinmu")]<-df[,c("a64_83_q0.95","a84_03_q0.95","a04_23_q0.95")]-df$mu_q0.95
  }
  
  return(df)
}

#por meses 
coef_g300<-coef_df(1,'q0.5_g300','q0.95_g300','')
coef_g300_45_.10<-coef_df(1,'q0.5_g300','q0.95_g300','_45_.10')
coef_g300_45_5<-coef_df(1,'q0.5_g300','q0.95_g300','_45_5')
coef_g300_35_.10<-coef_df(1,'q0.5_g300','q0.95_g300','_35_.10')
coef_g300_35_5<-coef_df(1,'q0.5_g300','q0.95_g300','_35_5')

coef_g500<-coef_df(1,'q0.5_g500','q0.95_g500','')
coef_g500_45_.10<-coef_df(1,'q0.5_g500','q0.95_g500','_45_.10')
coef_g500_45_5<-coef_df(1,'q0.5_g500','q0.95_g500','_45_5')
coef_g500_35_.10<-coef_df(1,'q0.5_g500','q0.95_g500','_35_.10')
coef_g500_35_5<-coef_df(1,'q0.5_g500','q0.95_g500','_35_5')

coef_g700<-coef_df(1,'q0.5_g700','q0.95_g700','')
coef_g700_45_.10<-coef_df(1,'q0.5_g700','q0.95_g700','_45_.10')
coef_g700_45_5<-coef_df(1,'q0.5_g700','q0.95_g700','_45_5')
coef_g700_35_.10<-coef_df(1,'q0.5_g700','q0.95_g700','_35_.10')
coef_g700_35_5<-coef_df(1,'q0.5_g700','q0.95_g700','_35_5')


#por años
coef_g300_a<-coef_df(2,'q0.5_g300','q0.95_g300','')
coef_g300_45_.10_a<-coef_df(2,'q0.5_g300','q0.95_g300','_45_.10')
coef_g300_45_5_a<-coef_df(2,'q0.5_g300','q0.95_g300','_45_5')
coef_g300_35_.10_a<-coef_df(2,'q0.5_g300','q0.95_g300','_35_.10')
coef_g300_35_5_a<-coef_df(2,'q0.5_g300','q0.95_g300','_35_5')

coef_g500_a<-coef_df(2,'q0.5_g500','q0.95_g500','')
coef_g500_45_.10_a<-coef_df(2,'q0.5_g500','q0.95_g500','_45_.10')
coef_g500_45_5_a<-coef_df(2,'q0.5_g500','q0.95_g500','_45_5')
coef_g500_35_.10_a<-coef_df(2,'q0.5_g500','q0.95_g500','_35_.10')
coef_g500_35_5_a<-coef_df(2,'q0.5_g500','q0.95_g500','_35_5')

coef_g700_a<-coef_df(2,'q0.5_g700','q0.95_g700','')
coef_g700_45_.10_a<-coef_df(2,'q0.5_g700','q0.95_g700','_45_.10')
coef_g700_45_5_a<-coef_df(2,'q0.5_g700','q0.95_g700','_45_5')
coef_g700_35_.10_a<-coef_df(2,'q0.5_g700','q0.95_g700','_35_.10')
coef_g700_35_5_a<-coef_df(2,'q0.5_g700','q0.95_g700','_35_5')


est_pend<-function(tipo,df,nombre, layout =TRUE){
  if (layout) par(mfrow = c(1, 2))
  
  if (tipo==1){
    plot(unlist(as.vector(df[1,c('junio_q0.5','julio_q0.5','agosto_q0.5')])),type='l',xaxt='n'
         ,xlim=c(0.9,3.1),ylim=c(min(df[,-c(1,2)]),max(df[,-c(1,2)])),
         main=bquote(.(nombre) * .(' (') * tau *.(' = ') * .(0.5) * .(')')),
         xlab= 'Mes',ylab='Coeficiente',col=df[1,2])
    axis(1,at=c(1,2,3),labels=c('Junio','Julio','Agosto'))
    
    for(i in 1:dim(df)[1]){
      lines(unlist(as.vector(df[i,c('junio_q0.5','julio_q0.5','agosto_q0.5')])),col=df[i,2])
    }
    
    plot(unlist(as.vector(df[1,c('junio_q0.95','julio_q0.95','agosto_q0.95')])),type='l',xaxt='n'
         ,xlim=c(0.9,3.1),ylim=c(min(df[,-c(1,2)]),max(df[,-c(1,2)])),
         main=bquote(.(nombre) * .(' (') * tau *.(' = ') * .(0.95) * .(')')),
         xlab= 'Mes',ylab='Coeficiente',col=df[1,2])
    axis(1,at=c(1,2,3),labels=c('Junio','Julio','Agosto'))
    
    for(i in 1:dim(df)[1]){
      lines(unlist(as.vector(df[i,c('junio_q0.95','julio_q0.95','agosto_q0.95')])),col=df[i,2])
    }
  }
  
  if (tipo==2){
    plot(unlist(as.vector(df[1,c('a64_83_q0.5','a84_03_q0.5','a04_23_q0.5')])),type='l',xaxt='n'
         ,xlim=c(0.9,3.1),ylim=c(min(df[,-c(1,2)]),max(df[,-c(1,2)])),
         main=bquote(.(nombre) * .(' (') * tau *.(' = ') * .(0.5) * .(')')),
         xlab= 'Periodo',ylab='Coeficiente',col=df[1,2])
    axis(1,at=c(1,2,3),labels=c('1964-1983','1984-2003','2004-2023'))
    
    for(i in 1:dim(df)[1]){
      lines(unlist(as.vector(df[i,c('a64_83_q0.5','a84_03_q0.5','a04_23_q0.5')])),col=df[i,2])
    }
    
    plot(unlist(as.vector(df[1,c('a64_83_q0.95','a84_03_q0.95','a04_23_q0.95')])),type='l',xaxt='n'
         ,xlim=c(0.9,3.1),ylim=c(min(df[,-c(1,2)]),max(df[,-c(1,2)])),
         main=bquote(.(nombre) * .(' (') * tau *.(' = ') * .(0.95) * .(')')),
         xlab= 'Periodo',ylab='Coeficiente',col=df[1,2])
    axis(1,at=c(1,2,3),labels=c('1964-1983','1984-2003','2004-2023'))
    
    for(i in 1:dim(df)[1]){
      lines(unlist(as.vector(df[i,c('a64_83_q0.95','a84_03_q0.95','a04_23_q0.95')])),col=df[i,2])
    }
  }
  
}


#meses
est_pend(1,coef_g300,'G300')
est_pend(1,coef_g300_45_.10,'G300 45ºN 10ºW')
est_pend(1,coef_g300_45_5,'G300 45ºN 5ºE')
est_pend(1,coef_g300_35_.10,'G300 35ºN 10ºW')
est_pend(1,coef_g300_35_5,'G300 35ºN 5ºE')

est_pend(1,coef_g500,'G500')
est_pend(1,coef_g500_45_.10,'G500 45ºN 10ºW')
est_pend(1,coef_g500_45_5,'G500 45ºN 5ºE')
est_pend(1,coef_g500_35_.10,'G500 35ºN 10ºW')
est_pend(1,coef_g500_35_5,'G500 35ºN 5ºE')

est_pend(1,coef_g700,'G700')
est_pend(1,coef_g700_45_.10,'G700 45ºN 10ºW')
est_pend(1,coef_g700_45_5,'G700 45ºN 5ºE')
est_pend(1,coef_g700_35_.10,'G700 35ºN 10ºW')
est_pend(1,coef_g700_35_5,'G700 35ºN 5ºE')

#periodos
est_pend(2,coef_g300_a,'G300')
est_pend(2,coef_g300_45_.10_a,'G300 45ºN 10ºW')
est_pend(2,coef_g300_45_5_a,'G300 45ºN 5ºE')
est_pend(2,coef_g300_35_.10_a,'G300 35ºN 10ºW')
est_pend(2,coef_g300_35_5_a,'G300 35ºN 5ºE')

est_pend(2,coef_g500_a,'G500')
est_pend(2,coef_g500_45_.10_a,'G500 45ºN 10ºW')
est_pend(2,coef_g500_45_5_a,'G500 45ºN 5ºE')
est_pend(2,coef_g500_35_.10_a,'G500 35ºN 10ºW')
est_pend(2,coef_g500_35_5_a,'G500 35ºN 5ºE')

est_pend(2,coef_g700_a,'G700')
est_pend(2,coef_g700_45_.10_a,'G700 45ºN 10ºW')
est_pend(2,coef_g700_45_5_a,'G700 45ºN 5ºE')
est_pend(2,coef_g700_35_.10_a,'G700 35ºN 10ºW')
est_pend(2,coef_g700_35_5_a,'G700 35ºN 5ºE')


par(mfrow=c(3,4))
est_pend(1,coef_g300,'G300',layout=F)
est_pend(2,coef_g300_a,'G300',layout=F)
est_pend(1,coef_g500,'G500',layout=F)
est_pend(2,coef_g500_a,'G500',layout=F)
est_pend(1,coef_g700,'G700',layout=F)
est_pend(2,coef_g700_a,'G700',layout=F)
