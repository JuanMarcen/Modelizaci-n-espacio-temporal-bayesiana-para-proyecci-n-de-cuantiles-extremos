# escritura tablas
rm(list=setdiff(ls(),c('tabla_R_cercanos','tabla_R_modelosAIC','tabla_R_modelosAIC_2','tabla_R_BIC_finales')))


#portatil
tabla_R_cercanos <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/tabla_R_cercanos.rds")
tabla_R_BIC_finales <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/tabla_R_BIC_finales.rds")
tabla_R1_locales_q0.95 <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/tabla_R1_locales_q0.95.rds")
tabla_R1_locales_q0.5 <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/tabla_R1_locales_q0.5.rds")
tabla_comp_c_q0.5 <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/tabla_comp_c_q0.5.rds")
tabla_comp_c_q0.95 <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/tabla_comp_c_q0.95.rds")
tabla_R_final <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/tabla_R_final.rds")

rho_todos <- readRDS("C:/Users/jumar/OneDrive/Escritorio/TFM/Datos/Analisis Exploratorio/ModelosReg/bay/rho_todos.rds")

library(xtable)

print(xtable(tabla_R_cercanos))

esc_tabla_negrita<-function(tabla,colq0.5,colq0.95,negrita=T){

  fila <- rownames(tabla)
  columnas <- ncol(tabla)
  
  media_cant<-round(apply(tabla[1:6,],FUN = mean,MARGIN = 2),3)
  media_med<-round(apply(tabla[7:18,],FUN = mean,MARGIN = 2),3)
  media_centro<-round(apply(tabla[19:40,],FUN = mean,MARGIN = 2),3)
  media_todo<-round(apply(tabla,FUN = mean,MARGIN = 2),3)
  
  for (i in 1:dim(tabla)[1]){
    
    maxq0.5<-max(tabla[i,colq0.5])
    maxq0.95<-max(tabla[i,colq0.95])
    
    cat(fila[i],'& ')
    
    
    for (j in 1:columnas){
      
      x <- tabla[i,j]
      
      if (j %in% colq0.5 && x == maxq0.5 && negrita==T){
        cat('$\\mathbf{',format(x,nsmall=3),'}$')
      }else if(j %in% colq0.95 && x == maxq0.95 && negrita==T){
        cat('$\\mathbf{',format(x,nsmall=3),'}$')
      }else{
        cat('$',format(x,nsmall=3),'$')
      }
      
      
      
      #separo valor y salto linea
      if (j < columnas){
        cat(' & ')
      }else{
        cat(' \\\\\n')
      }
      
    }
    
    #separacion zonas
    if(i %in% c(6,18)){
      cat('[2pt]','\\hline', '\\\\ [-10pt]')
    }
    if(i==40){
      cat('[2pt]','\\hline \\\\ [-10pt]')
    }
    
    
    if (i==6){
      cat(' Media & ')
      for (j in 1:columnas){
        m<-media_cant[j]
        cat('$',format(m,nsmall=3),'$')
        if (j < columnas){
          cat(' & ')
        }else{
          cat(' \\\\ [2pt] \\hline \\\\[-10pt] \n')
        }
      }
    }
    
    if (i==18){
      cat(' Media & ')
      for (j in 1:columnas){
        m<-media_med[j]
        cat('$',format(m,nsmall=3),'$')
        if (j < columnas){
          cat(' & ')
        }else{
          cat(' \\\\ [2pt] \\hline \\\\[-10pt] \n')
        }
      }
    }
    
    if (i==40){
      cat(' Media & ')
      for (j in 1:columnas){
        m<-media_centro[j]
        cat('$',format(m,nsmall=3),'$')
        if (j < columnas){
          cat(' & ')
        }else{
          cat(' \\\\ [2pt] \\hline \\\\[-10pt] \n')
        }
      }
    }
  }
  
  cat(' Media total & ')
  for (j in 1:columnas){
    m<-media_todo[j]
    cat('$',format(m,nsmall=3),'$')
    if (j<columnas){
      cat(' & ')
    }else{
      cat(' \\\\ [2pt] \\hline')
    }
  }
  
}

esc_tabla_negrita_2<-function(tabla,colq0.5,colq0.95,col3,negrita=T){
  
  fila <- rownames(tabla)
  columnas <- ncol(tabla)
  
  media_cant<-round(apply(tabla[1:6,],FUN = mean,MARGIN = 2),3)
  media_med<-round(apply(tabla[7:18,],FUN = mean,MARGIN = 2),3)
  media_centro<-round(apply(tabla[19:40,],FUN = mean,MARGIN = 2),3)
  media_todo<-round(apply(tabla,FUN = mean,MARGIN = 2),3)
  
  for (i in 1:dim(tabla)[1]){
    
    maxq0.5<-max(tabla[i,colq0.5])
    maxq0.95<-max(tabla[i,colq0.95])
    max3<-max(tabla[i,col3])
    
    cat(fila[i],'& ')
    
    
    for (j in 1:columnas){
      
      x <- tabla[i,j]
      
      if (j %in% colq0.5 && x == maxq0.5 && negrita==T){
        cat('$\\mathbf{',format(x,nsmall=3),'}$')
      }else if(j %in% colq0.95 && x == maxq0.95 && negrita==T){
        cat('$\\mathbf{',format(x,nsmall=3),'}$')
      }else if(j %in% col3 && x == max3 && negrita==T){
        cat('$\\mathbf{',format(x,nsmall=3),'}$')
      }else{
        cat('$',format(x,nsmall=3),'$')
      }
      
      
      
      #separo valor y salto linea
      if (j < columnas){
        cat(' & ')
      }else{
        cat(' \\\\\n')
      }
      
    }
    
    #separacion zonas
    if(i %in% c(6,18)){
      cat('[2pt]','\\hline', '\\\\ [-10pt]')
    }
    if(i==40){
      cat('[2pt]','\\hline \\\\ [-10pt]')
    }
    
    
    if (i==6){
      cat(' Media & ')
      for (j in 1:columnas){
        m<-media_cant[j]
        cat('$',format(m,nsmall=3),'$')
        if (j < columnas){
          cat(' & ')
        }else{
          cat(' \\\\ [2pt] \\hline \\\\[-10pt] \n')
        }
      }
    }
    
    if (i==18){
      cat(' Media & ')
      for (j in 1:columnas){
        m<-media_med[j]
        cat('$',format(m,nsmall=3),'$')
        if (j < columnas){
          cat(' & ')
        }else{
          cat(' \\\\ [2pt] \\hline \\\\[-10pt] \n')
        }
      }
    }
    
    if (i==40){
      cat(' Media & ')
      for (j in 1:columnas){
        m<-media_centro[j]
        cat('$',format(m,nsmall=3),'$')
        if (j < columnas){
          cat(' & ')
        }else{
          cat(' \\\\ [2pt] \\hline \\\\[-10pt] \n')
        }
      }
    }
  }
  
  cat(' Media total & ')
  for (j in 1:columnas){
    m<-media_todo[j]
    cat('$',format(m,nsmall=3),'$')
    if (j<columnas){
      cat(' & ')
    }else{
      cat(' \\\\ [2pt] \\hline')
    }
  }
  
}

#------------------------TABLA R'S SIMPLES------------------------
esc_tabla_negrita(tabla_R_cercanos,colq0.5 = c(1,3,5),colq0.95 = c(2,4,6))
esc_tabla_negrita(tabla_R_45_5,colq0.5 = c(1,3,5),colq0.95 = c(2,4,6))

#------------------------TABLA R'S MULTIPLES------------------------
esc_tabla_negrita(tabla_R_BIC_finales,colq0.5 = c(1,2),colq0.95 = c(3,4))

xtable(as.matrix(tabla_R1_locales_q0.5),include.rownames=F)

xtable(tabla_R1_locales_q0.95)


#------------------------COMPARACIÓN MODELOS------------------------
esc_tabla_negrita_2(round(tabla_comp_c_q0.5,3),colq0.5 = c(1,2,3),colq0.95 = c(4,5,6),col3=c(7,8,9),negrita=T)
esc_tabla_negrita_2(round(tabla_comp_c_q0.95,3),colq0.5 = c(1,2,3),colq0.95 = c(4,5,6),col3=c(7,8,9),negrita=T)

esc_tabla_negrita(round(tabla_R_final,3),colq0.5 = c(1,2),colq0.95 = c(3,4),negrita=T)

#RHO
esc_tabla_negrita(round(rho_todos,3),colq0.5 = c(1,3,5),colq0.95 = c(2,4,6),negrita=F)
esc_tabla_negrita(round(rho_todos_f,3),colq0.5 = c(1),colq0.95 = c(2),negrita=F)
