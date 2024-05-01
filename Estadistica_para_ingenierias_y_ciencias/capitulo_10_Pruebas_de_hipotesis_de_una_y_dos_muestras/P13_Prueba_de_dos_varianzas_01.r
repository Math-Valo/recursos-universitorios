# P13_Prueba_de_dos_varianzas_01.r
# Contrasta la hipótesis para la proporción de varianzas de
# dos poblaciones
########################################################
# Se realizan los cálculos para una prueba de hipótesis de
# dos muestras para la proporción de varianzas poblacionales
# usando la proporción de varianzas muestrales, suponiendo
# que provienen de poblaciones normales independientes. Se
# calcula el P-valor y la región de rechazo para un nivel de
# significancia con ayuda del estadístico F de fisher. El
# nivel de significancia es opcional. Si se asigna un nivel
# de significancia, la salida mostrará un veredicto de la
# prueba.
#
# Se está suponiendo conocido el resumen de los datos y,
# por lo tanto, no se leerán datos de un archivo externo.
########################################################
########################################################
# Sección modificable por el usuario
########################################################

# Tamaños de las muestras
n1<-11
n2<-14

# valor de las varianzas muestrales
s1<-6.1^2
s2<-5.3^2

# Nivel de significancia
alfa<-NULL

# Tipo de prueba: cola superior (S), cola inferior (I) o dos colas (D)
cola<-'S'

########################################################
# Sección que realiza el procedimiento
########################################################

TestVarProp<-function(n1,n2,s1,s2,alfa=0.05,colas='D'){
  estadistico<-round(s1/s2,7)
  v1<-n1-1
  v2<-n2-1
  if(n1<2 | n2 < 2){
    r<-data.frame(n1=n1,n2=n2,
                  s1=s1,s2=s2,
                  v1=v1, v2=v2,
                  alpha=alfa,
                  PValor=NA,
                  Estadistico=estadistico,
                  RegionRechazo=NA
                  )
  }else{
    if(colas=='D'){
      regionL<-round(qf(alfa/2,n1-1,n2-1),7)
      regionU<-round(qf(alfa/2,n1-1,n2-1,lower.tail=F),7)
      pvalor<-2*min(pf(estadistico,n1-1,n2-1,lower.tail=F),
                    pf(estadistico,n1-1,n2-1))
      r<-data.frame(n1=n1,n2=n2,
                    s1=s1,s2=s2,
                    v1=v1,v2=v2,
                    alpha=alfa,
                    PValor=round(pvalor,7),
                    Estadistico=estadistico,
                    RegionRechazo=paste("<",regionL,"y >",regionU)
                    )
    }else{
      if(colas=='I'){
        region<-round(qf(alfa,n1-1,n2-1),7)
        pvalor<-round(pf(estadistico,n1-1,n2-1),7)
        r<-data.frame(n1=n1,n2=n2,
                      s1=s1,s2=s2,
                      v1=v1,v2=v2,
                      alpha=alfa,
                      PValor=pvalor,
                      Estadistico=estadistico,
                      RegionRechazo=paste("<",region)
                      )
      }else{
        region<-round(qf(alfa,n1-1,n2-1,lower.tail=F),7)
        pvalor<-round(pf(estadistico,n1-1,n2-1,lower.tail=F),7)
        r<-data.frame(n1=n1,n2=n2,
                      s1=s1,s2=s2,
                      v1=v1,v2=v2,
                      alpha=alfa,
                      PValor=pvalor,
                      Estadistico=estadistico,
                      RegionRechazo=paste(">",region))
      }
    }
  }
  return(r)
}

if(is.null(alfa)){
  Test<-TestVarProp(n1,n2,s1,s2,colas=cola)
}else{
  Test<-TestVarProp(n1,n2,s1,s2,alfa=alfa,colas=cola)
  resultado<-ifelse(Test[,"PValor"]>=alfa,
                    "No se rechaza H0","Se rechaza H0")
  Test$Resultado<-resultado
  Test$Resultado[is.na(Test$Resultado)]<-"Pocos datos"
}

########################################################
# Muestra de los resultados
########################################################

Test