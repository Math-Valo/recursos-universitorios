# P05_Prueba_de_dos_medias_01.r
# Contrasta la hipótesis para las medias de dos poblaciones
########################################################
# Se realizan los cálculos para una prueba de hipótesis de
# dos muestra para la diferencias de medias poblacionales,
# ya sean muestras independientes o dependientes (muestras
# pareadas), usando el P-valor y calculando la región de
# rechazo para un nivel de significancia predeterminado. Se
# va a suponer distribuciones normales poblacionales en el
# caso de muestras pequeñas, pues se toman en cuenta los
# tamaños muestrales para realizar la prueba normal estándar
# o la prueba t. El nivel de significancia es opcional.
# Asignado un nivel de significancia, en la salida se da un
# veredicto a la prueba.
#
# Se está suponiendo conocido el resumen de los datos y,
# por lo tanto, no se leerán datos de un archivo externo.
########################################################
########################################################
# Sección modificable por el usuario
########################################################

# Tamaños de las muestras
n1<-30
n2<-30

# Diferencia de las medias de la prueba
mu<-0

# Medias de la muestra
# individuales
m1<-NULL
m2<-NULL
# de la diferencia
m<-34

# Desviaciones estándar
# poblacionales individuales
sigma1<-NULL
sigma2<-NULL

# Desviaciones estándar
# muestrales individuales
s1<-10.5
s2<-10.2

# desviación estándar muestral de la diferencia
# en muestras pareadas
sD<-NULL

# Desviaciones estándar desconocidas pero iguales
desv.iguales<-TRUE

# Nivel de significancia
alfa<-NULL

# Tipo de prueba: cola superior (S), cola inferior (I) o dos colas (D)
cola<-'S'

# ¿Son muestras pareadas?
par<-FALSE

########################################################
# Sección que realiza el procedimiento
########################################################

if(is.null(m)) m<-m1-m2
if(n1>=30 & n2>=30){
   if(is.null(sigma1)) sigma1<-s1
   if(is.null(sigma2)) sigma2<-s2
}
if(par & n1 != n2){
   stop("Las muestras pareadas deben tener el mismo tamaño.")
}

TestMedia<-function(n1,n2,mu,m,desv1,desv2,sD=NULL,alfa=0.05,colas='D',
                    conocidas=FALSE,iguales=FALSE,pareadas=FALSE){
  if(n1 < 2 | n2 < 2 | par & is.null(sD)){
    if(is.null(sD)){
       error<-ifelse(is.na(desv1)|is.na(desv2),
                     NA,sqrt(desv1^2/n1+desv2^2/n2))
    }else{
       error<-sD
    }
    r<-data.frame(Prueba=NA, H0=mu,
                  n1=n1, n2=n2,
                  DifMedias=m,
                  desv.est1=ifelse(is.na(desv1),NA,desv1),
                  desv.est2=ifelse(is.na(desv2),NA,desv2),
                  error.est=error,
                  alpha=alfa,
                  PValor=NA,
                  Estadistico=NA,
                  RegionRechazoZ=NA,
                  RegionRechazoX=NA)
  }
  estadistico<-m-mu
  if(!pareadas){
    if(conocidas){
      desv<-sqrt(desv1^2/n1+desv2^2/n2)
      estadistico<-estadistico/desv
      r<-data.frame(Prueba="Z",H0=mu,
                    n1=n1, n2=n2,
                    DifMedias=m,
                    desv.est1=desv1, desv.est2=desv2,
                    error.est=desv,
                    alpha=alfa)
      if(colas=='D'){
        pvalor<-round(2*pnorm(abs(estadistico),lower.tail=F),7)
        criticoz<-round(qnorm(1-alfa/2),7)
        criticox<-round(criticoz*desv,7)
        r$PValor<-pvalor
        r$Estadistico<-estadistico
        r$RegionRechazoZ<-paste("<",-criticoz," y >",criticoz)
        r$RegionRechazoX<-paste("<",mu-criticox," y >",mu+criticox)
      }else{
        criticoz<-round(qnorm(1-alfa),7)
        criticox<-round(criticoz*desv,7)
        if(colas=='I'){
          pvalor<-round(pnorm(estadistico),7)
          r$PValor<-pvalor
          r$Estadistico<-estadistico
          r$RegionRechazoZ<-paste("<",-criticoz)
          r$RegionRechazoX<-paste("<",mu-criticox)
        }else{
          pvalor<-round(pnorm(estadistico,lower.tail=F),7)
          r$PValor<-pvalor
          r$Estadistico<-estadistico
          r$RegionRechazoZ<-paste(">",criticoz)
          r$RegionRechazoX<-paste(">",mu+criticox)
        }
      }
    }else{
      if(iguales){
        grados<-n1+n2-2
        sp<-sqrt((desv1^2*(n1-1)+desv2^2*(n2-1))/grados)
        desv<-sp*sqrt(1/n1+1/n2)
        estadistico<-estadistico/desv
        r<-data.frame(Prueba="t",
                      var.pobl="Iguales",
                      H0=mu,
                      n1=n1,n2=n2,
                      DifMedias=m,
                      desv.est1=desv1,
                      desv.est2=desv2,
                      est.sp=sp,
                      error.est=desv,
                      grados.libertad=grados,
                      alpha=alfa)
      }else{
        numerador<-(desv1^2/n1 + desv2^2/n2)^2
        denominador<-(desv1^2/n1)^2/(n1-1) + (desv2^2/n2)^2/(n2-1)
        grados<-round(numerador/denominador,0)
        desv<-sqrt(desv1^2/n1 + desv2^2/n2)
        estadistico<-estadistico/desv
        r<-data.frame(Prueba="t",
                      var.pobl="Diferentes",
                      H0=mu,
                      n1=n1,n2=n2,
                      DifMedias=m,
                      desv.est1=desv1,
                      desv.est2=desv2,
                      error.est=desv,
                      grados.libertad=grados,
                      alpha=alfa)
      }
      if(colas=='D'){
        pvalor<-round(2*pt(abs(estadistico),grados,lower.tail=F),7)
        criticot<-round(qt(1-alfa/2,grados),7)
        criticox<-round(criticot*desv,7)
        r$PValor<-pvalor
        r$Estadistico<-estadistico
        r$RegionRechazoT<-paste("<",-criticot," y >",criticot)
        r$RegionRechazoX<-paste("<",mu-criticox," y >",mu+criticox)
      }else{
        criticot<-round(qt(1-alfa,grados),7)
        criticox<-round(criticot*desv,7)
        if(colas=='I'){
          pvalor<-round(pt(estadistico,grados),7)
          r$PValor<-pvalor
          r$Estadistico<-estadistico
          r$RegionRechazoT<-paste("<",-criticot)
          r$RegionRechazoX<-paste("<",mu-criticox)
        }else{
          pvalor<-round(pt(estadistico,grados,lower.tail=F),7)
          r$PValor<-pvalor
          r$Estadistico<-estadistico
          r$RegionRechazoT<-paste(">",criticot)
          r$RegionRechazoX<-paste(">",mu+criticox)
        }
      }
    }
  }
  else{
    desv<-sD/sqrt(n1)
    estadistico<-estadistico/desv
    grados<-n1-1
    r<-data.frame(Prueba="t",
                  Tipo="Muestras pareadas",
                  H0=mu,
                  n=n1,
                  MediaPareada=m,
                  desv.par=sD,
                  error.est=desv,
                  grados.libertad=grados,
                  alpha=alfa)
    if(colas=='D'){
      pvalor<-round(2*pt(abs(estadistico),grados,lower.tail=F),7)
      criticot<-round(qt(1-alfa/2,grados),7)
      criticox<-round(criticot*desv,7)
      r$PValor<-pvalor
      r$Estadistico<-estadistico
      r$RegionRechazoT<-paste("<",-criticot," y >",criticot)
      r$RegionRechazoX<-paste("<",mu-criticox," y >",mu+criticox)
    }else{
      criticot<-round(qt(1-alfa,grados),7)
      criticox<-round(criticot*desv,7)
      if(colas=="I"){
        pvalor<-round(pt(estadistico,grados),7)
        r$PValor<-pvalor
        r$Estadistico<-estadistico
        r$RegionRechazoT<-paste("<",-criticot)
        r$RegionRechazoX<-paste("<",mu-criticox)
      }else{
        pvalor<-round(pt(estadistico,grados,lower.tail=F),7)
        r$PValor<-pvalor
        r$Estadistico<-estadistico
        r$RegionRechazoT<-paste(">",criticot)
        r$RegionRechazoX<-paste(">",mu+criticox)
      }
    }
  }
  return(r)
}

if(is.null(alfa)){
  if(!is.null(sigma1)){
    Test<-TestMedia(n1,n2,mu,m,sigma1,sigma2,colas=cola,conocidas=TRUE,
                    pareadas=par)
  }else{
    Test<-TestMedia(n1,n2,mu,m,s1,s2,sD=sD,colas=cola,conocidas=FALSE,
                    iguales=desv.iguales,pareadas=par)
  }
}else{
  if(!is.null(sigma1)){
    Test<-TestMedia(n1,n2,mu,m,sigma1,sigma2,alfa=alfa,colas=cola,
                    conocidas=TRUE,pareadas=par)
  }else{
    Test<-TestMedia(n1,n2,mu,m,s1,s2,sD=sD,alfa=alfa,colas=cola,
                    conocidas=FALSE,iguales=desv.iguales,pareadas=par)
  }
  resultado<-ifelse(Test[,"PValor"]>=alfa,"No se rechaza H0","Se rechaza H0")
  Test$Resultado<-resultado
  Test$Resultado[is.na(Test$Resultado)]<-"Pocos datos"
}

########################################################
# Muestra de los resultados
########################################################

Test
