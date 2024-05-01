# P12_Prueba_de_una_varianza_03.r
# Contrasta las hipótesis para la varianza de una población
########################################################
# Se realizan los cálculos para una prueba de hipótesis de
# una muestra para la varianza o desviación estándar
# poblacional cuando se tiene una muestra grande (n>=30) y
# basado en un estadístico con normal estándar, usando el
# P-valor y calculando la región de rechazo. El nivel de
# significancia es opcional para una prueba con el P-valor;
# en caso de no indicarlo, se usa un valor predeterminado
# en el nivel de significancia para la región de rechazo y,
# si se asigna un nivel de significancia, la salida da un
# veredicto a la prueba.
#
# Esta es una extensión de P11_Prueba_de_una_varianza_02.r
# por lo que también se puede realizar la prueba de una
# varianza usando el estadístico ji cuadrada en muestras
# pequeñas siempre que se suponga normal la distribución
# poblacional.
#
# Se están suponiendo conocido el resumen de los datos y,
# por lo tanto, no se leerán datos de un archivo externo.
########################################################
########################################################
# Sección modificable por el usuario
########################################################

# Tamaño de la muestra
n<-72

# Varianza de la prueba
sigma2<-6.25

# Varianza de la muestra
s2<-4.41

# Nivel de significancia
alfa<-0.05

# Tipo de prueba: cola superior (S), cola inferior (I) o dos colas (D)
cola<-'I'

# ¿La población se distribuye de forma normal?
val<-FALSE

########################################################
# Sección que realiza el procedimiento
########################################################

if(!val & n < 30){
  stop("Se requiere normalidad o muestras grandes")
}

TestVar<-function(n,varP,varM,alfa=0.05,colas='D',val){
  if(val){
    v<-n-1
    r<-data.frame(Distr="Chi Cuadrada",
                  n=n,
                  H0=varP,
                  var.muestral=varM,
                  grados=v)
    if(n<2){
      r$error.est<-NA
      r$alpha<-alfa
      r$Pvalor<-NA
      r$estadistico<-NA
      r$RegionRechazoJi<-NA
      r$RegionRechazoX<-NA
    }else{
      error<-varP/v
      estadistico<-varM*v/varP
      r$error.est<-round(error,7)
      r$alpha<-alfa
      if(colas=='I'){
        criticojiL<-qchisq(alfa,v)
        criticoXL<-criticojiL*error
        r$PValor<-round(pchisq(estadistico,v),7)
        r$Estadistico<-estadistico
        r$RegionRechazoJi<-paste("<",round(criticojiL,7))
        r$RegionRechazoX<-paste("<",round(criticoXL,7))
      }else{
        if (colas=='S'){
          criticojiU<-qchisq(alfa,v,lower.tail=F)
          criticoXU<-criticojiU*error
          r$PValor<-round(pchisq(estadistico,v,lower.tail=F),7)
          r$Estadistico<-estadistico
          r$RegionRechazoJi<-paste(">",round(criticojiU,7))
          r$RegionRechazoX<-paste(">",round(criticoXU,7))
        }else{
          criticojiU<-round(qchisq(alfa/2,v,lower.tail=F),7)
          criticoXU<-round(criticojiU*error,7)
          criticojiL<-round(qchisq(alfa/2,v),7)
          criticoXL<-round(criticojiL*error,7)
          pvalor<-round(pchisq(estadistico,v),7)
          r$PValor<-ifelse(varM<varP,2*pvalor,2*(1-pvalor))
          r$Estadistico<-estadistico
          r$RegionRechazoJi<-paste("<",criticojiL,7,"y >",criticojiU)
          r$RegionRechazoX<-paste("<",criticoXL,7,"y >",criticoXU)
        }
      }
      return(r)
    }
  }else{
    desvM<-sqrt(varM)
    desvP<-sqrt(varP)
    error<-round(desvP/sqrt(2*n),7)
    estadistico<-(desvM-desvP)/error
    r<-data.frame(Distr="Normal",
                  n=n,
                  H0=desvP,
                  desv.muestral=desvM,
                  error.est=error,
                  alpha=alfa
                  )
    pvalor<-round(pnorm(estadistico),7)
    if(colas=='D'){
     criticoZ<-round(qnorm(alfa/2,lower.tail = F),7)
     criticoX<-round(criticoZ*error,7)
     r$PValor<-ifelse(varM<varP, 2*pvalor, 2*(1-pvalor))
     r$Estadistico<-estadistico
     r$RegionRechazoZ<-paste("<",-criticoZ,"y >",criticoZ)
     r$RegionRechazoX<-paste("<",desvP-criticoX,"y >",desvP+criticoX)
    }else{
      criticoZ<-round(qnorm(alfa,lower.tail = F),7)
      criticoX<-round(criticoZ*error,7)
      if(colas=='S'){
        r$PValor<-(1-pvalor)
        r$Estadistico<-estadistico
        r$RegionRechazoZ<-paste(">",criticoZ)
        r$RegionRechazoX<-paste(">",desvP+criticoX)
      }else{
        r$PValor<-pvalor
        r$Estadistico<-estadistico
        r$RegionRechazoZ<-paste("<",-criticoZ)
        r$RegionRechazoX<-paste("<",desvP-criticoX)
      }
    }
  }
  return(r)
}

if(is.null(alfa)){
  Test<-TestVar(n,sigma2,s2,colas=cola,val=val)
}else{
  Test<-TestVar(n,sigma2,s2,alfa=alfa,colas=cola,val=val)
  resultado<-ifelse(Test[,"PValor"]>=alfa,
                    "No se rechaza H0","Se rechaza H0")
  Test$Resultado<-resultado
  Test$Resultado[is.na(Test$Resultado)]<-"Pocos datos"
}

########################################################
# Muestra de los resultados
########################################################

Test