# P08_Prueba_de_una_proporcion_01.r
# Contrasta las hipótesis para proporción de una población
########################################################
# Se realizan los cálculos para una prueba de hipótesis de
# una muestra para la proporción poblacional, usando la
# distribución binomial en muestras pequeñas, la aproximación
# a la distribución de Poisson en muestras grandes donde la
# proporción poblacional está cerca de 0 o 1, o aproximando
# a la distribución normal en cualquier otra muestra grande.
# Calcula el P-valor y la región de rechazo para un nivel de
# significancia predeterminada. La región de rechazo por la
# distribución binomial o la aproximación de Poisson está
# dado por la probabilidad de comenter un error tipo I, pero
# se da una región de rechazo sobre la cantida de casos
# favorables dada la cantidad de observaciones. El nivel de
# significancia es opcional. Si es asignado un nivel de
# significancia, en la salida se da un veredicto a la prueba.
# Por razones didácticas, se permite elegir opcionalmente la
# distribución con la que se desea realizar los cálculos,
#
# Se están suponiendo conocido el resumen de los datos y,
# por lo tanto, no se leerán datos de un archivo externo.
########################################################
########################################################
# Sección modificable por el usuario
########################################################

# Tamaño de la muestra
n<-48
# casos favorales (puede no darse)
x<-16
# proporción muestral (si se da en vez de x)
p0<-NULL
# proporcion poblacional
p<-0.25
# Nivel de significancia
alfa<-0.05
# Tipo de prueba: cola superior (S), cola inferior (I) o dos colas (D)
cola<-'S'
# Distribucion preferencial: Binomial (B), Normal (N) o Poisson (P)
distr<-NULL

########################################################
# Sección que realiza el procedimiento
########################################################

if(is.null(x)){
  x<-round(p0*n)
}

TestProp<-function(n,x,p,alfa=0.05,colas='D',preferencia=NULL){
  p0<-x/n
  if(is.null(preferencia)){
    if(n <= 20){
      distr<-'B'
    }else{
      if(abs(0.5 - p) > 0.4){  # ¿p cercano a 0 o 1 (a menos de 0.1 unidades)?
        distr<-'P'
      }
      else{
        distr<-'N'
      }
    }
  }else{
    distr<-preferencia
  }
  if(distr=='B'){
    r<-data.frame(distr="Binomial",
                  p=p,
                  n=n,
                  x=x,
                  pMuestral=p0,
                  media=n*p,
                  desv.est=sqrt(n*p*(1-p)),
                  alpha=alfa,
                  Estadistico=paste("Var. binomial X con p=",p," y n=",n)
                  )
    if(colas=='D'){
      if(p0<p){
        pvalor<-round(2*pbinom(x,n,p),7)
      }else{
        pvalor<-round(2*pbinom(x-1,n,p,lower.tail=F),7)
      }
      xInf<-qbinom(alfa/2,n,p)-1
      xSup<-qbinom(alfa/2,n,p,lower.tail=F)+1
      r$PValor<-pvalor
      r$RegionRechazoX<-paste("<=",xInf," y >=",xSup)
    }else{
      if(colas=='I'){
        r$PValor<-round(pbinom(x,n,p),7)
        r$RegionRechazoX<-paste("<=",qbinom(alfa,n,p)-1)
      }else{
        r$PValor<-round(pbinom(x-1,n,p,lower.tail=F),7)
        r$RegionRechazoX<-paste(">=",qbinom(alfa,n,p,lower.tail=F)+1)
      }
    }
  }else{
    if(distr=='N'){
      r<-data.frame(distr="Normal",
                    p=p,
                    n=n,
                    x=x,
                    pMuestral=p0,
                    media=n*p,
                    desv.est=sqrt(n*p*(1-p)),
                    alpha=alfa
                    )
      estadistico<-(p0-p)/sqrt(p*(1-p)/n)
      if(colas=='D'){
        pvalor<-round(2*pnorm(abs(estadistico),lower.tail=F),7)
        criticoz<-round(qnorm(1-alfa/2),7)
        criticoxInf<-floor(n*p-criticoz*sqrt(n*p*(1-p)))
        criticoxSup<-ceiling(n*p+criticoz*sqrt(n*p*(1-p)))
        r$PValor<-pvalor
        r$Estadistico<-estadistico
        r$RegionRechazoZ<-paste("<=",-criticoz," y >=",criticoz)
        r$RegionRechazoX<-paste("<=",criticoxInf," y >=",criticoxSup)
      }else{
        criticoz<-round(qnorm(1-alfa),7)
        if(colas=='I'){
          pvalor<-round(pnorm(estadistico),7)
          criticox<-floor(n*p-criticoz*sqrt(n*p*(1-p)))
          r$PValor<-pvalor
          r$Estadistico<-estadistico
          r$RegionRechazoZ<-paste("<=",-criticoz)
          r$RegionRechazoX<-paste("<=",criticox)
        }else{
          pvalor<-round(pnorm(estadistico,lower.tail=F),7)
          criticox<-ceiling(n*p+criticoz*sqrt(n*p*(1-p)))
          r$PValor<-pvalor
          r$Estadistico<-estadistico
          r$RegionRechazoZ<-paste(">=",criticoz)
          r$RegionRechazoX<-paste(">=",criticox)
        }
      }
    }else{
      r<-data.frame(distr="Poisson",
                    p=p,
                    n=n,
                    x=x,
                    pMuestral=p0,
                    media=n*p,
                    desv.est=sqrt(n*p*(1-p)),
                    alpha=alfa,
                    Estadistico=paste("Var. Poisson X con mu=",n*p)
                    )
      if(colas=='D'){
        if(p0<p){
          pvalor<-round(2*ppois(x,n*p),7)
        }else{
          pvalor<-round(2*ppois(x-1,n*p,lower.tail=F),7)
        }
        xInf<-qpois(alfa/2,n*p)-1
        xSup<-qpois(alfa/2,n*p,lower.tail=F)+1
        r$PValor<-pvalor
        r$RegionRechazoX<-paste("<=",xInf," y >=",xSup)
      }else{
        if(colas=='I'){
          r$PValor<-round(ppois(x,n*p),7)
          r$RegionRechazoX<-paste("<=",qpois(alfa,n*p)-1)
        }else{
          r$PValor<-round(ppois(x-1,n*p,lower.tail=F),7)
          r$RegionRechazoX<-paste(">=",qpois(alfa,n*p,lower.tail=F)+1)
        }
      }
    }
  }
  return(r)
}

if(is.null(alfa)){
  Test<-TestProp(n,x,p,colas=cola,preferencia=distr)
}else{
  Test<-TestProp(n,x,p,alfa,cola,distr)
  resultado<-ifelse(Test[,"PValor"]>=alfa,"No se rechaza H0","Se rechaza H0")
  Test$Resultado<-resultado
}

########################################################
# Muestra de los resultados
########################################################

Test