# P09_Prueba_de_dos_proporciones_01.r
# Contrasta las hipótesis para proporción de una población
########################################################
# Se realizan los cálculos para una prueba de hipótesis de
# la diferencia de proporciones poblacionales a partir de los 
# casos favorables de dos muestras de poblaciones distintas.
# Calcula el P-valor y la región de rechazo. El nivel de
# significancia es opcional. Si es asignado un nivel de
# significancia, en la salida se da un veredicto a la prueba.
#
# Se están suponiendo conocido el resumen de los datos y,
# por lo tanto, no se leerán datos de un archivo externo.
########################################################
########################################################
# Sección modificable por el usuario
########################################################

# Tamaños de las muestras
n1<-300
n2<-400
# casos favorales (puede no darse)
x1<-72
x2<-70
# proporciones muestrales (para cada x no dada, según el caso)
p1<-NULL
p2<-NULL
# Nivel de significancia
alfa<-0.05
# Tipo de prueba, según la hipótesis alternativa:
# p1!=p2 (!=), p1>p2 (>) o p1<p2 (<)
alternativa<-'>'

########################################################
# Sección que realiza el procedimiento
########################################################

if(n1<30 || n2<30){
  stop("Las muestras no deben ser pequeñas (al menos de 30 observaciones).")
}
if(is.null(x1)){
  x1<-round(p1*n1)
}
if(is.null(x2)){
  x2<-round(p2*n2)
}

TestProp<-function(n1,n2,x1,x2,alfa=0.05,prueba='!='){
  p1<-x1/n1
  p2<-x2/n2
  p<-(x1+x2)/(n1+n2)
  r<-data.frame(alternativa=paste("p1",prueba,"p2"),
                n1=n1,n2=n2,
                x1=x1,x2=x2,
                p1=p1,p2=p2,
                pEstimada=p,
                DifProp=p1-p2,
                error.est=sqrt(p*(1-p)*(1/n1+1/n2)),
                alpha=alfa
  )
  estadistico<-(p1-p2)/sqrt(p*(1-p)*(1/n1+1/n2))
  if(prueba=='!='){
    pvalor<-round(2*pnorm(abs(estadistico),lower.tail=F),7)
    criticoz<-round(qnorm(1-alfa/2),7)
    r$PValor<-pvalor
    r$Estadistico<-estadistico
    r$RegionRechazoZ<-paste("<=",-criticoz," y >=",criticoz)
  }else{
    criticoz<-round(qnorm(1-alfa),7)
    if(prueba=='<'){
      pvalor<-round(pnorm(estadistico),7)
      r$PValor<-pvalor
      r$Estadistico<-estadistico
      r$RegionRechazoZ<-paste("<=",-criticoz)
    }else{
      pvalor<-round(pnorm(estadistico,lower.tail=F),7)
      r$PValor<-pvalor
      r$Estadistico<-estadistico
      r$RegionRechazoZ<-paste(">=",criticoz)
    }
  }
  return(r)
}

if(is.null(alfa)){
  Test<-TestProp(n1,n2,x1,x2,prueba=alternativa)
}else{
  Test<-TestProp(n1,n2,x1,x2,alfa,alternativa)
  resultado<-ifelse(Test[,"PValor"]>=alfa,"No se rechaza H0","Se rechaza H0")
  Test$Resultado<-resultado
}

########################################################
# Muestra de los resultados
########################################################

Test