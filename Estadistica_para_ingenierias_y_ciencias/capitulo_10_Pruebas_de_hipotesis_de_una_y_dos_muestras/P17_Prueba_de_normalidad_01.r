# P17_Prueba_de_normalidad_01
# Ajustes de distribución de normalidad
########################################################
# .
#
# Se leerá de un archivo externo los datos necesarios y se
# calcularán los valores para el resultado de la prueba de
# hipótesis.
########################################################
########################################################
# Paquetes requeridos
########################################################
require(MASS)  # Para la gráfica truehist
require(nortest)  # Para la prueba de lilliefors
########################################################
########################################################
# Sección modificable por el usuario
########################################################

# Lectura de la base de datos
datos<-read.csv("DB37_Problema_102.csv",sep=";",encoding="UTF-8")

# Selección de la variable de agrupación de interés
varInteres<-"Oxígeno.ml.kg.min"

# Parámetros supuestos para  la distribución normal
media<-NULL
desv.est<-NULL

# Clases para agrupar los valores
# Puede darse como un vector con los límites definidos,
# o con un valor con valor con el tamaño deseado.
# Si desea que se cree automáticamente, se deja como NULL
# clases<-NULL
# clases<-9
# clases<-c(9.5,19.5,29.5,39.5,49.5,59.5,69.5,79.5,89.5,99.5)
# clases<-c(-0.05,0.95,1.95,2.95,3.95,4.95,5.95,6.95)
# clases<-c(0,1,2,3,4,5,6,7)
clases<-NULL


graficaHist<-FALSE





# Función para colapsar categor?as de valores esperados
# inferiores a un límite predeterminado.
colapsar<-function(p1,f1,lim1=5){
  np1<-p1
  nf1<-f1
  tocollapse<-which((p1*sum(f1))<lim1)
  while(length(tocollapse)>0){
    x<-tocollapse[1]
    if (x<(length(f1)/2)){
      np1[x+1]<-p1[x]+p1[x+1]
      nf1[x+1]<-f1[x]+f1[x+1]
    }else{
      np1[x-1]<-p1[x]+p1[x-1]
      nf1[x-1]<-f1[x]+f1[x-1]
    }
    p1<-np1[-x]
    f1<-nf1[-x]
    np1<-p1
    nf1<-f1
    tocollapse<-which((p1*sum(f1))<lim1)
  }
  return(list(probs=p1,freqs=f1))  
}

calculos2<-function(x,media=NULL,desv.est=NULL,clases){
  n<-length(x)
  ajuste<-0
  if (is.null(media)) {
    media<-mean(x)
    ajuste<-ajuste+1
  }
  if (is.null(desv.est)) {
    desv.est<-sd(x)
    ajuste<-ajuste+1
  }
  if(is.null(clases)){
    k<-round(sqrt(length(x)),0)
    clases<-min(x)
    clases<-c(clases,diff(range(x))/k*(1:(k-1))+min(x))
    clases<-c(clases,max(x))
  }
  if(length(clases)==1){
    k<-clases[1]
    clases<-min(x)
    clases<-c(clases,diff(range(x))/k*(1:(k-1))+min(x))
    clases<-c(clases,max(x))
  }
  k<-length(clases)
  frecObs<-hist(x,clases)$count
  probEsp<-pnorm(clases[2],media,desv.est)
  if(k>3) for(i in (2:(k-2))) {
    probEsp<-c(probEsp,
               pnorm(clases[i+1],media,desv.est,lower.tail=TRUE)
               -pnorm(clases[i],media,desv.est,lower.tail=TRUE))
  }
  probEsp<-c(probEsp,1-sum(probEsp))

  # Prueba de Geary
  U<-sqrt(pi/2)*mean(abs(x-mean(x)))/sqrt(var(x)*(n-1)/n)
  Geary.statistic<-(U-1)/(0.2661/sqrt(n))
  Geary.p.value<-2*pnorm(abs(Geary.statistic),lower.tail=FALSE)
  
  # Prueba de lilliefors -- Kolmogorov-Smirnov para normalidad
  pruebaKSlillie<-lillie.test(x)  # nortest
  
  # Prueba de shapiro wilk.
  pruebaSW<-shapiro.test(x)
  
  # Prueba de Kolmogorov-Smirnov.
  # ks.test()
  pruebaks<-ks.test(x[!duplicated(x)],"pnorm",mean=media,sd=desv.est)
  
  # Prueba Chi cuadrada
  ajuste<-0
  # Combinar frecuencias esperadas menores a 5
  r1<-colapsar(probEsp,frecObs)
  frecObsC<-r1$freqs
  probEspC<-r1$probs
  if(length(frecObsC)>1){
    pruebachisq<-chisq.test(frecObsC,p=probEspC)
  }else{
    pruebachisq<-NA
  }
  r<-c(n, pruebachisq$statistic, pruebachisq$parameter-ajuste,
       pruebachisq$p.value, Geary.statistic, Geary.p.value,
       pruebaks$statistic, pruebaks$p.value, pruebaKSlillie$statistic,
       pruebaKSlillie$p.value, pruebaSW$statistic,pruebaSW$p.value)
  return(r)
} 

generaGrafica<-function(x,media,desv.est,clases,nombres){
  if (is.null(media)) {
    media<-mean(x)
  }
  if (is.null(desv.est)) {
    desv.est<-sd(x)
  }
  if(is.null(clases)){
    ni<-round(sqrt(length(x)),0)
  }else if(length(clases)==1){
    ni<-clases[1]
  } else ni<-length(clases)
  x11()
  truehist(x,nbins=ni,sub=nombres,xlab="")
  x<-seq(min(x)*.8,max(x)*1.2,l=100)
  lines(x,dnorm(x,mean=media,sd=desv.est),lwd=1.5)
}

# Organización de la base de datos.
valores<-datos[,varInteres]
valores<-valores[!is.na(valores)]
if(length(valores)<3){
  stop("Se necesita de al menos 3 valores para proceder")
}

# Cálculos
r1<-t(as.matrix(calculos2(valores,media,desv.est,clases)))

r1<-data.frame(nombre=varInteres,r1)
names(r1)<-c("nombres","n","X de chi2", "param chi2", "Valor-p chi2",
             "Z de Geary", "Valor-p Geary", "D de K-S", "Valor-p K-S",
             "D de K-S Lilliefors", "Valor-p K-S Lilliefors",
             "W de Shapiro","Valor-p Shapiro")

########################################################
# Secci?n que muestra los resultados
########################################################

r1

if (graficaHist){
  generaGrafica(valores,media,desv.est,clases,varInteres)
}