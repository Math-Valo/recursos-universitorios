# P16_Prueba_de_bondad_chi2_02
# Ajustes de distribuciones con el test X^2
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
########################################################
# Sección modificable por el usuario
########################################################

# Lectura de la base de datos
datos<-read.csv("DB17_Problema_081.csv",sep=";",encoding="UTF-8")

# Selección de la variable de agrupación de interés
varInteres<-"Mezcla.nueces"

# Selección del campo de frecuencias
varFrecuencia<-"Frecuencia"

# Selección del campo de probabilidades esperados
varProb<-"Probabilidad"

# ¿Combinar categorías de tamaño inferior a 5?
combinar<-TRUE

# Gráfica comparativa
grafica<-TRUE

# Título genérico del eje x
tituloEjeX<-"Mezcla de varios tipos de nueces"

# Nivel de significancia
alfa<-NULL

########################################################
# Sección que realiza el procedimiento
########################################################

# Preparar los datos
valores<-datos[,varInteres]
frecObs<-datos[,varFrecuencia]
probEsp<-datos[,varProb]

# Función para colapsar categorías de valores esperados
# inferiores a un límite predeterminado.
colapsar<-function(p1,f1,lim1=5){
  #if (){
  # return(list(probs=NA,freqs=NA))
  #}
  np1<-p1
  nf1<-f1
  tocollapse<-which((np1*sum(nf1))<lim1)
  while(length(tocollapse)>0){
    x<-tocollapse[1]
    if (x<length(np1)){
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

calculos<-function(frecObs,probEsp,alfa=0.05){
  if(length(frecObs)>1){
    r<-NULL
    if(combinar){
      r1<-colapsar(probEsp,frecObs)
      frecObsC<-r1$freqs
      probEspC<-r1$probs
      if(length(frecObsC)>1){
        prueba<-chisq.test(frecObsC,p=probEspC)
      }else{
        prueba<-NA
      }
    }else{
      prueba<-chisq.test(frecObs,p=probEsp)
    }
    if(!is.na(prueba[1]) & prueba$parameter > 0){
      r<-c(prueba$statistic, prueba$parameter,prueba$p.value,alfa,
           qchisq(alfa,prueba$parameter,lower.tail=FALSE))
    }
    if(is.null(r)){
      r<-rep(NA,5)
    }
    return(r)
  }
}

if(!is.null(alfa)){
  tabla<-t(as.matrix(calculos(frecObs,probEsp,alfa)))
}else{
  tabla<-t(as.matrix(calculos(frecObs,probEsp)))
}

tabla<-data.frame(nombre=varInteres,tabla)

names(tabla)<-c("Variable","Estadístico Chisq2",
                "Grados de libertad","Valor-p","alpha","Región de Rechazo")

tabla["Región de Rechazo"]<-paste(">",round(tabla["Región de Rechazo"],7))

generaGrafica<-function(valores,frecuencia,probEspG,tituloEjeX,nomVariable){
  x<-c()
  for(i in 1:length(valores)){
    x<-c(x,rep(valores[i],frecuencia[i]))
  }
  maxD<-max(density(x)$y)
  x11()
  probEspG<-c(probEspG,0)
  truehist(x,h=1,
           main=paste("Comparación de distribuciones","\n","Específica"),
           xlab=tituloEjeX, ylab="Densidad",sub=nomVariable,
           xlim=c((min(x)-1),(max(x)+2)),
           ymax=max(max(probEspG),maxD)*1.2)
  lines((min(x)):(max(x)+1),probEspG,type="s",col="red",lty=2,lwd=2)
  legend("topleft",legend = c("Dist. original", "Dist. teórica"),
         lwd=c(1,2),lty=2,col=c("black","red"))
  invisible()
}

if(!is.null(alfa)){
  tabla$alpha<-alfa
  tabla$Resultado<-ifelse(tabla$"Valor-p">=alfa,
                         "No se rechaza el ajuste de bondad",
                         "Se rechaza el ajuste de bondad")
}

########################################################
# Sección que muestra los resultados
########################################################
tabla

if (grafica){
  generaGrafica(valores,frecObs,probEsp,tituloEjeX,varInteres)
}
