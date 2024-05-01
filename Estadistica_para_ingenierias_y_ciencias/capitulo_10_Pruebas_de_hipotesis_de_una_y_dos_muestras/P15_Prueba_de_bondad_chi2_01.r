# P15_Prueba_de_bondad_chi2_01
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
datos<-read.csv("DB39_Problema_107.csv",sep=";",encoding="UTF-8")

# Selección de las variables de interés
varInteres<-"Frecuencia"

# En caso de que los valores ya se encuentren agrupados
# Selección de la variable de agrupación, caso contrario se asigna NULL
varCelda<-"Cacahuates.cantidad"

# Distribución para la cual se quiere hacer el ajuste
# 1. Uniforme (sin parámetros)
# 2. Binomial (p)
# 3. Hipergeométrica (N: tamaño pob., n: tamaño muestral, k: casos favorables)
# 4. Geométrica (p: probabilidad de éxito)
distribucion<-1

# Parámetros
# nombres de los parámetros
parametro_nombre<-NULL
# valores de los parámetros
parametro_valor<-NULL

# ¿Combinar categorías de tamaño inferior a 5?
combinar<-TRUE

# Gráfica comparativa
grafica<-TRUE

# Título genérico del eje x
tituloEjeX<-"Cacahuates distintos en la lata de nueces surtidas"

# Nivel de significancia
alfa<-0.01

########################################################
# Sección que realiza el procedimiento
########################################################

# Preparar los datos
valores<-datos[,varInteres]
valores<-valores[!is.na(valores)]  # Se supone que datos resumidos no tienen NA

# Los datos deben comenzar ya agrupados en celdas con las variables ordenadas
if(is.null(varCelda)){
  frecuencia<-as.vector(table(valores))
  valores<-as.integer(names(table(valores)))
}else{
  frecuencia<-valores[order(datos[,varCelda])]
  valores<-sort(datos[,varCelda])
  varInteres<-varCelda
}

# Crea un DataFrame de parámetros
cantParametros<-min(length(parametro_nombre),length(parametro_valor))
diccionario_parametros<-data.frame(Nombre=parametro_nombre[1:cantParametros],
                                   Valor=parametro_valor[1:cantParametros])

# Nombra las distribuciones
switch(distribucion,
       nomDist<-"Uniforme",
       nomDist<-"Binomial",
       nomDist<-"Hipergeométrica",
       nomDist<-"Geométrica")

# Cantidad de parámetros
switch(distribucion,
       maxParam<-0,
       maxParam<-1,
       maxParam<-3,
       maxParam<-1)

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

calculos<-function(valores,frecuencia,distribucion,parametros,alfa=0.05){
  if(length(frecuencia)>1){
    r<-NULL
    if(distribucion==1){  # Uniforme discreta
      k<-length(frecuencia)  # total de celdas
      frecObs<-frecuencia
      probEsp<-rep(1/k,k)
      # frecEsp<-probEsp*n
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
      }else{
        r<-rep(NA,3)
      }
    }else if(distribucion==2){  # Binomial
      ajustado<-identical(which(parametros$Nombre=="p"),integer(0))
      n<-max(valores)
      if(ajustado){
        media<-sum(valores*frecuencia)/sum(frecuencia)
        prob1<-media/n
      }else{
        prob1<-parametros$Valor[which(parametros$Nombre=="p")]
      }
      frecObs<-frecuencia
      probEsp<-dbinom(0:n,size=n,prob=prob1)
      # frecEsp<-probEsp*frecuencia
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
        if(ajustado){
          r<-c(prueba$statistic, prueba$parameter-1,
               pchisq(prueba$statistic, prueba$parameter-1,lower.tail=FALSE),
               alfa, qchisq(alfa,prueba$parameter-1,lower.tail=FALSE))
        }else{
          r<-c(prueba$statistic, prueba$parameter, prueba$p.value,alfa,
               qchisq(alfa,prueba$parameter,lower.tail=FALSE))
        }
      }else{
        r<-rep(NA,3)
      }
    }else if(distribucion==3){  # Hipergeométrica
      estimarN<-identical(integer(0),which(parametros$Nombre=="N"))
      estimarn<-identical(integer(0),which(parametros$Nombre=="n"))
      estimark<-identical(integer(0),which(parametros$Nombre=="k"))
      ajustes<-0
      if(estimarn){
        n<-max(valores)
        ajustes<-ajustes+1
      }else{
        n<-parametros$Valor[which(parametros$Nombre=="n")]
      }
      if(!estimarN | !estimark){
        if(estimark) {
          N<-parametros$Valor[which(parametros$Nombre=="N")]
          media<-sum(valores*frecuencia)/sum(frecuencia)
          k<-round(media*N/n,0)
          ajustes<-ajustes+1
        }else{
          k<-parametros$Valor[which(parametros$Nombre=="k")]
          if(estimarN) {
            media<-sum(valores*frecuencia)/sum(frecuencia)
            N<-floor(n*k/media)
            ajustes<-ajustes+1
          }else{
            N<-parametros$Valor[which(parametros$Nombre=="N")]
          }
        }
      }else{
        stop("No se puede estimar a la vez N y k.")
      }
      frecObs<-frecuencia
      probEsp<-dhyper(0:n,k,N-k,n)
      # frecEsp<-probEsp*frecuencia
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
      if(!is.na(prueba[1]) & prueba$parameter-ajustes> 0){
        r<-c(prueba$statistic, prueba$parameter-ajustes,
             pchisq(prueba$statistic, prueba$parameter-ajustes,lower.tail=FALSE),
             alfa, qchisq(alfa,prueba$parameter-ajustes,lower.tail=FALSE))
      }else{
        r<-rep(NA,3)
      }
    }else if(distribucion==4){  # Geométrica
      ajustes<-as.integer(identical(which(parametros$Nombre=="p"),integer(0)))
      n<-max(valores)
      if(ajustes){
        media<-sum(valores*frecuencia)/sum(frecuencia)
        prob1<-1/media
      }else{
        prob1<-parametros$Valor[which(parametros$Nombre=="p")]
      }
      frecObs<-frecuencia
      probEsp<-dgeom(0:(n-2),prob1)
      probEsp<-c(probEsp, 1-sum(probEsp))
      # frecEsp<-probEsp*frecuencia
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
      if(!is.na(prueba[1]) & prueba$parameter-ajustes> 0){
        r<-c(prueba$statistic, prueba$parameter-ajustes,
             pchisq(prueba$statistic, prueba$parameter-ajustes,lower.tail=FALSE),
             alfa, qchisq(alfa,prueba$parameter-ajustes,lower.tail=FALSE))
      }else{
        r<-rep(NA,3)
      }
    }
    if(is.null(r)){
      r<-rep(NA,3)
    }
    return(r)
  }
}

if(!is.null(alfa)){
  tabla<-t(as.matrix(calculos(valores,frecuencia,distribucion,diccionario_parametros,alfa)))
}else{
  tabla<-t(as.matrix(calculos(valores,frecuencia,distribucion,diccionario_parametros)))
}

if(cantParametros>0){
  tabla<-data.frame(nombre=varInteres,Ajuste=nomDist,
                    t(parametro_valor[1:cantParametros]),tabla)
  auxNombresParam<-parametro_nombre
}else{
  tabla<-data.frame(nombre=varInteres,Ajuste=nomDist,tabla)
  auxNombresParam<-NULL
}

if (cantParametros!=maxParam) {
    names(tabla)<-c("Variable","Distribución de ajuste",auxNombresParam,
                    "Estadístico Chisq2","Grados de libertad ajustado",
                    "Valor-p ajustado","alpha","Región de Rechazo")
}else{
  names(tabla)<-c("Variable","Distribución de ajuste",auxNombresParam,
                  "Estadístico Chisq2","Grados de libertad",
                  "Valor-p","alpha","Región de Rechazo")
}
tabla["Región de Rechazo"]<-paste(">",round(tabla["Región de Rechazo"],7))

generaGrafica<-function(valores,frecuencia,parametros=NULL,distribucion=1,tituloEjeX,nomVariable){
  x<-c()
  for(i in 1:length(valores)){
    x<-c(x,rep(valores[i],frecuencia[i]))
  }
  maxD<-max(density(x)$y)
  if(distribucion==1){  # Uniforme discreta
    k<-max(x)-min(x)+1
    probEspG<-rep(1/k,k+3)
    x11()
    truehist(x,h=1,
             main=paste("Comparación de distribuciones","\n",nomDist),
             xlab=tituloEjeX, ylab="Densidad",sub=nomVariable,
             xlim=c((min(x)-1),(max(x)+2)),
             ymax=max(max(probEspG),maxD)*1.2)
    lines((min(x)-1):(max(x)+2),probEspG,type="s",col="red",lty=2,lwd=2)
    legend("topleft",legend = c("Dist. original", "Dist. teórica"),
           lwd=c(1,2),lty=2,col=c("black","red"))
  }else if(distribucion==2) {  # Binomial
    n<-max(valores)
    if(identical(which(parametros$Nombre=="p"),integer(0))){
      media<-sum(valores*frecuencia)/sum(frecuencia)
      prob1<-media/n
    }else{
      prob1<-parametros$Valor[which(parametros$Nombre=="p")]
    }
    probEspG<-dbinom(0:(max(x)+1),size=n,prob=prob1)
    x11()
    truehist(x,h=1,main=paste("Comparación de distribuciones","\n",nomDist),
             xlab=tituloEjeX,ylab="Densidad",sub=nomVariable,
             xlim=c((min(x)-1),(max(valores)+2)),ymax=max(max(probEspG),maxD)*1.2)
    lines(0:(max(x)+2),c(probEspG,0),type="s",col="red",lty=2,lwd=2)
    legend("topleft",legend=c("Dist. original","Dist. teórica"),
           lwd=c(1,2),lty=2,col=c("black","red"))
  }else if(distribucion==3){  # Hipergeométrica
    estimarN<-identical(integer(0),which(parametros$Nombre=="N"))
    estimarn<-identical(integer(0),which(parametros$Nombre=="n"))
    estimark<-identical(integer(0),which(parametros$Nombre=="k"))
    if(estimarn){
      n<-max(valores)
    }else{
      n<-parametros$Valor[which(parametros$Nombre=="n")]
    }
    if(!estimarN | !estimark){
      if(estimark) {
        N<-parametros$Valor[which(parametros$Nombre=="N")]
        media<-sum(valores*frecuencia)/sum(frecuencia)
        k<-round(media*N/n,0)
      }else{
        k<-parametros$Valor[which(parametros$Nombre=="k")]
        if(estimarN) {
          media<-sum(valores*frecuencia)/sum(frecuencia)
          N<-floor(n*k/media)
        }else{
          N<-parametros$Valor[which(parametros$Nombre=="N")]
        }
      }
    }
    probEspG<-dhyper(0:(n+1),k,N-k,n)
    x11()
    truehist(x,h=1,main=paste("Comparación de distribuciones","\n",nomDist),
             xlab=tituloEjeX,ylab="Densidad",sub=nomVariable,
             xlim=c((min(x)-1),(max(valores)+2)),ymax=max(max(probEspG),maxD)*1.2)
    lines(0:(max(x)+2),c(probEspG,0),type="s",col="red",lty=2,lwd=2)
    legend("topleft",legend=c("Dist. original","Dist. teórica"),
           lwd=c(1,2),lty=2,col=c("black","red"))
  }else if(distribucion==4){  # Geométrica
    ajustes<-as.integer(identical(which(parametros$Nombre=="p"),integer(0)))
    n<-max(valores)
    if(ajustes){
      media<-sum(valores*frecuencia)/sum(frecuencia)
      prob1<-1/media
    }else{
      prob1<-parametros$Valor[which(parametros$Nombre=="p")]
    }
    probEspG<-dgeom(0:(n-2),prob1)
    probEspG<-c(probEspG, 1-sum(probEspG))
    x11()
    truehist(x,h=1,main=paste("Comparación de distribuciones","\n",nomDist),
             xlab=tituloEjeX,ylab="Densidad",sub=nomVariable,
             xlim=c((min(x)-1),(max(valores)+2)),ymax=max(max(probEspG),maxD)*1.2)
    lines(1:(max(x)+1),c(probEspG,0),type="s",col="red",lty=2,lwd=2)
    legend("topleft",legend=c("Dist. original","Dist. teórica"),
           lwd=c(1,2),lty=2,col=c("black","red"))
  }
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
  generaGrafica(valores,frecuencia,diccionario_parametros,
                distribucion,tituloEjeX,varInteres)
}
