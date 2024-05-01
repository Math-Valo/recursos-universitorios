# P06_Prueba_de_dos_medias_02.r
# Contrasta la hipótesis para las medias de dos poblaciones
########################################################
# Se realizan los cálculos para una prueba de hipótesis de
# dos muestra para la diferencias de medias poblacionales,
# ya sean muestras independentes o dependientes (muestras
# pareadas), usando el P-valor y calculando la región de
# rechazo para un nivel de significancia predeterminado. Se
# realiza una prueba t, independientemente del tamaño de la
# muestra, por lo que se supondrá distribuciones normales
# poblacionales. El nivel de significancia es opcional.
# Asignado un nivel de significancia, en la salida se da un
# veredicto a la prueba.
#
# Se leerá de un archivo externo los datos necesarios y se
# calcularán los valores requeridos para el test.
########################################################
########################################################
# Sección modificable por el usuario
########################################################

# Lectura de la base de datos
datos<-read.csv("DB45_Problema_114.csv",sep=";",encoding="UTF-8")

# Selección de las variables de interés
varInteres<-c("Errores.PKLineas")

# Selección de variables con los niveles deseados de
# comparación
varSel<-c("País")

# Media de la prueba
mu<-0

# Desviaciones estándar desconocidas pero iguales
desv.iguales<-TRUE

# Nivel de significancia
alfa<-NULL

# Tipo de prueba: cola superior (S), cola inferior (I) o dos colas (D)
cola<-'D'

# ¿Son muestras pareadas?
par<-FALSE

########################################################
# Sección que realiza el procedimiento
########################################################

w<-data.frame(factor(datos[,varSel]))
names(w)<-varSel
varBin<-c(varSel)
datos<-data.frame(datos,w)

# Verificación de las variables de agrupacion binarias
# realmente lo son.
if (length(varBin)<1){
 stop("Debe al menos indicar una variable binaria")
}else{
 sonbinarios<-ifelse(length(table(datos[,varBin]))!=2,1,0)
}
if (sonbinarios!=0)  stop("La variable no es binaria")

valores<-unlist(datos[,c(varInteres)])
variables<-factor(rep(varInteres,each=dim(datos)[1]))

agrupaciones<-data.frame(datos[1:dim(datos)[1],varBin])
names(agrupaciones)<-varBin
datos2<-data.frame(agrupaciones,variable=variables,valor=valores)

# Función que recibe dos vectores (en realidad, lo recibe como LISTAS)
# y realiza el test de dos muestras para la diferencia de medias
difMedias<-function(l,mu,cola,alfa=0.05,iguales=NULL,pareadas=FALSE){
  x<-l[[1]][!is.na(l[[1]])]
  y<-l[[2]][!is.na(l[[2]])]
  n1<-length(x)
  n2<-length(y)
  m1<-mean(x)
  m2<-mean(y)
  sd1<-sd(x)
  sd2<-sd(y)
  if ( n1< 2 | n2 < 2){
     r<-data.frame(H0=mu,Poblaciones=NA,
                   n1=n1,n2=n2,
                   media1=m1,media2=m2,
                   diferencia=m1-m2,
                   alpha=alfa,
                   PValor=NA,
                   valorPMedia=NA,
                   valorPVar=NA,
                   varIgual=NA,
                   Estadistico=NA,
                   RegionRechazoZ=NA,
                   RegionRechazoX=NA)
  }else{
    if(!pareadas){
      r1<-var.test(x,y,conf.level=1-alfa)
      if(is.null(iguales)){
        varIgual<-(r1$p.value>=alfa)
      }else{
        varIgual<-iguales
      }
      r2<-t.test(x,y,mu=mu,alternative=cola,conf.level=1-alfa,var.equal=varIgual)
      grados<-as.numeric(r2$parameter)
      r<-data.frame(Poblaciones=TRUE,
                    H0=mu,
                    valorPVar=r1$p.value,
                    suposicionVar=varIgual,
                    n1=n1, n2=n2,
                    media1=m1, media2=m2,
                    diferencia=m1-m2,
                    desv.est1=sd1,
                    desv.est2=sd2)
      if(varIgual){
        r$est.sp=round(r2$stderr/sqrt(1/n1+1/n2),7)
        r$error.est=r2$stderr
        r$grados=grados
        r$alpha=alfa
        r$PValor=r2$p.value
        r$Estadistico=r2$statistic
        if(cola=="two.sided"){
          criticot<-round(qt(1-alfa/2,grados),7)
          criticox<-round(criticot*r2$stderr,7)
          r$RegionRechazoInfT<--criticot
          r$RegionRechazoSupT<-criticot
          r$RegionRechazoInfX<-mu-criticox
          r$RegionRechazoSupX<-mu+criticox
        }else{
          criticot<-round(qt(1-alfa,grados),7)
          criticox<-round(criticot*r2$stderr,7)
          if(cola=="less"){
            r$RegionRechazoInfT<--criticot
            r$RegionRechazoInfX<-mu-criticox
          }else{
            r$RegionRechazoSupT<-criticot
            r$RegionRechazoSupX<-mu+criticox
          }
        }
      }else{
        r$error.est=r2$stderr
        r$grados.libertad=round(grados,0)
        r$alpha=alfa
        r$PValor=r2$p.value
        r$Estadistico=r2$statistic
        if(cola=="two.sided"){
          criticot<-round(qt(1-alfa/2,grados),7)
          criticox<-round(criticot*r2$stderr,7)
          r$RegionRechazoInfT<--criticot
          r$RegionRechazoSupT<-criticot
          r$RegionRechazoInfX<-mu-criticox
          r$RegionRechazoSupX<-mu+criticox
        }else{
          criticot<-round(qt(1-alfa,grados),7)
          criticox<-round(criticot*r2$stderr,7)
          if(cola=="less"){
            r$RegionRechazoInfT<--criticot
            r$RegionRechazoInfX<-mu-criticox
          }else{
            r$RegionRechazoSupT<-criticot
            r$RegionRechazoSupX<-mu+criticox
          }
        }
      }
    }else{
      r1<-t.test(x,y,mu=mu,alternative=cola,conf.level=1-alfa,paired=TRUE)
      grados<-as.numeric(r1$parameter)
      r<-data.frame(Poblaciones=FALSE,
                    H0=mu,
                    n=n1,
                    diferencia=m1-m2,
                    desv.par=sd(x-y),
                    error.est=r1$stderr,
                    grados=grados,
                    alpha=alfa,
                    PValor=r1$p.value,
                    Estadistico=r1$statistic)
      if(cola=="two.sided"){
        criticot<-round(qt(1-alfa/2,grados),7)
        criticox<-round(criticot*r1$stderr,7)
        r$RegionRechazoInfT<--criticot
        r$RegionRechazoSupT<-criticot
        r$RegionRechazoInfX<-mu-criticox
        r$RegionRechazoSupX<-mu+criticox
      }else{
        criticot<-round(qt(1-alfa,grados),7)
        criticox<-round(criticot*r1$stderr,7)
        if(cola=="less"){
          r$RegionRechazoInfT<--criticot
          r$RegionRechazoInfX<-mu-criticox
        }else{
          r$RegionRechazoSupT<-criticot
          r$RegionRechazoSupX<-mu+criticox
        }
      }
    }
  }
  return(r)
}

listaG<-datos2[,c(varBin,"valor")]
listaG1<-split(listaG$"valor", listaG[,varBin])
if(as.matrix(listaG)[1]==names(listaG1[2])){
   listaG1=listaG1[c(2,1)]
}
listaG1<-list(listaG1)
names(listaG1)<-varInteres

if(cola=='I'){
   cola<-"less"
}else{
   if(cola=='S'){
      cola<-"greater"
   }
   else{
      cola<-"two.sided"
   }
}

if(is.null(alfa)){
   rFin<-t(sapply(listaG1,difMedias,mu,cola,iguales=desv.iguales,pareadas=par))
}else{
   rFin<-t(sapply(listaG1,difMedias,mu,cola,alfa=alfa,iguales=desv.iguales,pareadas=par))
}

t1<-as.data.frame.table(table(datos2[,c("variable")]))
identif<-t1[t1$Freq>0,]
d<-dim(rFin)
n<-colnames(rFin)

rFin<-data.frame(matrix(unlist(rFin),d))
names(rFin)<-n

rFin<-data.frame(identif,rFin)

if(rFin$Poblaciones){
   rFin<-rFin[with(rFin,n1!=0 | n2!=0),]

   rFin$Poblaciones<-"Independientes"
   if(is.null(alfa)){
     rFin$suposicionVar<-ifelse(rFin$suposicionVar>=0.05,"Var no diferentes","Var diferentes")
   }else{
     rFin$suposicionVar<-ifelse(rFin$suposicionVar>=alfa,"Var no diferentes","Var diferentes")
   }
}else{
   rFin<-rFin[with(rFin,n!=0),]
   rFin$Freq=rFin$Freq/2

   rFin$Poblaciones<-"Pareadas"
}

if(!is.null(alfa)){
   rFin$Resultado<-ifelse(rFin$PValor>=alfa,
                          "No se rechaza H0","Se rechaza H0")
   rFin$Resultado[is.na(rFin$Resultado)]<-"Pocos datos"
}


########################################################
# Muestra de los resultados
########################################################

rFin

