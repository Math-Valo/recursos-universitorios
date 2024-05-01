# P04_Prueba_de_un_media_02.r
# Contrasta la hipótesis para la media de una población
########################################################
# Se realizan los cálculos para una prueba de hipótesis de
# una muestra para la media poblacional usando el P-valor y
# calculando la región de rechazo. Según los datos (tamaño
# de muestra y dar la desviación poblacional) se realiza una
# prueba normal o una prueba t. El nivel de significancia es
# opcional para una prueba con el P-valor, y en dado caso se
# usa un valor predeterminado para la región de rechazo, y,
# si se asigna un nivel de significancia, la salida da un
# veredicto a la prueba.
#
# Se leerá de un archivo externo los datos necesarios y se
# calcularán los valores requeridos para el test.
########################################################
########################################################
# Sección de cabezera (incluir paquetes, etc.)
########################################################

# Paquete requerido
library(TeachingDemos)

########################################################
# Sección modificable por el usuario
########################################################

# Lectura de la base de datos
datos<-read.csv("DB01_Problema_025.csv",sep=";",encoding="UTF-8")

# Selección de las variables de interés
varInteres<-c("Contenido.l")

# Media de la prueba
mu<-10

# Nivel de significancia
alfa<-0.01

# Tipo de prueba: cola superior (S), cola inferior (I) o dos colas (D)
cola<-'D'

# Valor de la desviación estándar poblacional
desv.pobl<-NULL

########################################################
# Sección que realiza el procedimiento
########################################################

valores<-unlist(datos[,varInteres])
variable<-factor(rep(varInteres,each=dim(datos)[1]))

TestMedia<-function(x,mu,alfa=0.05,colas='D',desv.pobl=NULL){
   n<-length(x[!is.na(x)])
   if(length(x[!is.na(x)])<=2){
      r<-data.frame(Prueba=NA,
                    H0=mu,
                    n=n,
                    MediaMuestral=mean(x[!is.na(x)]),
                    desv.est=NA,
                    error.est=NA,
                    alpha=alfa,
                    PValor=NA,
                    estadístico=NA,
                    RegionRechazoZ=NA,
                    RegionRechazoX=NA)
   }else{
      if(is.null(desv.pobl) & length(x[!is.na(x)])>=30){
         desv.pobl=sd(x)
      }
      if(is.null(desv.pobl)){
         if(colas=='D'){
            prueba<-t.test(x[!is.na(x)],mu=mu,conf.level=1-alfa)
            m<-as.numeric(prueba$estimate)
            error<-sd(x[!is.na(x)])/sqrt(n)
            criticot<-round((prueba$conf.int[2]-m)/error,7)
            criticox<-round(prueba$conf.int[2]-m,7)
            r<-data.frame(Prueba="t",
                          H0=mu,
                          n=n,
                          MediaMuestral=m,
                          desv.est=sd(x[!is.na(x)]),
                          error.est=error,
                          alpha=alfa,
                          PValor=prueba$p.value,
                          estadístico=round(as.numeric(prueba$statistic),7),
                          RegionRechazoT=paste("<",-criticot,7,
                                               " y >",criticot,7),
                          RegionRechazoX=paste("<",mu-criticox,7,
                                               " y >",mu+criticox))
         }else{
            if(colas=='I'){
               prueba<-t.test(x[!is.na(x)],mu=mu,conf.level=1-alfa,
                              alternative="less")
               m<-as.numeric(prueba$estimate)
               error<-sd(x[!is.na(x)])/sqrt(n)
               criticot<-round(qt(1-alfa,n-1),7)
               # criticot<-round((prueba$conf.int[1]-m)/error,7)
               criticox<-round(criticot*error,7)
               # criticox<-round(prueba$conf.int[1]-m,7)
               r<-data.frame(Prueba="t",
                             H0=mu,
                             n=n,
                             MediaMuestral=m,
                             desv.est=sd(x[!is.na(x)]),
                             error.est=error,
                             alpha=alfa,
                             PValor=prueba$p.value,
                             estadístico=round(as.numeric(prueba$statistic),7),
                             RegionRechazoT=paste("<",-criticot),
                             RegionRechazoX=paste("<",mu-criticox))
            }else{
               prueba<-t.test(x[!is.na(x)],mu=mu,conf.level=1-alfa,
                              alternative="greater")
               m<-as.numeric(prueba$estimate)
               error<-sd(x[!is.na(x)])/sqrt(n)
               criticot<-round(qt(1-alfa,n-1),7)
               # criticot<-round((prueba$conf.int[2]-m)/error,7)
               criticox<-round(criticot*error,7)
               # criticox<-round(prueba$conf.int[2]-m,7)
               r<-data.frame(Prueba="t",
                             H0=mu,
                             n=n,
                             MediaMuestral=m,
                             desv.est=sd(x[!is.na(x)]),
                             error.est=error,
                             alpha=alfa,
                             PValor=prueba$p.value,
                             estadístico=round(as.numeric(prueba$statistic),7),
                             RegionRechazoT=paste(">",criticot),
                             RegionRechazoX=paste(">",mu+criticox))
            }
         }
      }else{
         if(colas=='D'){
            prueba<-z.test(x[!is.na(x)],sd=desv.pobl,mu=mu,conf.level=1-alfa)
            m<-as.numeric(prueba$estimate)
            error<-sd(x[!is.na(x)])/sqrt(n)
            criticoz<-round((prueba$conf.int[2]-m)/error,7)
            criticox<-round(prueba$conf.int[2]-m,7)
            r<-data.frame(Prueba="Z",
                          H0=mu,
                          n=n,
                          MediaMuestral=m,
                          desv.est=sd(x[!is.na(x)]),
                          error.est=error,
                          alpha=alfa,
                          PValor=prueba$p.value,
                          estadístico=round(as.numeric(prueba$statistic),7),
                          RegionRechazoZ=paste("<",-criticoz,
                                               " y >",criticoz),
                          RegionRechazoX=paste("<",mu-criticox,
                                               " y >",mu+criticox))
         }else{
            if(colas=='I'){
               prueba<-z.test(x[!is.na(x)],sd=desv.pobl,mu=mu,conf.level=1-alfa,
                              alternative="less")
               m<-as.numeric(prueba$estimate)
               error<-sd(x[!is.na(x)])/sqrt(n)
               criticoz<-round(qnorm(1-alfa),7)
               criticox<-round(criticoz*error,7)
               r<-data.frame(Prueba="Z",
                             H0=mu,
                             n=n,
                             MediaMuestral=m,
                             desv.est=sd(x[!is.na(x)]),
                             error.est=error,
                             alpha=alfa,
                             PValor=prueba$p.value,
                             estadístico=round(as.numeric(prueba$statistic),7),
                             RegionRechazoZ=paste("<",-criticoz),
                             RegionRechazoX=paste("<",mu-criticox))
            }else{
               prueba<-z.test(x[!is.na(x)],sd=desv.pobl,mu=mu,conf.level=1-alfa,
                              alternative="greater")
               m<-as.numeric(prueba$estimate)
               error<-sd(x[!is.na(x)])/sqrt(n)
               criticoz<-round(qnorm(1-alfa),7)
               criticox<-round(criticoz*error,7)
               r<-data.frame(Prueba="Z",
                             H0=mu,
                             n=n,
                             MediaMuestral=m,
                             desv.est=sd(x[!is.na(x)]),
                             error.est=error,
                             alpha=alfa,
                             PValor=prueba$p.value,
                             estadístico=round(as.numeric(prueba$statistic),7),
                             RegionRechazoZ=paste(">",criticoz),
                             RegionRechazoX=paste(">",mu+criticox))
            }
         }
      }
   }
   return(r)
}

if(is.null(alfa)){
    Test<-TestMedia(valores,mu,colas=cola,desv.pobl=desv.pobl)
}else{
    Test<-TestMedia(valores,mu,alfa=alfa,colas=cola,desv.pobl=desv.pobl)
    resultado<-ifelse(Test[,"PValor"]>=alfa,
                      "No se rechaza H0","Se rechaza H0")
    Test$Resultado<-resultado
    Test$Resultado[is.na(Test$Resultado)]<-"Pocos datos"
}

identif<-data.frame(varInteres)
names(identif)<-"var"

Test<-data.frame(identif,Test)

########################################################
# Muestra de los resultados
########################################################

Test

