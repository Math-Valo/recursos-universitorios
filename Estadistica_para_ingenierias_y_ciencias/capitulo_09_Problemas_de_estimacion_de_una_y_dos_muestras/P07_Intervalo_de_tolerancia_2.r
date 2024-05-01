# P07_Intervalo_de_tolerancia_2.r
# Intervalo de tolerancia bilateral de una población normal.
########################################################
# Se calcula un intervalo de tolerancia, usando la media y
# desviación estándar muestrales, de una población que se
# distribuye de forma normal con (1-alpha)100% de seguridad
# que contiene P proporción de la población.
#
# Se leerá de un archivo externo los datos necesarios y se
# calcularán los valores requeridos para obtener el intervalo
# de tolerancia.
########################################################
########################################################
# Sección de cabezera (incluir paquetes, etc.)
########################################################

# Paquete requerido
library(tolerance)

########################################################
# Sección modificable por el usuario
########################################################

# Lectura de la base de datos
datos<-read.csv("DB02_Problema_18.csv",sep=";",encoding="UTF-8")

# Selección de las variables de interés
varInteres<-c("Tiempo.h")

# Indique el nivel de significancia
gamma<-0.01

# Nivel de proporción (proporción := (1-alfa)100%)
alfa<-0.05

########################################################
# Sección que realiza el procedimiento
########################################################

valores<-unlist(datos[,varInteres])
variable<-factor(rep(varInteres,each=dim(datos)[1]))

# Función que devuelve sólo el intervalo de tolerancia
# De acuerdo a las especifiaciones dadas.
IT<-function(x,sup=TRUE,alfa=0.05,P=0.99){
   if (length(x[!is.na(x)])>=2){
      pruebaT<-normtol.int(x[!is.na(x)],alpha=alfa,P=P,side=2,method="WBE")
      v<-ifelse(sup,round(pruebaT[,5],7),round(pruebaT[,4],7))
      return(v)
   } else return(NA)
}

media<-as.data.frame.table(tapply(valores,as.list(data.frame(variable)),mean,na.rm=TRUE),responseName="Media")
ITs1<-as.data.frame.table(tapply(valores,as.list(data.frame(variable)),IT,alfa=gamma,P=1-alfa),responseName="LimSup")
ITs2<-as.data.frame.table(tapply(valores,as.list(data.frame(variable)),IT,sup=FALSE,alfa=gamma,P=1-alfa),responseName="LimInf")

ITs<-na.omit(data.frame(ITs2,Media=media[,"Media"],LimSup=ITs1[,"LimSup"]))

########################################################
# Muestra de los resultados
########################################################

ITs

