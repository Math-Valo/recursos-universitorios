# P09_Intervalo_de_prediccion_2.r
# Intervalo de predicción bilateral de una población normal.
########################################################
# Se calcula un intervalo de predicción bilateral, usando la
# media y desviación estándar muestrales, de una población
# que se distribuye de forma normal con (1-alpha)100% de
# seguridad. El script hace uso un método de la biblioteca
# EnvStats, el cual usa el valor crítico de la distribución 
# t con la desviación estándar muestral, independientemente
# del tamaño de la muestra.
#
# Se leerá de un archivo externo los datos necesarios y se
# calcularán los valores requeridos para obtener el intervalo
# de tolerancia.
########################################################
########################################################
# Sección de cabezera (incluir paquetes, etc.)
########################################################

# Paquete requerido
library(EnvStats)

########################################################
# Sección modificable por el usuario
########################################################

# Lectura de la base de datos
datos<-read.csv("DB02_Problema_18.csv",sep=";",encoding="UTF-8")

# Selección de las variables de interés
varInteres<-c("Tiempo.h")

# Nivel de significancia
alfa<-0.05

########################################################
# Sección que realiza el procedimiento
########################################################

valores<-unlist(datos[,varInteres])
variable<-factor(rep(varInteres,each=dim(datos)[1]))

# Función que devuelve sólo el intervalo de tolerancia
# De acuerdo a las especifiaciones dadas.
IP<-function(x,sup=TRUE,alfa=0.05){
   if (length(x[!is.na(x)])>=2){
      RespGen<-predIntNorm(x[!is.na(x)],conf.level=alfa,pi.type="two-side",method="Bonferroni")
      v<-ifelse(sup,round(RespGen$interval$limits[[2]],7),round(RespGen$interval$limits[[1]],7))
      return(v)
   } else return(NA)
}

media<-as.data.frame.table(tapply(valores,as.list(data.frame(variable)),mean,na.rm=TRUE),responseName="Media")
IPs1<-as.data.frame.table(tapply(valores,as.list(data.frame(variable)),IP,alfa=1-alfa),responseName="LimSup")
IPs2<-as.data.frame.table(tapply(valores,as.list(data.frame(variable)),IP,sup=FALSE,alfa=1-alfa),responseName="LimInf")

IPs<-na.omit(data.frame(IPs2,Media=media[,"Media"],LimSup=IPs1[,"LimSup"]))

########################################################
# Muestra de los resultados
########################################################

IPs

