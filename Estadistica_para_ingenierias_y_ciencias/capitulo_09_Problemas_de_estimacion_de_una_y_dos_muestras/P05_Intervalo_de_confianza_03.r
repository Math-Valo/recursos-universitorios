# P05_Intervalo_de_confianza_03.r
# Intervalo de confianza de la media de una población con desviación desconocida.
########################################################
# Se calcula un intervalo de confianza para le media poblacional,
# usando la media muestral con una muestra pequeña, de una 
# población que se distribuye de forma normal en donde se 
# desconoce la desviación estándar poblacional.
#
# Se leerá de un archivo externo los datos necesarios y se
# calcularán los valores requeridos para obtener el intervalo
# de confianza.
########################################################
########################################################
# Sección modificable por el usuario
########################################################

# Lectura de la base de datos
datos<-read.csv("DB01_Problema_13.csv",sep=";",encoding="UTF-8")

# Selección de las variables de interés
varInteres<-c("Diámetro.cm")

# Indique el nivel de significancia
alfa<-0.01

########################################################
# Sección que realiza el procedimiento
########################################################

valores<-unlist(datos[,varInteres])
variable<-factor(rep(varInteres,each=dim(datos)[1]))

# Función que devuelve sólo el intervalo de confianza
# De acuerdo a las especifiaciones dadas.
IC<-function(x,sup=TRUE,alfa=0.05){
   if (length(x[!is.na(x)])>=2){
      pruebaT<-t.test(x[!is.na(x)],conf.level=1-alfa)
      v<-ifelse(sup,round(pruebaT$conf.int[2],7),round(pruebaT$conf.int[1],7))
      return(v)
   } else return(NA)
}

media<-as.data.frame.table(tapply(valores,as.list(data.frame(variable)),mean,na.rm=TRUE),responseName="Media")
ICs1<-as.data.frame.table(tapply(valores,as.list(data.frame(variable)),IC,alfa=alfa),responseName="LimSup")
ICs2<-as.data.frame.table(tapply(valores,as.list(data.frame(variable)),IC,sup=FALSE,alfa=alfa),responseName="LimInf")

ICs<-na.omit(data.frame(ICs2,Media=media[,"Media"],LimSup=ICs1[,"LimSup"]))

########################################################
# Muestra de los resultados
########################################################

ICs

