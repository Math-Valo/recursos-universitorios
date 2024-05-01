# P16_Intervalo_de_confianza_07.r
# Intervalo de pareos con poblaciones normales.
########################################################
# Se calcula un intervalo de confianza bilateral para la
# diferencia de medias poblacionales normales, cuando las
# poblaciones están pareadas, con (1-alpha)100% de seguridad.
# Se hace uso del estadístico t, independientemente de si se
# conoce o no las desviaciones poblacionales o de si las
# muestras son lo suficientemente grande, por lo que se
# supone que las poblaciones se distribuyen de forma
# aproximadamente normal.
#
# Se leerá de un archivo externo los datos necesarios y se
# calcularán los valores requeridos para obtener el intervalo
# de confianza.
########################################################
########################################################
# Sección modificable por el usuario
########################################################

# Lectura de la base de datos
datos<-read.csv("DB04_Problema_44.csv",sep=";",encoding="UTF-8")

# Selección de las variables de interés
varInteres<-c("Distancia.km")

# Selección de variable con la que se distingue el factor de uno
# y otro factor en el apareo
varSel<-list("Marca")

# Indique el nivel de significancia
alfa<-0.01

########################################################
# Sección que realiza el procedimiento
########################################################

valores<-datos[0:(dim(datos)[1]/2),varInteres]-datos[(dim(datos)[1]/2+1):dim(datos)[1],varInteres]
variable<-factor(rep(varInteres,each=dim(datos)[1]/2))

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
