# P21_Intervalo_de_confianza_10.r
# Intervalo de confianza de la varianza y desviaci�n tipica
########################################################
# Se calcula un intervalo de confianza para la varianza o la
# desviaci�n t�pica, usando la varianza o la desviaci�n
# est�ndar muestral, suponiendo que la poblaci�n tiene una
# districi�n normal, o que la muestra es lo suficientemente
# grande. El intervalo puede ser unilateral o bilateral.
#
# Se leer� de un archivo externo los datos necesarios y se
# calcular�n los valores requeridos para obtener el intervalo
# de confianza.
########################################################
########################################################
# Secci�n modificable por el usuario
########################################################
# Lectura de la base de datos
datos<-read.csv("DB09_Problema_71.csv",sep=";",encoding="UTF-8")

# Selecci�n de las variables de inter�s
varInteres<-c("Tiempo.a�os")

# Indique el nivel de significancia
alfa<-0.05

# Intervalo de confianza para varianza o desviacion tipica?
tipoInterv<-"var"
# tipoInterv<-"desv.est"

########################################################
# Secci�n que realiza el procedimiento
########################################################

valores<-unlist(datos[,varInteres])
variable<-factor(rep(varInteres,each=dim(datos)[1]))

ic.var.ds<-function(x,alfa=0.05,tipo="desv.est",sup=TRUE){
  x<-x[!is.na(x)]
  if (length(x)>=2){
      n<-length(x)
      var1<-var(x)
      v<-ifelse(sup,(n-1)*var1/(qchisq(alfa/2,n-1)),(n-1)*var1/(qchisq(1-alfa/2,n-1)))
      v<-ifelse(toupper(tipo)=="DESV.EST",sqrt(v),v)
      return(v)
  }else return(NA)
}

if(toupper(tipoInterv)=="DESV.EST"){
   var1<-as.data.frame.table(tapply(valores,as.list(data.frame(variable)),sd,na.rm=TRUE),responseName="DesviacionEstandar")
}else{
   var1<-as.data.frame.table(tapply(valores,as.list(data.frame(variable)),var,na.rm=TRUE),responseName="Varianza") 
}
ICs1<-as.data.frame.table(tapply(valores,as.list(data.frame(variable)),ic.var.ds,alfa=alfa,tipo=tipoInterv),responseName="LimSup")
ICs2<-as.data.frame.table(tapply(valores,as.list(data.frame(variable)),ic.var.ds,sup=FALSE,alfa=alfa,tipo=tipoInterv),responseName="LimInf")

if(toupper(tipoInterv)=="DESV.EST"){
   ICs<-na.omit(data.frame(ICs2,DesviacionEstandar=var1[,"DesviacionEstandar"],LimSup=ICs1[,"LimSup"]))
}else{
   ICs<-na.omit(data.frame(ICs2,Varianza=var1[,"Varianza"],LimSup=ICs1[,"LimSup"]))
}

########################################################
# Secci�n que muestra los resultados
########################################################

ICs


