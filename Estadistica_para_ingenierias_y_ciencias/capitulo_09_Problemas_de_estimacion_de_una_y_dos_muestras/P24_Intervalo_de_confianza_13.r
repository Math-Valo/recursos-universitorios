# P24_Intervalo_de_confianza_13.r
# Intervalo de confianza de la razón de varianzas
# de poblaciones normales y prueba de homogeneidad
# de varianza entre poblaciones normales
########################################################
# Se calcula un intervalo de confianza para la proporción de
# varianzas o de desviaciones estándar, usando la proporción
# de varianzas o de desviaciones estándar muestrales, para
# poblaciones con districión normal, o muestras que sean
# suficientemente grande. El intervalo puede ser unilateral
# o bilateral.
#
# Se leerá de un archivo externo los datos necesarios y se
# calcularán los valores requeridos para obtener el intervalo
# de confianza.
########################################################
########################################################
# Sección modificable por el usuario
########################################################
# Lectura de la base de datos
datos<-read.csv("DB06_Problema_46.csv",sep=";",encoding="UTF-8")

# Selección de las variables de interés
varInteres<-c("Tiempo.min")

# Selección de variables con los niveles deseados de
# comparación.
# Si no se colocan niveles de comparación se supone
# que la variable es binaria.
varSel<-c("Compañía")

# Indique el nivel de significancia
alfa<-0.1

########################################################
# Sección que realiza el procedimiento
########################################################

# Creación de nuevas variables con los niveles propuestos.
w<-data.frame(row.names=1:dim(datos)[1])
varBin<-as.character()

x1<-data.frame(factor(datos[,varSel]))
names(x1)<-varSel
varBin<-c(varBin,varSel)
w<-data.frame(w,x1)

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

# Función que recibe dos vectores y calcula el intervalo
# de confianza para razón de varianzas y
# muestra si hay o no diferencia estadística

razonVar<-function(l,alfa=0.05){
 if(length(l)>1){
   x<-l[[1]][!is.na(l[[1]])]
   y<-l[[2]][!is.na(l[[2]])]
   n1<-length(x)
   n2<-length(y)
   if ( n1< 2 | n2 < 2){
     r<-data.frame(n1=n1,n2=n2,
                   desv.Est1=NA,desv.Est2=NA,
                   limInf=NA,razon=NA,limSup=NA,
                   valorPVar=NA)
   }else{ 
   ds1<-sd(x)
   ds2<-sd(y)
   r1<-var.test(x,y,conf.level=1-alfa)
   r<-data.frame(n1=n1,n2=n2,
                 desv.Est1=ds1,desv.Est2=ds2,
                 limInf=r1$conf.int[1],
                 razon=r1$estimate,
                 limSup=r1$conf.int[2],
                 valorPVar=r1$p.value)
   }
 }else{
   r<-data.frame(n1=0,n2=0,
                 desv.Est1=NA,desv.Est2=NA,
                 limInf=NA,razon=NA,limSup=NA,
                 valorP=NA)
 }
 return(r)
}


listaG<-datos2[,c(varBin,"valor")]
listaG1<-list(split(listaG$"valor", listaG[,varBin]))
names(listaG1)<-varInteres
rFin<-t(sapply(listaG1,razonVar,alfa=alfa))

t1<-as.data.frame.table(table(datos2[,c("variable")]))
identif<-t1[t1$Freq>0,]
d<-dim(rFin)
n<-colnames(rFin)

rFin<-data.frame(matrix(unlist(rFin),d))
names(rFin)<-n

rFin<-data.frame(identif,rFin)
rFin<-rFin[with(rFin,n1!=0 | n2!=0),]
rFin$Resultado<-ifelse(rFin$valorPVar>=alfa,
                       "Var no diferentes",
                       "Var diferentes")
rFin$Resultado[is.na(rFin$Resultado)]<-"Pocos datos"


########################################################
# Sección que muestra los resultados
########################################################

rFin

