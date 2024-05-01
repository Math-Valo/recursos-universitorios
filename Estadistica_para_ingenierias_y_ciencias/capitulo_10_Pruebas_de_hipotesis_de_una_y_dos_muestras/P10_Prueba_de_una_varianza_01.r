# P10_Prueba_de_una_varianza_01.r
# Contrasta las hipótesis para la varianza de una población
########################################################
# Se realizan los cálculos para una prueba de hipótesis de
# una muestra para la varianza poblacional basado en un
# estadístico con distribución ji cuadrada, suponiendo que
# la población se distribuye normalmente, usando el P-valor
# y calculando la región de rechazo. El valor del nivel de
# significancia es opcional para una prueba con el P-valor;
# en caso de no indicarlo, se usa un valor predeterminado
# en el nivel de significancia para la región de rechazo y,
# si se asigna un nivel de significancia, la salida da un
# veredicto a la prueba.
#
# Se leerá de un archivo externo los datos necesarios y se
# calcularán los valores para el resultado de la prueba de
# hipótesis.
########################################################
########################################################
# Sección modificable por el usuario
########################################################

# Lectura de la base de datos
datos<-read.csv("DB01_Problema_025.csv",sep=";",encoding="UTF-8")

# Selección de la variable de interés
varInteres<-c("Contenido.l")

# Varianza de la prueba
var<-0.03

# Nivel de significancia
alfa<-NULL

# Tipo de prueba: cola superior (S), cola inferior (I) o dos colas (D)
cola<-'D'

########################################################
# Sección que realiza el procedimiento
########################################################

valores<-unlist(datos[,varInteres])
variable<-factor(rep(varInteres,each=dim(datos)[1]))

TestVar<-function(x,varP,alfa=0.05,colas='D'){
  x<-x[!is.na(x)]
  n<-length(x)
  r<-data.frame(n=n,
                H0=varP)
  if(n<2){
    r$var.muestral<-NA
    r$grados<-n-1
    r$error.est<-NA
    r$alpha<-alfa
    r$Pvalor<-NA
    r$estadistico<-NA
    r$RegionRechazoJi<-NA
    r$RegionRechazoX<-NA
  }else{
    var<-var(x)
    v<-n-1
    error<-varP/v
    estadistico<-var*v/varP
    r$var.muestral<-round(var,7)
    r$grados<-v
    r$error.est<-round(error,7)
    r$alpha<-alfa
    if(colas=='I'){
      criticojiL<-qchisq(alfa,v)
      criticoXL<-criticojiL*error
      r$PValor<-round(pchisq(estadistico,v),7)
      r$Estadistico<-estadistico
      r$RegionRechazoJi<-paste("<",round(criticojiL,7))
      r$RegionRechazoX<-paste("<",round(criticoXL,7))
    }else{
      if (colas=='S') {
        criticojiU<-qchisq(alfa,v,lower.tail=F)
        criticoXU<-criticojiU*error
        r$PValor<-round(pchisq(estadistico,v,lower.tail=F),7)
        r$Estadistico<-estadistico
        r$RegionRechazoJi<-paste(">",round(criticojiU,7))
        r$RegionRechazoX<-paste(">",round(criticoXU,7))
      }else{
        criticojiU<-round(qchisq(alfa/2,v,lower.tail=F),7)
        criticoXU<-round(criticojiU*error,7)
        criticojiL<-round(qchisq(alfa/2,v),7)
        criticoXL<-round(criticojiL*error,7)
        pvalor<-round(pchisq(estadistico,v),7)
        r$Pvalor<-ifelse(var<varP,2*pvalor,2*(1-pvalor))
        r$Estadistico<-estadistico
        r$RegionRechazoJi<-paste("<",criticojiL,7,"y >",criticojiU,7)
        r$RegionRechazoX<-paste("<",criticoXL,7,"y >",criticoXU,7)
      }
    }
    return(r)
  }
}
if(is.null(alfa)){
  Test<-TestVar(valores,var,colas=cola)
}else{
  Test<-TestVar(valores,var,alfa=alfa,colas=cola)
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