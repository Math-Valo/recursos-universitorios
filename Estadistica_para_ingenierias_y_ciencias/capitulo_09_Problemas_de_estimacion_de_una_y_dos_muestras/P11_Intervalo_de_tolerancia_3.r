# P11_Intervalo_de_tolerancia_3.r
# Intervalo de tolerancia con resumen de datos.
########################################################
# Se calcula un intervalo de tolerancia, bilateral o 
# unilateral.
#
# Se están suponiendo que el resumen de los datos se conocen,
# y, por lo tanto, este programa no leerá datos de un archivo
# externo.
########################################################
########################################################
# Sección de cabezera (incluir paquetes, etc.)
########################################################

# Paquete requerido
library(tolerance)

########################################################
# Sección modificable por el usuario
########################################################

# Tamaño de la muestra
n<-15

# Media muestral
m<-3.84

# Desviación estándar (muestral o poblacional)
desv<-3.07

# Nivel de significancia
gamma<-0.05

# Valor alfa para el cual se cubre la proporción (1-alfa) de la población
alfa<-0.05

# Límite superior (S), inferior (I) o intervalo de dos colas (D)
inter<-'S'

########################################################
# Sección que realiza el procedimiento
########################################################

IT<-function(tamanyo,media,desviacion,alpha=0.05,P=0.99,lados='D'){
    if(lados=='D'){
        k<-K.factor(tamanyo,alpha=alpha,P=P,side=2,method=c("WBE"))
        LL<-media-k*desviacion
        LU<-media+k*desviacion
        Resultado<-data.frame(LL,media,LU)
        names(Resultado)[names(Resultado) == "LL"]<-"LimInf"
        names(Resultado)[names(Resultado) == "media"]<-"Media"
        names(Resultado)[names(Resultado) == "LU"]<-"LimSup"
    }
    else{
        k<-K.factor(tamanyo,alpha=alpha,P=P,side=1,method=c("WBE"))
        if(lados=='I'){
            LL<-media-k*desviacion
            Resultado<-data.frame(LL,media)
            names(Resultado)[names(Resultado) == "LL"]<-"LimInf"
            names(Resultado)[names(Resultado) == "media"]<-"Media"
        }
        else{
            LU<-media+k*desviacion
            Resultado<-data.frame(LU,media)
            names(Resultado)[names(Resultado) == "LL"]<-"LimInf"
            names(Resultado)[names(Resultado) == "media"]<-"Media"
        }
    }
    return(Resultado)
}

ITs<-IT(n,m,desv,alpha=gamma,P=1-alfa,lados=inter)

########################################################
# Muestra de los resultados
########################################################

ITs

