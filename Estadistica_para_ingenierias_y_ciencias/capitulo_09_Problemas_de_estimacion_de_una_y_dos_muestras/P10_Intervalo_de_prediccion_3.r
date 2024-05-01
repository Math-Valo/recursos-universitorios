# P10_Intervalo_de_prediccion_3.r
# Intervalo de predicción un elemento con resumen de datos.
########################################################
# Se calcula un intervalo de predicción, bilateral o 
# unilateral, el script permite poner la opción de si se
# conoce o no la desviación estándar poblacional, calculando
# según sea el caso con el valor crítico de una distribución
# normal o distribución t, respectivamente. En caso de que
# la muestra sea mayor a 30 elementos, por defecto se usará
# la desviación dada como si fuese la poblacional.
#
# Se están suponiendo que el resumen de los datos se conocen,
# y, por lo tanto, este programa no leerá datos de un archivo
# externo.
########################################################
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
alfa<-0.05

# ¿Se conoce la desviación poblacional?
val<-FALSE

# Límite superior (S), inferior (I) o intervalo de dos colas (D)
inter<-'S'

########################################################
# Sección que realiza el procedimiento
########################################################

# La desviación se considerará poblacional si el tamaño de muestra es grande
if (n >= 30)
{
    val<-TRUE
}

IP<-function(tamanyo, media, desviacion, alpha=0.05, pob=FALSE, colas = 'D'){
    if(colas == 'D'){
        k<-ifelse(pob,round(qnorm(1-alfa/2),7),round(qt(1-alfa/2,n-1),7))
        LL<-m-k*desv*sqrt(1+1/n)
        LU<-m+k*desv*sqrt(1+1/n)
        Resultado<-data.frame(LL,media,LU)
        names(Resultado)[names(Resultado) == "LL"]<-"LimInf"
        names(Resultado)[names(Resultado) == "media"]<-"Media"
        names(Resultado)[names(Resultado) == "LU"]<-"LimSup"
    }
    else{
        k<-ifelse(pob,round(qnorm(1-alfa),7),round(qt(1-alfa,n-1),7))
        if (colas == 'I'){
            LL<-m-k*desv*sqrt(1+1/n)
            Resultado<-data.frame(LL,media)
            names(Resultado)[names(Resultado) == "LL"]<-"LimInf"
            names(Resultado)[names(Resultado) == "media"]<-"Media"
        }
        else{
            LU<-m+k*desv*sqrt(1+1/n)
            Resultado<-data.frame(media,LU)
            names(Resultado)[names(Resultado) == "media"]<-"Media"
            names(Resultado)[names(Resultado) == "LU"]<-"LimSup"
        }
    }
    return(Resultado)
}

IPs<-IP(n,m,desv,alfa,val,inter)

########################################################
# Muestra de los resultados
########################################################

IPs

