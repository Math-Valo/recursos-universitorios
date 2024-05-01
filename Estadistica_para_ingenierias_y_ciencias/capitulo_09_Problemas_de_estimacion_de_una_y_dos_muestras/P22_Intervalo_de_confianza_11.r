# P22_Intervalo_de_confianza_11.r
# Intervalo de confianza de la varianza y desviaci�n tipica
########################################################
# Se calcula un intervalo de confianza para la varianza o la
# desviaci�n t�pica, usando la varianza o la desviaci�n
# est�ndar muestral, suponiendo que la poblaci�n tiene una
# districi�n normal, o que la muestra es lo suficientemente
# grande. El intervalo puede ser unilateral o bilateral.
#
# Se est�n suponiendo que el resumen de los datos se conocen,
# y, por lo tanto, este programa no leer� datos de un archivo
# externo.
########################################################
########################################################
# Secci�n modificable por el usuario
########################################################

# Tama�o de la muestra
n<-20

# valor del estimador (varianza o desviaci�n muestral)
var<-16
desv.est<-NULL

# Nivel de significancia
alfa<-0.02

# Intervalo de confianza para varianza o desviacion tipica?
tipoInterv<-"var"
# tipoInterv<-"desv.est"

# L�mite superior (S), inferior (I) o intervalo de dos colas (D)
inter<-'D'

# �La poblaci�n se distribuye de forma normal?
val<-TRUE

########################################################
# Secci�n que realiza el procedimiento
########################################################

# Si no se puede suponer que la poblaci�n se distribuye normalmente o
# si la muestra es peque�a, entonces no hay validez en el intervalo 
# de confianza y se detiene el proceso.
if(!val & (n < 30)){
    stop("Se requiere una poblaci�n normal o una muestra lo suficientemente grande")
}

if(is.null(var)){
    var<-desv.est^2
}

# Funci�n que recibe los datos de entrada y calcula el
# intervalo de confianza para la diferencia de medias.
# Puede calcular intervalos bilaterales o unilaterales.
difMedias<-function(n,var,alfa=0.05,colas='D',tipoInter="var"){
    estimador<-ifelse(tipoInter=="var",round(var,7),round(sqrt(var),7))
    if(n<2){
        r<-data.frame(Estimando=tipoInterv,n=n,
                      LimInf=NA,
                      estimador=estimador,
                      LimSup=NA)
    }else{
        aux<-(n-1)*var
        if(colas=='D'){
            jicuadL<-qchisq(alfa/2,n-1,lower.tail=F)
            jicuadU<-qchisq(1-alfa/2,n-1,lower.tail=F)
            LL<-round(ifelse(tipoInter=="var",aux/jicuadL,sqrt(aux/jicuadL)),7)
            LU<-round(ifelse(tipoInter=="var",aux/jicuadU,sqrt(aux/jicuadU)),7)
            r<-data.frame(Estimando=tipoInterv,n=n,
                          LimInf=LL,
                          estimador=estimador,
                          LimSup=LU)
        }else{
            if(colas=='I'){
                jicuadL<-qchisq(alfa,n-1,lower.tail=F)
                LL<-round(ifelse(tipoInter=="var",aux/jicuadL,sqrt(aux/jicuadL)),7)
                r<-data.frame(Estimando=tipoInterv,n=n,
                              LimInf=LL,
                              estimador=estimador)
            }else{
                jicuadU<-qchisq(1-alfa,n-1,lower.tail=F)
                LU<-round(ifelse(tipoInter=="var",aux/jicuadU,sqrt(aux/jicuadU)),7)
                r<-data.frame(Estimando=tipoInterv,n=n,
                              estimador=var,
                              LimSup=LU)
            }
        }
    }
    return(r)
}

ICs<-difMedias(n,var,alfa=alfa,colas=inter,tipoInter=tipoInterv)

########################################################
# Secci�n que muestra los resultados
########################################################

ICs


