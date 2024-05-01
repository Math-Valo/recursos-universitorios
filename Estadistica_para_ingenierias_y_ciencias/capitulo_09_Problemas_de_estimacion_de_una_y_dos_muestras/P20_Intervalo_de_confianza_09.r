# P13_Intervalo_de_confianza_04.r
# Intervalo de la diferencia de medias de poblaciones
########################################################
# Se calcula un intervalo de confianza para la diferencia de
# medias poblacionales, usando la diferencia de las medias
# muestrales, conociendo las desviaciones estándar de la
# población o si las muestras son lo suficientemente grandes.
# El intervalo puede ser unilateral o bilateral.
#
# Se están suponiendo que el resumen de los datos se conocen,
# y, por lo tanto, este programa no leerá datos de un archivo
# externo.
########################################################
########################################################
# Sección modificable por el usuario
########################################################
# Tamaño de las muestras
n1<-1000
n2<-1000

# casos favorables en las muestras o, en su defecto, 
# proporciones
x1<-275
x2<-250

p1<-NULL
p2<-NULL

# Indique el nivel de significancia
alfa<-0.05

# Límite superior (S), inferior (I) o intervalo de dos colas (D)
inter<-'D'

########################################################
# Sección que realiza el procedimiento
########################################################

# Asegurar los valores de p1 y p2
if(is.null(p1))
    p1<-x1/n1
if(is.null(p2))
    p2<-x2/n2

# Función que recibe los datos de entrada y calcula el
# intervalo de confianza para la diferencia de medias.
# Puede calcular intervalos bilaterales o unilaterales.
difMedias<-function(n1,n2,p1,p2,alfa=0.05,colas='D'){
    q1<-1-p1
    q2<-1-p2
    diferencia<-p1-p2
    if(n1 < 2 | n2 < 2){
        r<-data.frame(n1=n1,n2=n2,
                      media1=m1,media2=m2,
                      LimInf=NA,
                      diferencia=diferencia,
                      LimSup=NA)
    }else{
        # Calcular la desviación de la diferencia de los promedios
        desvDifProm<-sqrt(p1*q1/n1 + p2*q2/n2)
        # Caso de dos colas
        if(colas=='D'){
            # valor crítico cuando es un intervalo bilateral
            zalfa<-qnorm(1-alfa/2)
            # Límites inferior y superior
            LL<-round(diferencia-zalfa*desvDifProm,7)
            LU<-round(diferencia+zalfa*desvDifProm,7)
            r<-data.frame(n1=n1,n2=n2,p1=p2,p2=p2,
                          LimInf=LL,
                          diferencia=diferencia,
                          LimSup=LU)
        }
        else{
            # Valor crítico cuando es un intervalo unilateral
            zalfa<-qnorm(1-alfa)
            if(colas=='I'){  # Cálculo del límite para el intervalo inferior
                LL<-round(diferencia-zalfa*desvDifProm,7)
                r<-data.frame(n1=n1,n2=n2,p1=p1,p2=2,
                              LimInf=LL,
                              diferencia=diferencia)
            }else{  # Cálculo del límite para el intervalo superior
                LU<-round(diferencia+zalfa*desvDifProm,7)
                r<-data.frame(n1=n1,n2=n2,p1=p1,p2=p2,
                              diferencia=diferencia,
                              LimSup=LU)
            }
        }
    }
    return(r)
}

rFin<-difMedias(n1,n2,p1,p2,alfa=alfa, colas=inter)

########################################################
# Sección que muestra los resultados
########################################################

rFin

