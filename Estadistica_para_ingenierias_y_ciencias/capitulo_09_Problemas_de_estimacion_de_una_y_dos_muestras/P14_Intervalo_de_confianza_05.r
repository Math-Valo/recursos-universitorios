# P14_Intervalo_de_confianza_05.r
# Intervalo de la diferencia de medias de poblaciones Normales
########################################################
# Se calcula un intervalo de confianza para la diferencia de
# medias poblacionales, usando la diferencia de las medias
# muestrales, sin conocer las desviaciones estándar de la
# población pero suponiendo que las poblaciones se distribuyen
# aproximadamente normal. El intervalo puede ser unilateral o
# bilateral.
#
# Se están suponiendo que el resumen de los datos se conocen,
# y, por lo tanto, este programa no leerá datos de un archivo
# externo.
########################################################
########################################################
# Sección modificable por el usuario
########################################################
# Tamaño de las muestras
n1<-12
n2<-10

# Medias muestrales
m1<-85
m2<-81

# Desviación estándar de cada población (o muestra si el
# tamaño de muestra es grande)
desv.tipica1<-4
desv.tipica2<-5

# Indique el nivel de significancia
alfa<-0.1

# ¿Se conoce la desviación poblacional?
val<-FALSE

# ¿Varianzas iguales?
varia<-TRUE

# Límite superior (S), inferior (I) o intervalo de dos colas (D)
inter<-'D'

########################################################
# Sección que realiza el procedimiento
########################################################

# La desviación se considerará poblacional si el tamaño de muestra es grande
if (n1 >= 30 & n2 >= 30){
    val<-TRUE
}

# Función que recibe los datos de entrada y calcula el
# intervalo de confianza para la diferencia de medias.
# Puede calcular intervalos bilaterales o unilaterales.
difMedias<-function(n1,n2,m1,m2,desv1,desv2,alfa=0.05,colas='D',distribucion.z=FALSE,
                    varia=FALSE){
    diferencia<-m1-m2
    if(n1 < 2 | n2 < 2){
        r<-data.frame(n1=n1,n2=n2,
                      media1=m1,media2=m2,
                      LimInf=NA,
                      diferencia=diferencia,
                      LimSup=NA)
    }else{
        if(distribucion.z){
            # Calcular la desviación de la diferencia de medias
            desvDifMedia<-sqrt(desv1^2/n1 + desv2^2/n2)
            # Caso de dos colas
            if(colas=='D'){
                # valor crítico cuando es un intervalo bilateral
                k<-qnorm(1-alfa/2)
                # Límites inferior y superior
                LL<-round(diferencia-k*desvDifMedia,7)
                LU<-round(diferencia+k*desvDifMedia,7)
                r<-data.frame(n1=n1,n2=n2,
                              media1=m1,media2=m2,
                              LimInf=LL,
                              diferencia=diferencia,
                              LimSup=LU)
            }else{
                # Valor crítico cuando es un intervalo unilateral
                k<-qnorm(1-alfa)
                if(colas=='I'){  # Cálculo del límite para el intervalo inferior
                    LL<-round(diferencia-k*desvDifMedia,7)
                    r<-data.frame(n1=n1,n2=n2,
                                  media1=m1,media2=m2,
                                  LimInf=LL,
                                  diferencia=diferencia)
                }else{  # Cálculo del límite para el intervalo superior
                    LU<-round(diferencia+k*desvDifMedia,7)
                    r<-data.frame(n1=n1,n2=n2,
                                  media1=m1,media2=m2,
                                  diferencia=diferencia,
                                  LimSup=LU)
                }
            }
        }else{
            if(varia){  # ¿Las varianzas son iguales?
                Sp <- sqrt(((n1-1)*desv1^2+(n2-1)*desv2^2)/(n1+n2-2))
                if(colas=='D'){
                    k<-qt(1-alfa/2,n1+n2-2)
                    LL<-round(diferencia-k*Sp*sqrt(1/n1+1/n2),7)
                    LU<-round(diferencia+k*Sp*sqrt(1/n1+1/n2),7)
                    r<-data.frame(n1=n1,n2=n2,
                                  media1=m1,media2=m2,
                                  LimInf=LL,
                                  diferencia=diferencia,
                                  LimSup=LU)
                }else{
                    k<-qt(1-alfa,n1+n2-2)
                    if(colas=='I'){
                        LL<-round(diferencia-k*Sp*sqrt(1/n1+1/n2),7)
                        r<-data.frame(n1=n1,n2=n2,
                                      media1=m1,media2=m2,
                                      LimInf=LL,
                                      diferencia=diferencia)
                    }else{
                        LU<-round(diferencia+k*Sp*sqrt(1/n1+1/n2),7)
                        r<-data.frame(n1=n1,n2=n2,
                                      media=m1,media2=m2,
                                      diferencia=diferencia,
                                      LimSup=LU)
                    }
                }
            }else{
                numerador<-(desv1^2/n1+desv2^2/n2)^2
                denominador<-(desv1^2/n1)^2/(n1-1)+(desv2^2/n2)^2/(n2-1)
                grados<-round(numerador/denominador,0)
                if(colas=='D'){
                    k<-qt(1-alfa/2,grados)
                    LL<-round(diferencia-k*sqrt(desv1^2/n1+desv2^2/n2),7)
                    LU<-round(diferencia+k*sqrt(desv1^2/n1+desv2^2/n2),7)
                    r<-data.frame(n1=n1,n2=n2,
                                  media1=m1,media2=m2,
                                  LimInf=LL,
                                  diferencia=diferencia,
                                  LimSup=LU)
                }else{
                    k<-qt(1-alfa,grados)
                    if(colas=='I'){
                        LL<-round(diferencia-k*sqrt(desv1^2/n1+desv2^2/n2),7)
                        r<-data.frame(n1=n1,n2=n2,
                                      media1=m1,media2=m2,
                                      LimInf=LL,
                                      diferencia=diferencia)
                    }else{
                        LU<-round(diferencia+k*sqrt(desv1^2/n1+desv2^2/n2),7)
                        r<-data.frame(n1=n1,n2=n2,
                                      media1=m1,media2=m2,
                                      diferencia=diferencia,
                                      LimSup=LU)
                    }
                }
            }
        }
    }
    return(r)
}

rFin<-difMedias(n1,n2,m1,m2,desv.tipica1,desv.tipica2,alfa=alfa,colas=inter,
                distribucion.z=val,varia=varia)

########################################################
# Sección que muestra los resultados
########################################################

rFin

