# P03_Prueba_de_un_media_01.r
# Contrasta la hipótesis para la media de una población
########################################################
# Se realizan los cálculos para una prueba de hipótesis de
# una muestra para la media poblacional usando el P-valor y
# calcula la región de rechazo. Según los datos (tamaño de 
# muestra y distribución poblacional) se realiza una prueba
# normal estándar o una prueba t. El nivel de significancia
# es opcional para una prueba con el P-valor, en dado caso
# se usa un valor predeterminado para la región de rechazo,
# y, si se asigna un nivel de significancia, la salida da
# un veredicto a la prueba.
#
# Se están suponiendo conocido el resumen de los datos y,
# por lo tanto, no se leerán datos de un archivo externo.
########################################################
########################################################
# Sección modificable por el usuario
########################################################

# Tamaño de la muestra
n<-50

# Media de la prueba
mu<-24

# Media de la muestra
m<-22.8

# Desviación estándar (poblacional o muestral)
desv<-4.8

# ¿La desviación estándar es poblacional?
pobl<-FALSE

# Nivel de significancia
alfa<-NULL

# Tipo de prueba: cola superior (S), cola inferior (I) o dos colas (D)
cola<-'I'

# ¿La población se distribuye de forma normal?
val<-FALSE

########################################################
# Sección que realiza el procedimiento
########################################################

# Si no se puede suponer que la población se distribuye normalmente o
# si la muestra es pequeña, entonces no hay método valido y se detiene
# el proceso.
if(!val & n < 30){
    stop("Se requiere normalidad o muestras grandes")
}
if(!pobl & n >= 30){
    pobl<-TRUE
}

TestMedia<-function(n,mu,m,desv,alfa=0.05,colas='D',distribucion.z=FALSE){
    estadistico<-(m-mu)/(desv/sqrt(n))
    if(n < 2){
        r<-data.frame(Prueba=NA,
                      H0=mu,
                      n=n,
                      MediaMuestral=m,
                      desv.est=ifelse(is.na(desv),NA,desv),
                      error.est=ifelse(is.na(desv),NA,desv/sqrt(n)),
                      alpha=alfa,
                      PValor=NA,
                      Estadistico=NA,
                      RegionRechazoZ=NA,
                      RegionRechazoX=NA)
    }else{
        if(distribucion.z){
            r<-data.frame(Prueba="Z",
                          H0=mu,
                          n=n,
                          MediaMuestral=m,
                          desv.est=desv,
                          error.est=desv/sqrt(n),
                          alpha=alfa)
            if(colas=='D'){
                pvalor<-round(2*pnorm(abs(estadistico),lower.tail=F),7)
                criticoz<-round(qnorm(1-alfa/2),7)
                criticox<-round(criticoz*desv/sqrt(n),7)
                r$PValor<-pvalor
                r$Estadistico<-estadistico
                r$RegionRechazoZ<-paste("<",-criticoz," y >",criticoz)
                r$RegionRechazoX<-paste("<",mu-criticox," y >",mu+criticox)
            }else{
                criticoz<-round(qnorm(1-alfa),7)
                criticox<-round(criticoz*desv/sqrt(n),7)
                if(colas=='I'){
                    pvalor<-round(pnorm(estadistico),7)
                    r$PValor<-pvalor
                    r$Estadistico<-estadistico
                    r$RegionRechazoZ<-paste("<",-criticoz)
                    r$RegionRechazoX<-paste("<",mu-criticox)
                }else{
                    pvalor<-round(pnorm(estadistico,lower.tail=F),7)
                    r$PValor<-pvalor
                    r$Estadistico<-estadistico
                    r$RegionRechazoZ<-paste(">",criticoz)
                    r$RegionRechazoX<-paste(">",mu+criticox)
                }
            }
        }else{
            r<-data.frame(Prueba="t",
                          H0=mu,
                          n=n,
                          MediaMuestral=m,
                          desv.est=desv,
                          error.est=desv/sqrt(n),
                          alpha=alfa)
            if(colas=='D'){
                pvalor=round(2*pt(abs(estadistico),n-1,lower.tail=F),7)
                criticot<-round(qt(1-alfa/2,n-1),7)
                criticox<-round(criticot*desv/sqrt(n),7)
                r$PValor<-pvalor
                r$Estadistico<-estadistico
                r$RegionRechazoT<-paste("<",-criticot," y >",criticot)
                r$RegionRechazoX<-paste("<",mu-criticox," y >",mu+criticox)
            }else{
                criticot<-round(qt(1-alfa,n-1),7)
                criticox<-round(criticot*desv/sqrt(n),7)
                if(colas=='I'){
                    pvalor<-round(pt(estadistico,n-1),7)
                    r$PValor<-pvalor
                    r$Estadsitico<-estadistico
                    r$RegionRechazoT<-paste("<",-criticot)
                    r$RegionRechazoX<-paste("<",mu-criticox)
                }else{
                    pvalor<-round(pt(estadistico,n-1,lower.tail=F),7)
                    r$PValor<-pvalor
                    r$Estadistico<-estadistico
                    r$RegionRechazoT<-paste(">",criticot)
                    r$RegionRechazoX<-paste(">",mu+criticox)
                }
            }
        }
    }
    return(r)
}

if(is.null(alfa)){
    Test<-TestMedia(n,mu,m,desv,colas=cola,distribucion.z=pobl)
}else{
    Test<-TestMedia(n,mu,m,desv,alfa,cola,pobl)
    resultado<-ifelse(Test[,"PValor"]>=alfa,"No se rechaza H0","Se rechaza H0")
    Test$Resultado<-resultado
    Test$Resultado[is.na(Test$Resultado)]<-"Pocos datos"
}

########################################################
# Muestra de los resultados
########################################################

Test

