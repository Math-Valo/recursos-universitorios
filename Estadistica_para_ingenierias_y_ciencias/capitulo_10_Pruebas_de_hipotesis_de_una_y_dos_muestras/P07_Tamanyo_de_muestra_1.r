# P07_Tamanyo_de_muestra_1.r
# Calcula el m�nimo tama�o muestral para alcanzar una calidad
# deseada para la prueba de hip�tesis de una media
########################################################
# Se calcula el tama�o que debe de tener la muestra para que, al
# realizar con ella una prueba de hip�tesis de una media, se
# alcance la calidad de una potencia de prueba predeterminada,
# dada una hip�tesis alternativa fija, suponiendo que la muestra
# proviene de una poblaci�n que se distribuye normalmente.
########################################################
########################################################
# Secci�n de cabezera (incluir paquetes, etc.)
########################################################

# Paquete requerido
library(pwr)

########################################################
# Secci�n modificable por el usuario
########################################################

# Probabilidad de cometer un error tipo I
alfa<-0.05
# Probabilidad de cometer un error tipo II
beta<-0.1
# Media poblacional de la hip�tesis nula
mu<-5.5
# Diferencia de las medias poblacionales.
# La de la hip�tesis nula menos la de la alternativa,
# dada una media poblacional alternativa fija.
delta<--0.3
# Desviaci�n est�ndar poblacional
sigma<-0.24

# Para un error unilateral (U) o bilateral (D)
inter<-'D'

########################################################
# Secci�n que realiza el procedimiento
########################################################

alpha1<-0
alpha2<-0
beta1<-0
beta2<-0

binf<-!is.null(CriticoInf)
bsup<-!is.null(CriticoSup)
balpha<-!is.null(p0)
bbeta<-!is.null(p1)

if(binf){
    if(balpha) alpha1<-pbinom(CriticoInf-1,n,p0)
    if(bbeta) beta1<-pbinom(CriticoInf-1,n,p1)
}
if(bsup){
    if(balpha) alpha2<-1-pbinom(CriticoSup,n,p0)
    if(bbeta) beta2<-1-pbinom(CriticoSup,n,p1)
}

alpha<-alpha1+alpha2
beta<-1-(beta1+beta2)

if(balpha){
    if(binf&bsup){
        Resultado1<-data.frame(HipotesisNula=p0,
                               n=n,
                               Cr�ticoInf=CriticoInf,
                               CriticoSup=CriticoSup,
                               alpha=alpha)
    } else{
        if(binf){
            Resultado1<-data.frame(HipotesisNula=p0,
                                   n=n,
                                   CriticoInf=CriticoInf,
                                   alpha=alpha)
        }
        else{
            Resultado1<-data.frame(HipotesisNula=p0,
                                   n=n,
                                   CriticoSup=CriticoSup,
                                   alpha=alpha)
        }
    }
} else Resultado1<-NULL

if(bbeta){
    if(binf&bsup){
        Resultado2<-data.frame(HipotesisAlternativa=p1,
                               n=n,
                               Cr�ticoInf=CriticoInf,
                               CriticoSup=CriticoSup,
                               beta=beta)
    } else{
        if(binf){
            Resultado2<-data.frame(HipotesisAlternativa=p1,
                                   n=n,
                                   CriticoInf=CriticoInf,
                                   beta=beta)
        }
        else{
            Resultado2<-data.frame(HipotesisAlternativa=p1,
                                   n=n,
                                   CriticoSup=CriticoSup,
                                   beta=beta)
        }
    }
} else Resultado2<-NULL

if(balpha&bbeta){
    Resultado<-list(Resultado1,Resultado2)
    names(Resultado)<-c("Probabilidad de error tipo I",
                        "Probabilidad de error tipo II")
} else{
    if(balpha){
        Resultado<-list(Resultado1)
        names(Resultado)<-c("Probabilidad de error tipo I")
    } else{
        Resultado<-list(Resultado2)
        names(Resultado)<-c("Probabilidad de error tipo II")
    }
}

########################################################
# Secci�n que muestra los resultados
########################################################

Resultado

