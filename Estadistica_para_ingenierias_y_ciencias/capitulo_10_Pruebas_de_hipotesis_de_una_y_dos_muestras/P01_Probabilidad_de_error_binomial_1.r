# P01_Probabilidad_de_error_binomial_1.r
# Probabilidad de error de tipo I y/o II en pruebas binomiales.
########################################################
# Se calcula alpha, la probabilidad de cometer un error de tipo
# I, y beta, la probabilidad de cometer un error de tipo II,
# seg�n el que pida el usuario (puede hacer ambos en simult�neo)
# suponiendo una distribuci�n binomial, y dados los valores
# cr�ticos superior o inferior, o ambos, en los que se incluyen
# para la regi�n de aceptaci�n.
########################################################
########################################################
# Secci�n modificable por el usuario
########################################################

# Tama�o de la muestra
n<-15

# Valor(es) cr�tico(s)
# Despu�s de estos, es regi�n de aceptaci�n
CriticoInf<-6
CriticoSup<-12

# Hip�tesis nula
p0<-0.6

# Hip�tesis alternativa
p1<-NULL

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

