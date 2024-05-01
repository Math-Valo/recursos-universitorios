# P01_Probabilidad_de_error_binomial_1.r
# Probabilidad de error de tipo I y/o II en pruebas binomiales.
########################################################
# Se calcula alpha, la probabilidad de cometer un error de tipo
# I, y beta, la probabilidad de cometer un error de tipo II,
# según el que pida el usuario (puede hacer ambos en simultáneo)
# suponiendo una distribución binomial, y dados los valores
# críticos superior o inferior, o ambos, en los que se incluyen
# para la región de aceptación.
########################################################
########################################################
# Sección modificable por el usuario
########################################################

# Tamaño de la muestra
n<-15

# Valor(es) crítico(s)
# Después de estos, es región de aceptación
CriticoInf<-6
CriticoSup<-12

# Hipótesis nula
p0<-0.6

# Hipótesis alternativa
p1<-NULL

########################################################
# Sección que realiza el procedimiento
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
                               CríticoInf=CriticoInf,
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
                               CríticoInf=CriticoInf,
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
# Sección que muestra los resultados
########################################################

Resultado

