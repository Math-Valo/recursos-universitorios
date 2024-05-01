# P02_Probabilidad_de_error_normal_1.r
# Probabilidad de error de tipo I y/o II en pruebas normales.
########################################################
# Se calcula alpha, la probabilidad de cometer un error de tipo
# I, y beta, la probabilidad de cometer un error de tipo II,
# según el que pida el usuario (puede hacer ambos en simultáneo)
# suponiendo una distribución normal, y dados los valores
# críticos superior o inferior, o ambos, en los que se incluyen
# para la región de aceptación.
#
# La prueba se permite hacer como la aproximación normal de una
# prueba binomial, o como una prueba directamente de una normal
# con la desviación estándar conocida.
########################################################
########################################################
# Sección modificable por el usuario
########################################################

# Tamaño de la muestra
n<-200

# Valor(es) crítico(s)
CriticoInf<-110
CriticoSup<-130

# Si se da una distribución normal
# desviación estándar normal
desv<-NULL
# Hipótesis nula normal
media0<-NULL
# Hipótesis alternativa normal
media1<-NULL

# Si se da una aproximación binomial
# Hipótesis nula como aproximación binomial
p0<-0.6
# Hipótesis alternativa como aproximación binomial
p1<-0.5

########################################################
# Sección que realiza el procedimiento
########################################################

alpha1<-0
alpha2<-0
beta1<-0
beta2<-0
hipprop0<-FALSE
hipprop1<-FALSE

binf<-!is.null(CriticoInf)
bsup<-!is.null(CriticoSup)

# Si no se está dando la desviación, entonces se trata de una aproximación binomial
# en este caso, se debe calcular el valor de los parámetros
if(is.null(desv)){
    if(!is.null(p0)){
        media0<-n*p0
        desv0<-sqrt(n*p0*(1-p0))
        hipprop0<-TRUE
    }
    if(!is.null(p1)){
        media1<-n*p1
        desv1<-sqrt(n*p1*(1-p1))
        hipprop1<-TRUE
    }
    if(binf) CriticoInf<-CriticoInf-0.5
    if(bsup) CriticoSup<-CriticoSup+0.5
} else{
    desv0<-desv
    desv1<-desv
}

balpha<-!is.null(media0)
bbeta<-!is.null(media1)

if(binf){
    if(balpha) alpha1<-pnorm(CriticoInf,media0,desv0)
    if(bbeta) beta1<-pnorm(CriticoInf,media1,desv1)
}
if(bsup){
    if(balpha) alpha2<-1-pnorm(CriticoSup,media0,desv0)
    if(bbeta) beta2<-1-pnorm(CriticoSup,media1,desv1)
}

alpha<-alpha1+alpha2
beta<-1-(beta1+beta2)

if(balpha){
    if(binf&bsup){
        Resultado1<-data.frame(HipotesisNula
                               =ifelse(hipprop0,
                                       paste("p = ",p0),
                                       paste("mu = ",media0)),
                               n=n,
                               media=media0,
                               desv=desv0,
                               CríticoInf=CriticoInf,
                               CriticoSup=CriticoSup,
                               alpha=alpha)
    } else{
        if(binf){
            Resultado1<-data.frame(HipotesisNula
                                   =ifelse(hipprop0,
                                           paste("p = ",p0),
                                           paste("mu = ",media0)),
                                   n=n,
                                   media=media0,
                                   desv=desv0,
                                   CriticoInf=CriticoInf,
                                   alpha=alpha)
        }
        else{
            Resultado1<-data.frame(HipotesisNula
                                   =ifelse(hipprop0,
                                           paste("p = ",p0),
                                           paste("mu = ",media0)),
                                   n=n,
                                   media=media0,
                                   desv=desv0,
                                   CriticoSup=CriticoSup,
                                   alpha=alpha)
        }
    }
} else Resultado1<-NULL

if(bbeta){
    if(binf&bsup){
        Resultado2<-data.frame(HipotesisAlternativa
                               =ifelse(hipprop1,
                                       paste("p = ",p1),
                                       paste("mu = ",media1)),
                               n=n,
                               media=media1,
                               desv=desv1,
                               CríticoInf=CriticoInf,
                               CriticoSup=CriticoSup,
                               beta=beta)
    } else{
        if(binf){
            Resultado2<-data.frame(HipotesisAlternativa
                                   =ifelse(hipprop1,
                                           paste("p = ",p1),
                                           paste("mu = ",media1)),
                                   n=n,
                                   media=media1,
                                   desv=desv1,
                                   CriticoInf=CriticoInf,
                                   beta=beta)
        }
        else{
            Resultado2<-data.frame(HipotesisAlternativa
                                   =ifelse(hipprop1,
                                           paste("p = ",p1),
                                           paste("mu = ",media1)),
                                   n=n,
                                   media=media1,
                                   desv=desv1,
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

