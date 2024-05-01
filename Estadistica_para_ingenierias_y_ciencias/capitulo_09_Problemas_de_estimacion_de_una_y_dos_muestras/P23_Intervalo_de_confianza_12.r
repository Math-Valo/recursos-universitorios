# P23_Intervalo_de_confianza_12.r
# Intervalo de confianza de la raz?n de varianzas
# de poblaciones normales y prueba de homogeneidad
# de varianza entre poblaciones normales
########################################################
# Se calcula un intervalo de confianza para la proporci?n de
# varianzas o de desviaciones est?ndar, usando la proporci?n
# de varianzas o de desviaciones est?ndar muestrales, para
# poblaciones con districi?n normal, o muestras que sean
# suficientemente grande. El intervalo puede ser unilateral
# o bilateral.
#
# Se está suponiendo conocido el resumen de los datos y,
# por lo tanto, no se leerán datos de un archivo externo.
########################################################
########################################################
# Secci?n modificable por el usuario
########################################################

# Tama?o de las muestras
n1<-12
n2<-10

# valor de los estimadores (varianza o desviaci?n muestral)
var1<-NULL
var2<-NULL
desv.est1<-1
desv.est2<-0.8

# Nivel de significancia
alfa<-0.02

# Intervalo de confianza para proporci?n de varianza o de desviacion tipica?
# tipoInterv<-"var"
tipoInterv<-"desv.est"

# L?mite superior (S), inferior (I) o intervalo de dos colas (D)
inter<-'D'

# ?La poblaci?n se distribuye de forma normal?
val<-TRUE

########################################################
# Secci?n que realiza el procedimiento
########################################################

# Si no se puede suponer que la poblaci?n se distribuye normalmente o
# si la muestra es peque?a, entonces no hay validez en el intervalo 
# de confianza y se detiene el proceso.
if(!val & n1 < 30 & n2 < 30){
    stop("Se requiere normalidad o muestras grandes")
}

if((is.null(var1) & is.null(desv.est1)) | (is.null(var2) & is.null(desv.est2))){
    stop("datos insuficientes")
}

if(is.null(var1)) var1<-desv.est1^2
if(is.null(var2)) var2<-desv.est2^2

# Funci?n que recibe los datos de entrada y calcula el
# intervalo de confianza para la diferencia de medias.
# Puede calcular intervalos bilaterales o unilaterales.
difMedias<-function(n1,n2,var1,var2,alfa=0.05,colas='D',tipoInter="var"){
    razon<-round(var1/var2,7)
    estimador<-ifelse(tipoInter=="var",razon,round(sqrt(razon),7))
    if(n1<2 | n2 < 2){
        r<-data.frame(Estimando=tipoInterv,
                      n1=n1,n2=n2,
                      desv.Est1=NA,
                      desv.Est2=NA,
                      LimInf=NA,
                      razon=NA,
                      LimSup=NA)
    }else{
        if(colas=='D'){
            jicuadL<-qf(alfa/2,n1-1,n2-1,lower.tail=F)
            jicuadU<-qf(alfa/2,n2-1,n1-1,lower.tail=F)
            LL<-round(ifelse(tipoInter=="var",
                             estimador/jicuadL,
                             estimador/sqrt(jicuadL)),7)
            LU<-round(ifelse(tipoInter=="var",
                             estimador*jicuadU,
                             estimador*sqrt(jicuadU)),7)
	    Pvalor<-2*min(pf(razon,n1-1,n2-1,lower.tail=F),
                          pf(razon,n1-1,n2-1))
            r<-data.frame(Estimando=tipoInterv,
                          n1=n1,n2=n2,
                          desv.Est1=sqrt(var1),
                          desv.Est2=sqrt(var2),
                          LimInf=LL,
                          razon=estimador,
                          LimSup=LU,
                          Pvalor=Pvalor)
        }else{
            if(colas=='I'){
                jicuadL<-qf(alfa,n1-1,n2-1,lower.tail=F)
                LL<-round(ifelse(tipoInter=="var",
                                 estimador/jicuadL,
                                 estimador/sqrt(jicuadL)),7)
                Pvalor<-pf(razon,n1-1,n2-1)
                r<-data.frame(Estimando=tipoInterv,
                              n1=n1,n2=n2,
                              desv.Est1=sqrt(var1),
                              desv.Est2=sqrt(var2),
                              LimInf=LL,
                              razon=estimador,
                              Pvalor=Pvalor)
            }else{
                jicuadU<-qf(alfa,n2-1,n1-1,lower.tail=F)
                LU<-round(ifelse(tipoInter=="var",
                                 estimador*jicuadU,
                                 estimador*sqrt(jicuadU)),7)
                Pvalor<-1/pf(razon,n2-1,n1-1)
                r<-data.frame(Estimando=tipoInterv,
                              n1=n1,n2=n2,
                              desv.Est1=sqrt(var1),
                              desv.Est2=sqrt(var2),
                              razon=estimador,
                              LimSup=LU,
                              Pvalor=Pvalor)
            }
        }
    }
    return(r)
}

ICs<-difMedias(n1,n2,var1,var2,alfa=alfa,colas=inter,tipoInter=tipoInterv)
resultado<-ifelse(ICs[,"Pvalor"]>=alfa,"Var no diferentes","Var diferentes")
ICs$Resultado<-resultado
ICs$Resultado[is.na(ICs$Resultado)]<-"Pocos datos"

########################################################
# Secci?n que muestra los resultados
########################################################

ICs


