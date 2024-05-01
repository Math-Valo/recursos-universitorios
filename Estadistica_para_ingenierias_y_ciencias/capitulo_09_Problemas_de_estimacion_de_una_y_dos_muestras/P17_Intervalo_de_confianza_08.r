# P17_Intervalo_de_confianza_08.r
# Intervalo de confianza de una proporción para una muestra grande
########################################################
# Se calcula un intervalo de confianza para la proporción de una
# población, suponiendo que la muestra es lo suficientemente grande,
# o, si tanto el producto del tamaño de muestra por el estimador
# como por uno menos el estimador, son al menos 5.
#
# Se están suponiendo que el resumen de los datos se conocen,
# y, por lo tanto, este programa no leerá datos de un archivo
# externo.
########################################################
########################################################
# Sección modificable por el usuario
########################################################

# Tamaño de la muestra
n<-200
# casos favorales (puede no darse)
x<-114
# proporción estimada (si se da en vez de x)
p<-NULL
# Nivel de significancia
alfa<-0.04
# Límite superior (S), inferior (I) o intervalo de dos colas (D)
inter<-'D'

########################################################
# Sección que realiza el procedimiento
########################################################

# Estimador
if(is.null(p)){
   p<-x/n
}
q<-1-p

# Valor crítico
if(inter=='D'){
   zalfa<-qnorm(1-alfa/2)
}else{
   zalfa<-qnorm(1-alfa)
}

# Límites del intervalo
LL<-round(p-zalfa*sqrt(p*q/n),7)
LU<-round(p+zalfa*sqrt(p*q/n),7)

# Creación del resultado en una única variable
IC<-data.frame(LL,p,LU)
# Renombrar las columnas
names(IC)[names(IC) == "LL"]<-"LimInf"
names(IC)[names(IC) == "p"]<-"Proporción"
names(IC)[names(IC) == "LU"]<-"LimSup"


########################################################
# Muestra de los resultados
########################################################

IC

