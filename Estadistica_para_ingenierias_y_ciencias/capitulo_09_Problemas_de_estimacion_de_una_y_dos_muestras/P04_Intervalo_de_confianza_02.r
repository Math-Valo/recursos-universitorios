# P04_Intervalo_de_confianza_02.r
# Intervalo de confianza de la media de una población con desviación desconocida.
########################################################
# Se calcula un intervalo de confianza para le media poblacional,
# usando la media muestral con una muestra pequeña, de una 
# población que se distribuye de forma normal en donde se 
# desconoce la desviación estándar poblacional.
#
# Se están suponiendo que el resumen de los datos se conocen,
# y, por lo tanto, este programa no leerá datos de un archivo
# externo.
########################################################
########################################################
# Sección modificable por el usuario
########################################################

# Tamaño de la muestra
n<-20
# Media muestral
m<-11.3
# Desviación estándar muestral
s<-2.45
# Nivel de significancia
alfa<-0.05

########################################################
# Sección que realiza el procedimiento
########################################################

# Valor crítico
talfa<-qt(1-alfa/2,n-1)
# Límites del intervalo
LL<-round(m-talfa*s/sqrt(n),7)
LU<-round(m+talfa*s/sqrt(n),7)
# Creación del resultado en una única variable
IC<-data.frame(LL,m,LU)
# Renombrar las columnas
names(IC)[names(IC) == "LL"]<-"LimInf"
names(IC)[names(IC) == "m"]<-"Media"
names(IC)[names(IC) == "LU"]<-"LimSup"

########################################################
# Muestra de los resultados
########################################################

IC
