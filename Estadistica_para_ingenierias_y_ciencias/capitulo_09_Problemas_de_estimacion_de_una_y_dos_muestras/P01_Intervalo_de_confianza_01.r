# P01_Intervalo_de_confianza_01.r
# Intervalo de confianza de la media de una población normal
########################################################
# Se calcula un intervalo de confianza para la media poblacional
# en donde se conoce la desviación estándar poblacional, o bien,
# si la muestra es lo suficientemente grande para que la 
# desviación estándar muestral sea aproximadamente el valor de
# la desviación estándar poblacional.
#
# Se están suponiendo que el resumen de los datos se conocen,
# y, por lo tanto, este programa no leerá datos de un archivo
# externo.
########################################################
########################################################
# Sección modificable por el usuario
########################################################

# Tamaño de la muestra
n<-30
# Media muestral
m<-780
# Desviación estándar
desv.tipica<-40
# Nivel de significancia
alfa<-0.04

########################################################
# Sección que realiza el procedimiento
########################################################

# Valor crítico
zalfa<-qnorm(1-alfa/2)
LL<-round(m-zalfa*desv.tipica/sqrt(n),7)
LU<-round(m+zalfa*desv.tipica/sqrt(n),7)

########################################################
# Muestra de los resultados
########################################################

LL; LU;
