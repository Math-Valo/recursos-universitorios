# P02_Estimacion_del_error_1.r
# Calcula el error cuando se estima la media poblacional con la media muestral
########################################################
# Se calcula un valor que no excede el error de cálculos al
# estimar con la media muestral una media poblacional de una
# población que se distribuye de forma normal; o bien, que
# la muestra es lo suficientemente grande para que la
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
n<-50
# Desviación estándar poblacional
desv.tipica<-6.9
# Nivel de significancia
alfa<-0.02

########################################################
# Sección que realiza el procedimiento
########################################################

# Valor crítico
zalfa<-qnorm(1-alfa/2)
error<-round(zalfa*desv.tipica/sqrt(n),7)

########################################################
# Muestra de los resultados
########################################################

error
