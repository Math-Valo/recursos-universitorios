# P03_Tamanyo_de_muestra_1.r
# Calcula el tamaño de muestra para no exceder cierto error al estimar la media
########################################################
# Se calcula el tamaño que debe de tener la muestra para que,
# al estimar la media poblacional con la media muestral, el
# error no exceda una cantidad específica, suponiendo que
# la varianza es conocida o que la muestra es suficientemente 
# grande.
########################################################
########################################################
# Sección modificable por el usuario
########################################################

# Error considerado
error<-10
# Desviación estándar poblacional
desv.tipica<-40
# Nivel de significancia
alfa<-0.04

########################################################
# Sección que realiza el procedimiento
########################################################

# Valor crítico
zalfa<-qnorm(1-alfa/2)
# Tamaño de la muestra
n<-ceiling((zalfa*desv.tipica/error)^2)

########################################################
# Muestra de los resultados
########################################################

n
