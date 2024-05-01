# P06_Intervalo_de_tolerancia_1.r
# Intervalo de tolerancia bilateral de una población normal.
########################################################
# Se calcula un intervalo de tolerancia, usando la media y
# desviación estándar muestrales, de una población que se
# distribuye de forma normal con (1-alpha)100% de seguridad
# que contiene P proporción de la población.
#
# Se están suponiendo que el resumen de los datos se conocen,
# y, por lo tanto, este programa no leerá datos de un archivo
# externo.
########################################################
########################################################
# Sección de cabezera (incluir paquetes, etc.)
########################################################

# Paquete requerido
library(tolerance)

########################################################
# Sección modificable por el usuario
########################################################

# Tamaño de la muestra
n<-25

# Media muestral
m<-325.05

# Desviación estándar muestral
s<-0.5

# Nivel de significancia
gamma<-0.05

# Nivel de proporción (proporción := (1-alfa)100%)
alfa<-0.1

########################################################
# Sección que realiza el procedimiento
########################################################

# Factor de tolerancia
k<-K.factor(n,alpha=gamma,P=1-alfa,side=2,method=c("WBE"))

# Cálculo del intervalo
LL<-m-k*s
LU<-m+k*s

########################################################
# Muestra de los resultados
########################################################

LL;LU;

