# P08_Intervalo_de_prediccion_1.r
# Intervalo bilateral de predicción de un elemento.
########################################################
# Se calcula un intervalo de predicción bilateral, el script
# permite poner la opción de si se conoce o no la desviación
# estándar poblacional, calculando según sea el caso con el 
# valor crítico de una distribución normal o distribución t, 
# respectivamente. En caso de que la muestra sea mayor a 30
# elementos, por defecto se usará la desviación dada como si
# fuese la poblacional.
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

# Desviación estándar (muestral o poblacional)
desv<-2.45

# Nivel de significancia
alfa<-0.05

# ¿Se conoce la desviación poblacional?
val<-FALSE

########################################################
# Sección que realiza el procedimiento
########################################################

# La desviación se considerará poblacional si el tamaño de muestra es grande
if (n >= 30)
{
    val<-TRUE
}

# valor crítico
k<-ifelse(val,round(qnorm(1-alfa/2),7),round(qt(1-alfa/2,n-1),7))

# Límites del intervalo
LL<-m-k*desv*sqrt(1+1/n)
LU<-m+k*desv*sqrt(1+1/n)

# Creación del resultado en una única variable
IP<-data.frame(LL,m,LU)
# Renombrar las columnas
names(IP)[names(IP) == "LL"]<-"LimInf"
names(IP)[names(IP) == "m"]<-"Media"
names(IP)[names(IP) == "LU"]<-"LimSup"

########################################################
# Muestra de los resultados
########################################################

IP

