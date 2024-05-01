# P18_Estimacion_del_error_2.r
# Calcula el error cuando se estima la proporción de casos favorables
########################################################
# Se calcula un valor que no excede el error de cálculos al
# estimar con la proporción de casos favorables entre totales la
# proporción real en una población usando una muestra grande o que,
# al menos, el producto del tamaño de la muestra por, tanto la proporción
# estimada como por uno menos la proporción estimada, sean al menos 5
########################################################
########################################################
# Sección modificable por el usuario
########################################################

# Tamaño de la muestra
n<-200
# casos favorables (puede no darse)
x<-114
# proporción estimada (si se da en vez de x)
p<-NULL
# Nivel de significancia
alfa<-0.04
# Para un error unilateral (U) o bilateral (D)
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

# Cálculo del error
error<-round(zalfa*sqrt(p*q/n),7)

########################################################
# Muestra de los resultados
########################################################

error

