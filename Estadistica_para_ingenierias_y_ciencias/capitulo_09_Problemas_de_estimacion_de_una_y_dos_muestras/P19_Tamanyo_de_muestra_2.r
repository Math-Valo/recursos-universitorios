# P19_Tamanyo_de_muestra_2.r
# Calcula el tamaño de muestra al estimar la proporción para no exceder cierto error al estimar la proporción
########################################################
# Se calcula el tamaño que debe de tener la muestra para que, al
# estimar una proporción poblacional con la proporción muestral,
# el error no exceda una cantidad específica, suponiendo que o
# bien que se ha hecho un muestreo previo o bien si no.
########################################################
########################################################
# Sección modificable por el usuario
########################################################

# Error considerado
error<-0.02
# Nivel de significancia
alfa<-0.05
# Para un error unilateral (U) o bilateral (D)
inter<-'D'
# Para indicar si hubo una muestra previa
previo<-TRUE
# Tamaño de la muestra previa
n<-1600
# casos favorables de la muestra previa
x<-NULL
# proporción estimada (puede darse en vez de x)
p<-0.32

########################################################
# Sección que realiza el procedimiento
########################################################

# Valor crítico
if(inter=='D'){
   zalfa<-qnorm(1-alfa/2)
}else{
   zalfa<-qnorm(1-alfa)
}

# Valor estimado de p y q
if(previo){
   if(is.null(p)){
      p<-x/n
   }
}else{
   p<-1/2
}
q<-1-p

# Tamaño de la muestra
n<-ceiling((zalfa/error)^2*p*q)

########################################################
# Muestra de los resultados
########################################################

n

