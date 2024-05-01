# P18_Prueba_de_independencia_y_homogeneidad_01.r
# Contraste de independencia
########################################################
# .
#
# Se leerá de un archivo externo los datos necesarios y se
# calcularán los valores para el resultado de la prueba de
# hipótesis.
########################################################
########################################################
# Sección modificable por el usuario
########################################################

# Lectura de la base de datos
datos<-read.csv("DB40_Problema_109.csv",sep=";",encoding="UTF-8")

# Selección de las (2) variables de interés
varInteres<-c("Preferencia","Estado")

# Selección de la columna de frecuencias
# Si aún hay que contar, se deja en NULL
varFrecuencia<-"Frecuencia"

# Tipos de prueba a realizar
#  1. Prueba chi-cuadrada.
#  2. Prueba G sin correción de Williams.
#  3. Prueba G con corrección de Williams.
#  4. Prueba chi-cuadrada con corrección de Yates (sólo para tablas 2x2).
#  5. Prueba "exacta" de Fisher (sólo para tablas 2x2), bilateral.
#  6. Prueba "exacta" de Fisher (sólo para tablas 2x2), unilateral: menor que.
#  5. Prueba "exacta" de Fisher (sólo para tablas 2x2), unilateral: mayor que.
pruebas<-c(1)

########################################################
# Sección que realiza el procedimiento
########################################################

g.test <- function(x, correct="none",
                   p = rep(1/length(x), length(x)))
{
  DNAME <- deparse(substitute(x))
  if (is.matrix(x)) {
    if (min(dim(x)) == 1) 
      x <- as.vector(x)
  }

  if (any(x < 0) || any(is.na(x))) 
    stop("Todas las entradas deben de ser no negativas")
  if ((n <- sum(x)) == 0) 
    stop("Al menos una entrada debe de ser positiva")
  nrows<-nrow(x)
  ncols<-ncol(x)
  sr <- apply(x,1,sum)
  sc <- apply(x,2,sum)
  E <- outer(sr,sc, "*")/n
  # Calcular G
  g <- 0
  for (i in 1:nrows){
    for (j in 1:ncols){
      if (x[i,j] != 0) g <- g + x[i,j] * log(x[i,j]/E[i,j])
    }
  }
  q <- 1
  if (correct=="williams"){ # Hacer la corrección de Williams
    row.tot <- col.tot <- 0    
    for (i in 1:nrows){ row.tot <- row.tot + 1/(sum(x[i,])) }
    for (j in 1:ncols){ col.tot <- col.tot + 1/(sum(x[,j])) }
    q <- 1+ ((n*row.tot-1)*(n*col.tot-1))/(6*n*(ncols-1)*(nrows-1))
  }
  STATISTIC <- G <- 2 * g / q
  PARAMETER <- (nrow(x)-1)*(ncol(x)-1)
  PVAL <- 1-pchisq(STATISTIC,df=PARAMETER)
  base<-"Log likelihood ratio (G-test) test of independence "
  if(correct=="none")
    METHOD<-paste(base,"without correction")
  if(correct=="williams")
    METHOD<-paste(base,"with Williams' correction")
  names(STATISTIC) <- "Log likelihood ratio statistic (G)"
  names(PARAMETER) <- "X-squared df"
  names(PVAL) <- "p.value"
  structure(list(statistic=STATISTIC,parameter=PARAMETER,p.value=PVAL,
                 method=METHOD,data.name=DNAME, observed=x, expected=E),
            class="htest")
}

tablaDobleEntrada<-function(datos,varInteres,pruebas){
  tbl1<-table(datos[,varInteres])
  listaPruebas<-NULL
  for (i in pruebas){
    if (i==1) listaPruebas<-c(listaPruebas,
                              list(chisq.test(tbl1,correct=FALSE)))
    if (i==2) listaPruebas<-c(listaPruebas,list(g.test(tbl1)))
    if (i==3) listaPruebas<-c(listaPruebas,
                              list(g.test(tbl1,correct="williams")))
    if (i==4) listaPruebas<-c(listaPruebas,list(chisq.test(tbl1,correct=TRUE)))
    if (i==5) listaPruebas<-c(listaPruebas,list(fisher.test(tbl1)))
    if (i==6) listaPruebas<-c(listaPruebas,
                              list(fisher.test(tbl1,alternative="less")))
    if (i==7) listaPruebas<-c(listaPruebas,
                              list(fisher.test(tbl1,alternative="greater")))
  }
  return(list(tabla=tbl1,listaPruebas=listaPruebas))
}

if(length(varInteres)!=2) stop("Deben ser dos variables de interés")

if(!is.null(varFrecuencia)){
  datos<-datos[rep(row.names(datos), datos$Frecuencia),
                which(colnames(datos)!=varFrecuencia)]
}

listaR<-tablaDobleEntrada(datos,varInteres,pruebas)

########################################################
# Sección que muestra los resultados
########################################################

listaR
