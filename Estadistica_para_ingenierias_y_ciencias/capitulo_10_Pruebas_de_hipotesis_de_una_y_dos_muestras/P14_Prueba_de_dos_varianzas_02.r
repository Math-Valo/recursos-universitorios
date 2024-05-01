# P14_Prueba_de_dos_varianzas_02.r
# Contrasta la hipótesis para la proporción de varianzas de
# dos poblaciones
########################################################
# Se realizan los cálculos para una prueba de hipótesis de
# dos muestras para la proporción de varianzas poblacionales
# usando la proporción de varianzas muestrales, suponiendo
# que provienen de poblaciones normales independientes. Se
# calcula el P-valor y la región de rechazo para un nivel de
# significancia con ayuda del estadístico F de fisher. El
# nivel de significancia es opcional. Si se asigna un nivel
# de significancia, la salida mostrará un veredicto de la
# prueba.
#
# Se leerá de un archivo externo los datos necesarios y se
# calcularán los valores para el resultado de la prueba de
# hipótesis.
########################################################
########################################################
# Sección modificable por el usuario
########################################################

# Lectura de la base de datos
datos<-read.csv("DB45_Problema_114.csv",sep=";",encoding="UTF-8")

# Selección de las variables de interés
varInteres<-c("Errores.PKLineas")

# Selección de variables con los niveles deseados de
# comparación
varSel<-c("País")

# Nivel de significancia
alfa<-NULL

# Tipo de prueba: cola superior (S), cola inferior (I) o dos colas (D)
cola<-'D'

########################################################
# Sección que realiza el procedimiento
########################################################

w<-data.frame(factor(datos[,varSel]))
names(w)<-varSel
varBin<-c(varSel)
datos<-data.frame(datos,w)

# Verificación de las variables de agrupacion binarias
# realmente lo son.
if (length(varBin)<1){
  stop("Debe al menos indicar una variable binaria")
}else{
  sonbinarios<-ifelse(length(table(datos[,varBin]))!=2,1,0)
}
if (sonbinarios!=0)  stop("La variable no es binaria")

valores<-unlist(datos[,c(varInteres)])
variables<-factor(rep(varInteres,each=dim(datos)[1]))

agrupaciones<-data.frame(datos[1:dim(datos)[1],varBin])
names(agrupaciones)<-varBin
datos2<-data.frame(agrupaciones,variable=variables,valor=valores)

# Función que recibe dos vectores (en realidad, lo recibe como LISTAS)
# y realiza el test de dos muestras para la diferencia de medias
propVar<-function(l,cola,alfa=0.05){
  x<-l[[1]][!is.na(l[[1]])]
  y<-l[[2]][!is.na(l[[2]])]
  n1<-length(x)
  n2<-length(y)
  m1<-mean(x)
  m2<-mean(y)
  s1<-var(x)
  s2<-var(y)
  v1<-n1-1
  v2<-n2-1
  if ( n1< 2 | n2 < 2){
    r<-data.frame(n1=n1,n2=n2,
                  media1=round(m1,7),media2=round(m2,7),
                  varianza1=round(s1,7),varianza2=round(s2,7),
                  v1=v1,v2=v2,
                  alpha=alfa,
                  PValor=NA,
                  Estadistico=NA,
                  RegionRechazo=NA
                  )
  }else{
    estadistico = round(s1/s2,7)
    if(cola=='D'){
      regionL<-round(qf(alfa/2,v1,v2),7)
      regionU<-round(qf(alfa/2,v1,v2,lower.tail=F),7)
      pvalor<-2*min(pf(estadistico,v1,v2,lower.tail=F),
                    pf(estadistico,v1,v2))
      r<-data.frame(n1=n1,n2=n2,
                    media1=round(m1,7),media2=round(m2,7),
                    varianza1=round(s1,7),varianza2=round(s2,7),
                    v1=v1,v2=v2,
                    alpha=alfa,
                    PValor=round(pvalor,7),
                    Estadistico=estadistico,
                    RegionRechazo=paste("<",regionL,"y >",regionU)
                    )
    }else{
      if(cola=='I'){
        region<-round(qf(alfa,v1,v2),7)
        pvalor<-round(pf(estadistico,v1,v2),7)
        r<-data.frame(n1=n1,n2=n2,
                      media1=round(m1,7),media2=round(m2,7),
                      varianza1=round(s1,7),varianza2=round(s2,7),
                      v1=v1,v2=v2,
                      alpha=alfa,
                      PValor=pvalor,
                      Estadistico=estadistico,
                      RegionRechazo=paste("<",region)
                      )
      }else{
        region<-round(qf(alfa,v1,v2,lower.tail=F),7)
        pvalor<-round(pf(estadistico,v1,v2,lower.tail=F),7)
        r<-data.frame(n1=n1,n2=n2,
                      media1=round(m1,7),media2=round(m2,7),
                      varianza1=round(s1,7),varianza2=round(s2,7),
                      v1=v1,v2=v2,
                      alpha=alfa,
                      PValor=pvalor,
                      Estadistico=estadistico,
                      RegionRechazo=paste(">",region)
                      )
      }
    }
  }
  return(r)
}

listaG<-datos2[,c(varBin,"valor")]
listaG1<-split(listaG$"valor", listaG[,varBin])
if(as.matrix(listaG)[1]==names(listaG1[2])){
  listaG1=listaG1[c(2,1)]
}
listaG1<-list(listaG1)
names(listaG1)<-varInteres

if(is.null(alfa)){
  Test<-t(sapply(listaG1,propVar,cola))
}else{
  Test<-t(sapply(listaG1,propVar,cola,alfa=alfa))
}

t1<-as.data.frame.table(table(datos2[,c("variable")]))

identif<-t1[t1$Freq>0,]
# Para renombrar Var1 por variable
names(identif)[names(identif)=="Var1"]<-"variable"

Test<-data.frame(identif,Test)

if(!is.null(alfa)){
  Test$Resultado<-ifelse(Test$PValor>=alfa,
                         "No se rechaza H0","Se rechaza H0")
  Test$Resultado[is.na(Test$Resultado)]<-"Pocos datos"
}


########################################################
# Muestra de los resultados
########################################################

Test
