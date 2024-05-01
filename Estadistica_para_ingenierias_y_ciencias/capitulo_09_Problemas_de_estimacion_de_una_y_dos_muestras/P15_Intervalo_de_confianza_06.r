# P15_Intervalo_de_confianza_06.r
# Intervalo de la diferencia de medias de poblaciones normales.
########################################################
# Se calcula un intervalo de confianza, bilateral para la
# diferencia de medias poblacionales normales, usando la
# diferencia de las medias muestrales, con (1-alpha)100% de
# seguridad. Se hace uso del estadístico t, independientemente
# de si se conoce o no las desviaciones poblacionales o de si
# las muestras son lo suficientemente grande, por lo que se
# supone que las poblaciones se distribuyen de forma
# aproximadamente normal.
#
# Se leerá de un archivo externo los datos necesarios y se
# calcularán los valores requeridos para obtener el intervalo
# de confianza.
########################################################
########################################################
# Sección modificable por el usuario
########################################################

# Lectura de la base de datos
datos<-read.csv("DB03_Problema_40.csv",sep=";",encoding="UTF-8")

# Selección de las variables de interés
# varInteres<-c("Altura.cm","Peso.Kg")
varInteres<-c("Peso.g")

# Selección de categorización de las variables
# Si no se utilizan variables de agrupación obligatoriamente
# se exige que se coloce varAgrupación como NULL
# 
# varAgrupacion<-c("Árbol")
varAgrupacion<-NULL

# Selección de variables con los niveles deseados de
# comparación.
# Si no se colocan niveles de comparación se supone
# que la variable es binaria.
#
# varSel<-list("Sexo")
# varSel<-list("Sexo",c("Ciudad","3","4"))
# varSel<-list(c("Ciudad","1","2"))
varSel<-list("Árbol")

# Indique el nivel de significancia
alfa<-0.05

########################################################
# Sección que realiza el procedimiento
########################################################

# Creación de nuevas variables con los niveles propuestos.
w<-data.frame(row.names=1:dim(datos)[1])
varBin<-as.character()
for (i in 1:length(varSel)){
    nom<-varSel[[i]][1]
    x<-factor(datos[,nom])
    if (length(varSel[[i]])>1){
        sufijo<-paste(varSel[[i]][2:3],collapse="_")
        nom<-paste(nom,".",sufijo,sep="")
        x1<-factor(ifelse(x %in% varSel[[i]][2:3],as.character(x),NA))
        x1<-data.frame(factor(x1))
    }else{
        x1<-x
        x1<-data.frame(x)
    }
    names(x1)<-nom
    varBin<-c(varBin,nom)
    w<-data.frame(w,x1)
}
datos<-data.frame(datos,w)

# Verificación de las variables de agrupacion binarias
# realmente lo son.
if (length(varBin)<1){
    stop("Debe al menos indicar una variable binaria")
}else{
    sonbinarios<-sapply(1:length(varBin),function(i) if(length(table(datos[,varBin[i]]))!=2) return(1) else return(0))
}
if (sum(sonbinarios)!=0)  stop("Alguna variable no es binaria")


valores<-unlist(datos[,c(varInteres)])
variables<-factor(rep(varInteres,each=dim(datos)[1]))
agrupaciones<-data.frame(datos[rep(1:dim(datos)[1],length(varInteres)),c(varAgrupacion,varBin)])
names(agrupaciones)<-c(varAgrupacion,varBin)
datos2<-data.frame(agrupaciones,variable=variables,valor=valores)

# Función que recibe dos vectores (en realidad, lo recibe como LISTAS)
# y calcula el intervalo de confianza para la diferencia de medias y
# muestra si hay diferencia probando primero si las varianzas son
# iguales y utilizando el estadístico de acuerdo a este resultado.
difMedias<-function(l,alfa=0.05){
    x<-l[[1]][!is.na(l[[1]])]
    y<-l[[2]][!is.na(l[[2]])]
    n1<-length(x)
    n2<-length(y)
    m1<-mean(x)
    m2<-mean(y)
    diferencia<-m1-m2
    if ( n1< 2 | n2 < 2){
        r<-data.frame(n1=n1,n2=n2,
                      media1=m1,media2=m2,
                      limInf=NA,
                      diferencia=m1-m2,
                      limSup=NA,
                      valorPMedia=NA,
                      valorPVar=NA,
                      varIgual=NA)
    }else{ 
    r1<-var.test(x,y,conf.level=1-alfa)
    varIgual<-(r1$p.value>=alfa)
    r2<-t.test(x,y,conf.level=1-alfa,var.equal=varIgual)
    r<-data.frame(n1=n1,n2=n2,
                  media1=m1,media2=m2,
                  limInf=r2$conf.int[1],
                  diferencia=-diff(r2$estimate),
                  limSup=r2$conf.int[2],
                  valorPMedia=r2$p.value,
                  valorPVar=r1$p.value,
                  varIgual=varIgual)
    }
    return(r)
}


# Función para dividir en grupos binarios la lista de variables
dividegrupos<-function(i,l,vB) split(l[[i]]$valor,l[[i]][,vB])
listdiv<-function(i,l,vB) {
    nombres<-names(l[[i]])
    l2<-lapply(1:length(l[[i]]),dividegrupos,l[[i]],vB[i])
    names(l2)<-nombres
    return(l2)
}

if(!is.null(varAgrupacion)) lista1<-as.list(datos2[,c(varAgrupacion,"variable")]) else lista1<-datos2$variable
listaG<-lapply(1:length(varBin),function(i) split(datos2[,c(varBin[i],"valor")],lista1,drop=TRUE))
listaG1<-lapply(1:length(listaG),listdiv,listaG,varBin)
r<-lapply(1:length(listaG1),function(i) t(sapply(listaG1[[i]],difMedias,alfa)))
names(r)<-varBin

rFin<-r[[1]]
f1<-function(i){ rFin<<-rbind(rFin,r[[i]]) }
if (length(r)>=2) invisible(lapply(2:length(r),f1) )
t1<-as.data.frame.table(table(datos2[,c(varAgrupacion,"variable")]))
identif<-t1[t1$Freq>0,]
d<-dim(rFin)
n<-colnames(rFin)
rFin<-data.frame(matrix(unlist(rFin),d))
names(rFin)<-n
rFin<-data.frame(Tipo.de.Grupo=rep(names(r),each=dim(r[[1]])[1]),identif[rep(1:dim(identif)[1],length(r)),],rFin)
rFin<-rFin[with(rFin,n1!=0 | n2!=0),]
rFin$Resultado<-ifelse(rFin$valorPMedia>=alfa,"No signif dif de medias","Signif dif de medias")
rFin$Resultado[is.na(rFin$Resultado)]<-"Poco datos"
rFin$varIgual<-ifelse(rFin$varIgual==1,"Var no diferentes","Var diferentes")

########################################################
# Sección que muestra los resultados
########################################################

rFin

