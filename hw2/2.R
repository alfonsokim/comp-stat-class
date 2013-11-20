setwd("~/r-workspace/comp-stats/hw2/")
data <- read.delim("finanzas.dat")
colnames(data) <- c("Compania", "Ganancia", "Libros", "Precio")

library(ggplot2)
ggplot(data, aes(x=Libros,y=Ganancia)) + 
    geom_point() + geom_smooth()

## =====================================================
## ==       2. Datos de acciones de companias         ==
## -----------------------------------------------------
# a. Tablas de contingencia
# Para el valor en libros
with(data, table(Compania, cut(data$Libros, breaks=4)))

# Para el precio por accion
with(data, table(Compania, cut(data$Precio, breaks=4)))

# a.1 Frecuencias absolutas por valor en Libros
factor.Ganancia <- factor(cut(data$Libros, breaks=4))
table.Ganancia <- as.data.frame(table(factor.Ganancia))
table.Ganancia <- transform(table.Ganancia, cumFreq=cumsum(Freq), relative=prop.table(Freq))
table.Ganancia
# La mayor dispersion esta cuando las ganancias estan entre 
# (3.08, 6.58]. No hay unidades...

# a.1 Frecuencias absolutas por precio de acciones
factor.Precio <- factor(cut(data$Precio, breaks=4))
table.Precio <- as.data.frame(table(factor.Precio))
table.Precio <- transform(table.Precio, cumFreq=cumsum(Freq), relative=prop.table(Freq))
table.Precio

## -----------------------------------------------------
# b. Graficas de dispersion
pairs(~Ganancia+Libros+Precio, data)
### ====================================================


## =====================================================
## ==       2. Datos de manchas solares               ==
## -----------------------------------------------------
## a. Promedio, stddev y cuartiles
ssn <- read.delim("spot_num.txt", sep=",")
head(ssn, 20)
colnames(ssn)
subset(ssn, YEAR==1749)
ssn.stats.1 <- aggregate(SSN ~ YEAR, data=ssn, 
                         FUN=function(x) c(SUM=sum(x), MEAN=mean(x), SD=sd(x),
                                           QUANT=quantile(x, probs=c(0.25, 0.50, 0.75))))


ssn.stats <- data.frame(year=ssn.stats.1$YEAR)
# Llenar los valores faltantes en el data.frame
ssn.stats$sum <- ssn.stats.1$SSN[,1]
ssn.stats$mean <- ssn.stats.1$SSN[,2]
ssn.stats$sd <- ssn.stats.1$SSN[,3]
ssn.stats$quant.25 <- ssn.stats.1$SSN[,4]
ssn.stats$quant.50 <- ssn.stats.1$SSN[,5]
ssn.stats$quant.75 <- ssn.stats.1$SSN[,6]

head(ssn.stats)

## b. Variable ciclo
ssn.stats$cycle <- c(6:11, rep(1:11, 23), 1:6)

# Id Ciclo, para las etiquetas de las graficas
c.ids <- c()
for(i in 2:24){
    c.ids <- c(c.ids, paste("Ciclo", rep(i, 11), sep="."))
}
ssn.stats$cycle.id <- c(paste("Ciclo", rep(1, 6), sep="."), 
                        c.ids, 
                        paste("Ciclo", rep(25, 6), sep="."))

head(ssn.stats, 40)

## c. Contar numero de manchas
ssn.cycles.1 <- aggregate(sum ~ cycle.id, data=ssn.stats, 
                          FUN=function(x) c(sum=sum(x)))
str(ssn.cycles.1)
# c.1: EL que tiene menor numero de manchas
ssn.cycles.1[which.min(ssn.cycles.1$sum), ]
# R: El ciclo 25

# c.1: EL que tiene mayor numero de manchas
ssn.cycles.1[which.max(ssn.cycles.1$sum), ]
# R: El ciclo 20

## d. Grafica con el numero de manchas por ciclo
df.ssn.1 <- data.frame(x=ssn.stats$cycle, val=ssn.stats$sum, ciclo=ssn.stats$cycle.id)
ssn.plot.1 <- ggplot(data=df.ssn.1, aes(x=x, y=val)) + geom_line(aes(colour=ciclo)) +
    xlab("Longitud del ciclo") +
    ylab("Numero de manchas solares") +
    ggtitle("Numero de manchas solares a por ciclo")
ssn.plot.1

## e. Grafica con el numero de manchas por anio del ciclo
df.ssn.2 <- data.frame(x=ssn.stats$cycle, val=ssn.stats$sum, 
                       anio=paste("y", ssn.stats$cycle, sep="."))
ssn.plot.2 <- ggplot(data=df.ssn.2, aes(x=x, y=val)) + geom_line(aes(colour=anio)) +
    xlab("Año por cada ciclo") +
    ylab("Numero de manchas solares") +
    ggtitle("Numero de manchas solares por año del ciclo")
ssn.plot.2

## la funcion par no es compatible con ggplot.
## se usa grid.arrange que se encuentra en la libreria gridExtra
## descomentar la siguiente linea para instalar la libreria
#install.packages("gridExtra")
library(gridExtra)
grid.arrange(ssn.plot.1, ssn.plot.2, ncol=2)



## =====================================================
## ==      5. Datos de Calificaciones y Jueces        ==
## -----------------------------------------------------
## Al archivo calificaciones.csv le agregamos un nuevo
## encabezado para las calificaciones ordinales de
## cada juez.
## -----------------------------------------------------
calificaciones <- read.delim("calificaciones.csv", sep=",")
dim(calificaciones)

# Ordenar alfabeticamente por nombres de Jueces, primero obtenemos 
# los nombres de los jueces sin la columna "Equipo"
header <- names(calificaciones)[2:length(names(calificaciones))]

# Despues los ordenamos
header <- sort(header)

# Luego volvemos a pegarle la columna Equipo
header <- c("Equipo", header)

# Luego reintegramos el resto de las calificaciones del dataframe
# original en un dataframe con los nombres ordenados
calif <- data.frame(temp=1:5) #columna temporal para formar el data.frame
for(name in header){
    calif[[name]] <- calificaciones[[ name ]]
}
#Quitar la columna temporal
calif <- subset(calif, select = -c(temp))

# Renombrar las columnas con JuezX. Dado que cada juez imparte 2 calificaciones
# a cada uno se le asigna el sufijo "Ord" y "Cont" dependiendo de la calificacion
col.n <- c()
for(n in 1:22){
    if (n %% 2 == 1) {
        col.n <- c(col.n, paste(ceiling(n/2), "Cont", sep="."))
    } else {
        col.n <- c(col.n, paste(ceiling(n/2), "Ord", sep="."))
    }
}

# Formamos los nuevos nombres
new.colnames <- paste("Juez", col.n, sep="")

# Renombramos el dataframe
colnames(calif) <- c("Equipo", new.colnames)
names(calif)

# Ordenar y renombrar los equipos
calif <- calif[order(calif$Equipo), ]
rownames(calif) <- c("AAA", "BBB", "CCC", "DDD", "EEE")
calif["AAA",]

# a.1: ¿que equipo es el mejor calificado respecto a la continua?
# Escojemos los nombres de las columnas que contienen calificacion continua
cont <- subset(colnames(calif), grepl("Cont", colnames(calif)))
# Sacamos el equipo que tiene las calidicaciones continuas mas altas
which.max(rowMeans(calif[, colnames(calif) %in% cont]))
# R: El equipo CCC

# a.2: ¿el peor?
which.min(rowMeans(calif[, colnames(calif) %in% cont]))
# R: El equipo AAA
which(mean(calif[, colnames(calif) %in% cont]))

# El equipo en el intermedio
means <- rowMeans(calif[, colnames(calif) %in% cont])
which.min(abs(means - mean(means)))
# R: BBB


# b. Lo mismo pero con las ordinales
ord <- subset(colnames(calif), grepl("Ord", colnames(calif)))
# Sacamos el equipo que tiene las calidicaciones ordinales mas bajas
which.min(rowMeans(calif[, colnames(calif) %in% ord]))
# R: El equipo CCC

# a.2: ¿el peor?
which.max(rowMeans(calif[, colnames(calif) %in% ord]))
# R: El equipo AAA

# El equipo en el intermedio
means <- rowMeans(calif[, colnames(calif) %in% ord])
which.min(abs(means - mean(means)))
# R: BBB

# c.1: Equipo con mayor desacuerdo = mayor desviacion estandar
which.max(apply(calif[, colnames(calif) %in% cont], 1, sd))
# R: EEE

# c.2: Equipo mas consistente = menor desviacion estandar
which.min(apply(calif[, colnames(calif) %in% cont], 1, sd))
# R: CCC

# d.1: ¿Que juez diria usted fue el mas duro?
which.min(apply(calif[, colnames(calif) %in% cont], 2, mean))
#R: Juez 6

# d.2: ¿Que juez diria usted fue el mas barco?
which.max(apply(calif[, colnames(calif) %in% cont], 2, mean))
#R: Juez 4

# E: Cual de los jueces presento mayor variacion?
which.max(apply(calif[, colnames(calif) %in% cont], 2, sd))
# R: Juez 5



