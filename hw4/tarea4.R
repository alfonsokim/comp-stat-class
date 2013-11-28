#### Tarea 4 Estadistica

## 1
azul<-c(4,69,87,35,39,79,31,79,65,95,68,62,70,80,84,79,66,75,59,77,36,
        86,39,85,74,72,69,85,85,72)
rojo<-c(62,80,82,83,0,81,28,69,48,90,63,77,0,55,83,85,54,72,58,68,88,83,
        78,30,58,45,78,64,87,65)

datos = data.frame(
    identificador = c(rep(0, 20), rep(1, 10)),
    azul=azul, rojo=rojo
    )

## 3
?read.table
setwd("~/r-workspace/comp-stats/hw4")
ozone <- read.table("ozone.txt", header=T, sep=" ")
head(ozone)

?lm
lm.model <- lm(maxO3 ~ T9 + T12 + T15 + Ne9 + Ne12 + Ne15 + 
                   Wx9 + Wx12 + Wx15 + maxO3y, data=ozone)

coef(lm.model)
str(lm.model)
#Ecuacion: maxO3 = 12.24 -0.01T9 + 2.22T12 + 0.56T15 - 
#                  2.19Ne9 - 0.42N12 + 0.18Ne15 + 
#                  0.94Wx9 + 0.03Wx12 + 0.42Wx15 + 0.35max03y


### ==========================================================
## 4: Aleatorios
numbers <- read.csv("aleatorios.txt", header=F)
summary(numbers)
numbers.fix <- subset(numbers, V1 < 1)
summary(numbers.fix)

## Prueba chi-cuadrada
# Paso 1: Dividir en intervalos
bins <- apply(matrix(0:10), 1, function(i) i/10)
numbers.table <- table(cut(as.matrix(numbers.fix),
                           breaks=bins, right=F))
numbers.df <- as.data.frame(numbers.table)
# Paso 2: Frecuencia Esperada
numbers.df$FE <- nrow(numbers.fix) / 10
# Paso 3: La frecuencia Observada esta en la columna "Freq"
# Paso 4: Valor de Chi
numbers.df$Chi <- ((numbers.df$FE - numbers.df$Freq) ^ 2) / numbers.df$FE
# Paso 5: Suma de los valores Cho
chi.sq <- sum(numbers.df$Chi)
# Paso 6: Chi en valor en tablas con alfa=0.05 y n-1 g.l.
chi.sq.2 <- qchisq(p=0.95, df=9)
# Verificar
chi.sq < chi.sq.2

## Prueba Kolmogorov-Smirnov
# Paso 1: Ordenar los numeros
numbers.sort <- data.frame(x=sort(as.matrix(numbers.fix)))
# Paso 2: Distribucion Acumulada
numbers.sort$DA <- apply(matrix(1:nrow(numbers.sort)), 1, 
                         function(i) i / nrow(numbers.sort))
# Paso 3: Diferencia del valor - la distribucion
numbers.sort$KS <- numbers.sort$DA - numbers.sort$x
# Paso 4: Valor observado
ds <- max(abs(numbers.sort$KS))
# No hay implementacion de la distribucion KS en R.
# segun http://www.real-statistics.com/statistics-tables/kolmogorov-smirnov-table/
ds.2 <- 1.36 / sqrt(nrow(numbers.sort))

ds < ds.2
