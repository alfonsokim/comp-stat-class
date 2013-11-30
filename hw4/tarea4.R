#### Tarea 4 Estadistica Computacional
### Itzel Muñoz - Alfonso Kim

## 1
azul <- c(4,69,87,35,39,79,31,79,65,95,68,62,70,80,84,79,66,75,59,77,36,
          86,39,85,74,72,69,85,85,72)
rojo <- c(62,80,82,83,0,81,28,69,48,90,63,77,0,55,83,85,54,72,58,68,88,83,
          78,30,58,45,78,64,87,65)

datos = data.frame(
    id = c(rep(0, 20), rep(1, 10)),
    azul=azul, rojo=rojo
    )

attach(datos)

#a)

#1)
n <- 20
dbarra <- mean(azul[1:20]) - mean(rojo[1:20])

d <- array(0,20)
for (i in 1:20){
    d[i] <- azul[i] - rojo[i]
}

suma <- 0
for (i in 1:20){
    suma <- suma + (d[i] - dbarra)^2
}

Sd<- sqrt(suma/(n-1))

gamma <- .05
t_alfa <- pt(1-(gamma/2),n-1)

c1 <- -t_alfa
c2 <- t_alfa

tobs <- dbarra / (Sd*sqrt(1/n))

# por lo tanto, tobs no pertenece a la región de rechazo 

#2)

linf <- dbarra - t_alfa*Sd*sqrt(1/n)
lsup <- dbarra + t_alfa*Sd*sqrt(1/n)

# el IC de 95% para delta es (linf, lsup)


#b)

n <- 10
abarra <- mean(azul[21:30]) 
rbarra <- mean(rojo[21:30])
# se desconocen las varianzas pero se asumen iguales
# es decir, sigma1 = sigma2


suma <- 0
for (i in 1:10){
    suma <- suma + (azul[i] - abarra)^2
}

Sa_cuad <- suma/(n-1)


suma <- 0
for (i in 1:10){
    suma <- suma + (rojo[i] - rbarra)^2
}

Sr_cuad <- suma/(n-1)

Sp<- sqrt( ((n-1)*Sa_cuad + (n-1)*Sr_cuad) / (2*n-2) )


tobs <- (abarra-rbarra) /(Sp*sqrt(1/n+1/n))

gamma <- 0.05
t_alfa <- pt(1-(gamma/2),2*n-2)


c1 <- -t_alfa
c2 <- t_alfa

# por lo tanto no rechazas la hipótesis nula


## 3
?read.table
ozone <- read.table("ozone.txt", header=T, sep=" ")
head(ozone)

?lm
lm.model <- lm(maxO3 ~ T9 + T12 + T15 + Ne9 + Ne12 + Ne15 + 
                   Wx9 + Wx12 + Wx15 + maxO3y, data=ozone)

## Analisis de error
summary(lm.model)

# Segun los valores t  sólo las variables Ne9 y max03y 
# contribuyen a la prediccion de maxO3

## maxO3 = 12.24 - 2.19Ne9 + 0.35maxO3y

# Analisis de Residuales
residuals <- resid(lm.model)
plot(lm.model$fitted.values, residuals, 
     ylab="Residuales", xlab="O3", 
     main="Analisis de Residuales") 
abline(0, 0)

summary(ozone)
ozone$wind <- NULL
ozone$rain <- NULL

ozone.corr <- data.frame(cor(ozone, use="everything"))
class(ozone.corr)
subset(ozone.corr, ozone.corr$T9 > 0.6)



### ==========================================================
## 4: Aleatorios

#b)

a <- 5
b <- 2
U <- runif(20000,0,1)
X <- b/(1-U)^(1/a)

#c)

hist(X,probability=TRUE,xlim=c(b,8),ylim=c(0,3))
lines(density(X),col="blue")
# se observa una densidad sesgada 

#d)

library(VGAM)

Y <- rpareto(2000,2,5)
mean(Y)
var(Y)
mean(X)
var(X)
# se observa que los valores de los momentos teóricos y los simulados son cercanos
y<-seq(0,8,0.02)
lines(y,dpareto(y,2,5),col="red")
# se observa que las curvas de densidad se traslapan

#e)

Xmax <- max(X)
Xmin <- min(X)

m <- (Xmax - Xmin) / 5


o1 <- 0
o2 <- 0
o3 <- 0
o4 <- 0


# con 4 intervalos

for (i in 1:20000)
{
    if (X[i] < Xmin + m)
    {
        o1 <- o1 + 1
    }
    else
    {
        if (X[i] < Xmin + 2*m)
        {
            o2 <- o2 + 1
        }
        else
        {
            if (X[i] < Xmin + 3*m)
            {
                o3 <- o3 + 1
            }
            else 
            {
                o4 <- o4 + 1
            }
        }
    }
}


Fcd <- function(c,d){(b/c)^a - (b/d)^a} # F(d)-F(c) c<d


esp1 <- Fcd(Xmin,Xmin+m)*20000
esp2 <- Fcd(Xmin+m,Xmin+2*m)*20000
esp3 <- Fcd(Xmin+2*m,Xmin+3*m)*20000
esp4 <- Fcd(Xmin+3*m,Xmax)*20000


obs <- c(o1,o2,o3,o4)
esp <- c(esp1,esp2,esp3,esp4)
suma <- 0
for(i in 1:4)
{
    suma <- suma + (obs[i] - esp[i])^2/esp[i]
}

pchisq(.95,3) # alfa = .05
suma
# el valor del suma es mayor al teórico por lo tanto se rechaza la hipótesis nula de que
# la distribución pareto "ajusta" los datos



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
