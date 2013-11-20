# ejercicio 1

cable <- read.table("C:/Users/Itzel/Documents/My Dropbox/ITAM/Est Computacional/Tareas/Tarea 2/Datos/cableTV.dat",header=TRUE)
#head(cable)
attach(cable)

# a)

# 1) y 2)

hist_tvtot <- hist(tvtot,col="blue")
text(max(tvtot)-3, 5.5 , paste("Máximo =", max(tvtot), sep = " "))
text(min(tvtot)+5, 2.5 , paste("Mínimo =", min(tvtot), sep = " "))
text(mean(tvtot), 3 , paste("Media =", mean(tvtot), sep = " "))

n <- length(hist_tvtot$breaks)
tabla_tvtot <- cbind(hist_tvtot$breaks[-n],hist_tvtot$breaks[-1],hist_tvtot$counts)
dimnames(tabla_tvtot)[[2]]<-c("Linf","Lsup","f")
print(tabla_tvtot)

hist_renta <- hist(renta,col="red")
text(max(renta), 2 , paste("Máximo =", max(renta), sep = " "))
text(min(renta)+6, 3 , paste("Mínimo =", min(renta), sep = " "))
text(mean(renta), 10 , paste("Media =", mean(renta), sep = " "))
n <- length(hist_tvtot$breaks)
tabla_renta <- cbind(hist_renta$breaks[-n],hist_renta$breaks[-1],hist_renta$counts)
dimnames(tabla_renta)[[2]]<-c("Linf","Lsup","f")
print(tabla_renta)

hist_valor <- hist(valor,col="green")
text(max(valor)-5000, 4 , paste("Máximo =", max(valor), sep = " "))
text(min(valor)+6000, 2.5 , paste("Mínimo =", min(valor), sep = " "))
text(mean(valor), 9 , paste("Media =", mean(valor), sep = " "))
n <- length(hist_valor$breaks)
tabla_valor <- cbind(hist_valor$breaks[-n],hist_valor$breaks[-1],hist_valor$counts)
dimnames(tabla_valor)[[2]]<-c("Linf","Lsup","f")
print(tabla_valor)

# 3)

boxplot(tvtot,horizontal=F,xlab="tvtot") # no hay valores atípicos
boxplot(renta,horizontal=F,xlab="renta") # sí hay valores atípicos
boxplot(valor,horizontal=F,xlab="valor") # no hay valores atípicos

# b)

horas_tv <- data.frame(cable[,8])
tvtot_50 <- length(which(horas_tv >= 50))/40*100 # el 40%

tvtot_40por <- quantile(tvtot, .4)


# c)

quantile(renta, .8)

# d)

inf <- quantile(valor, .25)
sup <- quantile(valor, .5)
inf
sup
# entre 162,187.2 y 216,393

# e)

col_valor <- data.frame(cable[,2],cable[,10])
names(col_valor) <- c("colonia","valor")
col1_valor <- subset(col_valor,colonia == 1)
summary(col1_valor$valor)
hist(col1_valor$valor, main = "Histograma de valor para colonia 1", xlab = "valor")
col2_valor <- subset(col_valor,colonia == 2)
summary(col2_valor$valor)
hist(col2_valor$valor, main = "Histograma de valor para colonia 2", xlab = "valor")
# Sí hay diferencia en la variable valor por colonia, pues los estadísticos descriptivos
# son diferentes dependiendo de la colonia, esto también se observa en los histogramas
# ya que en la colonia 1 los datos están sesgados hacia la derecha, mientras que
# en la colonia 2 están más centrados


# f)


col_renta <- data.frame(cable[,2],cable[,7])
names(col_renta) <- c("colonia","renta")
col1_renta <- subset(col_renta,colonia == 1)
summary(col1_renta$renta)
hist(col1_renta$renta, main = "Histograma de renta para colonia 1", xlab = "renta")
col2_renta <- subset(col_renta,colonia == 2)
summary(col2_renta$renta)
hist(col2_renta$renta, main = "Histograma de renta para colonia 2", xlab = "renta")
# Sí hay diferencia en la variable renta por colonia, pues los estadísticos descriptivos
# son diferentes dependiendo de la colonia, esto también se observa en los histogramas
# ya que en la colonia 1 los datos están sesgados hacia la izquierda, mientras que
# en la colonia 2 están sesgados hacia la derecha

# g)

cor(tvtot,ninos) # sí hay relación pues la correlación entre estas dos variables
# es de .6248

detach(cable)


# ejercicio 4

# a) analíticamente

# b)

x <- c(2,3,4,5,6,7,8,9,10,11,12)
p <- c(1/36, 2/36, 3/36, 4/36, 5/36, 6/36, 5/36, 4/36, 3/36, 2/36, 1/36)


suma_dados <- function(n, m)
{
  suma <- 0
  for (k in 1:m)
{
  tot_3 <- 0
  tot_7 <- 0
  for (j in 1:n)
  {
    s <- sample(x, 1, replace = TRUE, p)
    suma_3 <- 0
    suma_7 <- 0
    i <- 1
    while (suma_3 + suma_7 == 0  && i <= 1000)
    {
      if (s == 3)
      {
        suma_3 <- 1
      }
      if (s == 7)
      {
        suma_7 <- 1
      }
      i <- i + 1
    } #while
    tot_3 <- tot_3 + suma_3
    tot_7 <- tot_7 + suma_7
    if ( tot_3+tot_7 > 0)
    {
      prop <- tot_3/(tot_3+tot_7)
    } 
    else
    {
      prop <- 0
    }
  } #for j's
  suma <- prop + suma
} #for k's
  suma/m
}

suma_3o7 <- suma_dados(30,10)
# da probabilidades cercanos a 1/4, la probabilidad teórica

suma_dados2 <- function(n, m)
{
  suma <- 0
  for (k in 1:m)
  {
    tot_4 <- 0
    tot_7 <- 0
    for (j in 1:n)
    {
      s <- sample(x, 1, replace = TRUE, p)
      suma_4 <- 0
      suma_7 <- 0
      i <- 1
      while (suma_4 + suma_7 == 0  && i <= 1000)
      {
        if (s == 4)
        {
          suma_4 <- 1
        }
        if (s == 7)
        {
          suma_7 <- 1
        }
        i <- i + 1
      } #while
      tot_4 <- tot_4 + suma_4
      tot_7 <- tot_7 + suma_7
      if ( tot_4 + tot_7 > 0)
      {
        prop <- tot_4/(tot_4+tot_7)
      } else
      {
        prop <- 0
      }
    } #for j's
    suma <- prop + suma
  } #for k's
  suma/m
}

suma_4o7 <- suma_dados2(30,10)
# da probabilidades cercanos a 1/3, la probabilidad teórica



# ejercicio 6

# a) 
teta <- 5
fx <- function(x){teta*(1/x)^(teta + 1)}
integrate(fx, 1, Inf)$value
# el valor de la integral es 1, por lo tanto es densidad

# b)

x <- seq(1,10,0.01)
y <- teta*(1/x)^(teta+1)
plot(x, y, xlim=c(1,10), ylim=c(0,teta), type="l", xlab="x", ylab="f(x)")
title("Función de densidad, f(x)")
y <- 1 - x^(-teta)
plot(x, y, xlim=c(1,10), ylim=c(0,1.3), type="l", xlab="x", ylab="F(x)")
title("Función distribución, F(x)")

# c)

#set.seed(12345)
teta <- 2
# generar U~unif(0,1) -> F^(-1)(u) -> X
U <- runif(10000,0,1)
X <- (1-U)^(-1/teta)
mean(X)
var(X)
teta <- 10
U <- runif(10000,0,1)
X <- (1-U)^(-1/teta)
mean(X)
var(X)


# d)

# son muy cercanos pues los valores teóricos para la media son, 2 y 10/9,
# (usando teta = 2 y teta = 10 respectivamente); en el caso de las varianzas
# cuando teta = 10 el valor de la varianza en las muestras aleatorias también
# muy cercano al teórico que es 10/(81*8), para el caso en que teta = 2 la varianza
# teórica no existe

# e)

teta <- 2
p_acum_2 <- 1-3^(-teta)
teta <- 10
p_acum_10 <- 1-3^(-teta)

# pregunta 7

binomial <- function(p, n, m)
{
  muestra <- sample(c(0,1), n, replace = TRUE, c(1-p, p))
  frec <- sum(muestra)
  for (i in 2:m)
  {
    muestra<- sample(c(0,1), n, replace = TRUE, c(1-p, p))
    frec <- c(sum(muestra), frec)
  }
  print(table(frec))
  bin <- rbinom(2000,10,.6)
  print(table(bin))  
  frec
}

bin_sim <- binomial(.6, 10, 2000)
hist(bin_sim, probability = TRUE)
bin <- rbinom(20, 10, .6)
lines(density(bin))

