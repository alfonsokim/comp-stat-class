
## =============================================
##          Estadistica Computacional
##                 Examen Final
## ---------------------------------------------
##              Alfonso Kim Quezada
## =============================================

## =============================================
## Pregunta 1
## ---------------------------------------------
## a)

set.seed(145849)
p.ganar=18/38

juego <- function(n=20, p.ganar=18/38){
    sample(c(5, -5), size=n, replace=T, prob=c(p.ganar, 1-p.ganar))    
}

juego()

ganancias.juego <- function(salidas=juego()){
    sum(salidas)
}

juegos.100 <- replicate(100, ganancias.juego())
p.ganar.100 <- length(which(juegos.100 > 0)) / 100
p.ganar.100

## ---------------------------------------------
## b)
1 - pbinom((20 - 10), 20, p.ganar)

## ---------------------------------------------
## c)
ganancias <- apply(matrix(replicate(500, juego()), nrow=20, ncol=500), 2, cumsum)
P <- apply(ganancias > 0, 2, sum)
hist(P, breaks= 0:20, right=F, col="blue")
## Los valores mas frecuentes son cero y uno

## =============================================
## Pregunta 2
## ---------------------------------------------

## ---------------------------------------------
## b)
f.x <- function(x){
    ifelse(x<0 | x>5, 0, x^2 * (5-x) * (sin(2*x)) ^ 2)
}

## ---------------------------------------------
## c)
aceptar.rechazar <- function(n, A){
    iters <- 0
    y <- c()
    while(length(y) < n) {
        iters <- iters + 1
        u <- runif(1, min=0, max=5)
        v <- runif(1, min=0, max=A)
        if(u < f.x(v)){
            y <- c(y, v)
        }
    }
    list(y=y, iters=iters)
}

aceptar.rechazar(n=2000, A=1)$iters
aceptar.rechazar(n=2000, A=0.1)$iters
## Con valores pequeños de A se ejecutan demasiadas iteraciones
aceptar.rechazar(n=2000, A=5)$iters

plot(density(aceptar.rechazar(n=2000, A=5)$y),
     main="Densidad de X", xlab="x", col="blue", lwd=2)

## =============================================
## Pregunta 3
## ---------------------------------------------
## a)
library(boot)
library(bootstrap)
data(scor)

pairs(scor)
cor(scor)

## ---------------------------------------------
## b)
names(scor)
variables <- list(c("mec", "vec"), c("alg", "ana"), c("alg", "sta"), 
             c("ana", "sta"), c("mec", "ana"))

for(vars.par in variables){
    correlaciones <- boot(scor[, vars.par], R=1000, 
                          statistic=function(x, i){cor(x[i,1], x[i,2])})
    corr.val <- boot.ci(correlaciones, type="norm")
    rango.confianza <- corr.val$normal[2:3]
    error.estandar <- sd(correlaciones$t) ## sd del vector t da el error estandar
    s <- sprintf("Para p(%s, %s) rango = (%f, %f) y error estandar %f", 
                 vars.par[[1]], vars.par[[2]], 
                 rango.confianza[[1]], rango.confianza[[2]],
                 error.estandar)
    print(s)
}


## =============================================
## Pregunta 4
## ---------------------------------------------
## a)
teorica = exp(-8)

## ---------------------------------------------
## b)
prob <- 1 - pweibull(4, shape=3, scale=2)
muestra <- rweibull(2000, shape=3, scale=2)
length(which(muestra > 4)) / 2000

## ---------------------------------------------
## c)
f.x <- function(x){
    ifelse(x > 4, (2/3) * (x/3) * exp(-(x/3) ^ 2), 0)
}

x <- rnorm(10000, mean=2, sd=2)
f.g <- f.x(x) / dnorm(x, mean=2, sd=2)

theta.hat <- mean(f.g)
theta.hat

## =============================================
## Pregunta 5
## ---------------------------------------------
## a)
flat <- function(x, a=0, b=1){ 1/(b-a) }
jeffreys <- function(x, n=50){ n/(x*(1-x)) }

metropolis <- function(N, yfunc, rfunc){
    x <- rep(0, N)
    k <- 0
    x[1] <- 0.5
    for(i in 2:N){
        y <- yfunc()
        r <- rfunc(x=x, y=y, n=N)
        u <- runif(1)
        if(u < r){
            x[i] <- y
        } else {
            x[i] <- x[i-1]
            k <- k+1
        }
    }
    list(x=x, k=k, efficiencia=k/N)
}

binomial <- function(m = 10,n = 20 ,p ){
    choose(n,m)*((p)^m)*(1-p)^(n-m)
}

binomial(p=0.5)
binomial(p=0.2)
binomial(p=0.8)
?pbinom
dbinom(10, size=20, prob=0.5)
dbinom(10, size=20, prob=10)

y.flat <- function() { runif(1) }
r.flat <- function(x, y, n) {
    ( dbinom(10, size=20, prob=y) * flat(x) ) / 
        ( dbinom(10, size=20, prob=x[i-1] ) * flat(x))
}

?sample
y.jeff <- function() { 
    rango <- seq(0.001, 0.999, 0.001)
    sample(rango, size=1, replace=T, prob=rango)
}

r.jeff <- function(x, y, n) { 
    ( dbinom(10, size=20, prob=y) * jeffreys(x) ) / 
        ( dbinom(10, size=20, prob=x[i-1] ) * jeffreys(x))
}


met.flat <- metropolis(10000, y.flat, r.flat)
met.jeff <- metropolis(10000, y.jeff, r.jeff)

## ---------------------------------------------
## b)
curve(1+x*0, from=0, to=1, ylab="flat")
curve(jeffreys, from=0, to=1, ylab="jeffrey")

library(ggplot2)
qplot(1:length(met.flat$x), met.flat$x, 
      geom="line")

qplot(1:length(met.jeff$x), met.jeff$x, 
      geom="line")

## ---------------------------------------------
## c)
source("http://www.stat.psu.edu/~mharan/batchmeans.R")
bm(met.flat$x[2501:length(met.flat$x)])
bm(met.jeff$x[2501:length(met.jeff$x)])

