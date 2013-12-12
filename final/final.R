
## 1
## a

set.seed(145849)

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

## b
1 - pbinom((20 - 10), 20, p.ganar)

## c
ganancias <- apply(matrix(replicate(500, juego()), nrow=20, ncol=500), 2, cumsum)
P <- apply(ganancias > 0, 2, sum)
hist(P, breaks= 0:20, right=F, col="blue")
## Los valores mas frecuentes son cero y uno

## 2
## a
f.x <- function(x){
    ifelse(x<0 | x>5, 0, x^2 * (5-x) * (sin(2*x)) ^ 2)
}

# b)

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

plot(density(aceptar.rechazar(n=2000, A=1)$y),
     main="Densidad de X", xlab="x", col="blue", lwd=2)

### 3
library(bootstrap)

data(scor)

## a)
pairs(scor)
cor(scor)

## b)
names(scor)
variables <- list(c("mec", "vec"), c("alg", "ana"), c("alg", "sta"), 
             c("ana", "sta"), c("mec", "ana"))

for(vars.par in variables){
    correlaciones <- boot(scor[, vars.par], R=1000, statistic=corr)
    corr.val <- boot.ci(correlaciones, type="norm")
    rango.confianza <- corr.val$normal[2:3]
    s <- sprintf("Para p(%s, %s) rango = (%f, %f)", 
                 vars.par[[1]], vars.par[[2]], 
                 rango.confianza[[1]], rango.confianza[[2]])
    print(s)
}



### 4
## (k/l) * ((x/l) ^ (k-1)) * exp(-((x/l)^k))

h <- function(s, x){
    ifelse(s < x, 1, 0)
}

muestra <- rweibull(2000, shape=3, scale=2)

sum(h(4, muestra)) / 2000

"ho" %in% "hola"


### 4



