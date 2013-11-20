
### ===============================================
set.seed(145849)
### ===============================================

# Ejercicio 1

# a)
muestraX <- rnorm(100,100,10)
mediasX <- mean(muestraX)
for (i in 1:999){
  muestraX <- rnorm(100,100,10)
  mediasX <- c(mediasX,mean(muestraX))
}

muestraY <- rnorm(81,50,9)
mediasY <- mean(muestraY)
for (i in 1:999){
  muestraY <- rnorm(81,50,9)
  mediasY <- c(mediasY,mean(muestraY))
}

# b)
resta <- mediasX - mediasY
hist(resta,probability=TRUE)
lines(density(rnorm(1000,50,sqrt(2))))

# c)
mu <- mean(resta)
sigma <- sd(resta)

# d)
pnorm(52,mu,sigma) # empirica
pnorm(52,50,sqrt(2)) # teorica

### ===============================================
### ===============================================

# Ejercicio 2
# a) y b)

lambda <- 1
x<- c(seq(0.01, 2, 0.05))

par(mfrow=c(2,2))
for(n in c(30, 100, 300, 500)){
    plot(x,dgamma(x,n,n*lambda),type="l",lty=1,
         main=paste("Distribución Gamma para n =", n))    
}

for(n in c(30, 100, 300, 500)){
    mediaExp <- array(0,n)
    for (i in 1:n) {mediaExp[i]<-mean(rexp(n,1))}
    hist(mediaExp, probability=TRUE, 
         main=paste("Medias de dist. exponencial con n =", n))
    lines(density(rgamma(x,n,lambda*n)), 
          col="blue", lwd=2)
}
par(mfrow=c(1,1))


## d)
# a partir de una muestra de 300 se puede decir que coinciden
# la distribución de muestreo y la función de densidad

### ===============================================
### ===============================================

## Ejercicio 3
binom.a <- rbinom(size=1000, n=20000, p=0.3)
binom.b <- rbinom(size=1000, n=20000, p=0.7)

hist(binom.b - binom.a, probability=TRUE)

# Aproximacion a una distribucion normal:
u <- (1000 * 0.7) - (1000 * 0.3)
s <- sqrt((1000 * 0.7) * (1 - 0.7)) + sqrt((1000 * 0.3) * (1 - 0.3))

norm <- rnorm(n=20000, mean=u, sd=s)
lines(density(norm), col="red", lwd=2)

### ===============================================
### ===============================================

## Ejercicio 4

### ===============================================
### ===============================================

## Ejercicio 5

# a
x.0 <- function(p){ p ^ 3 }
x.1 <- function(p){ (1 - p) * (p ^ 2) }
x.2 <- function(p){ (1 - p) ^ 2 }
x.3 <- function(p){ (1 - p) * p * 2 }

f.x <- function(x){
    if(! x %in% 0:3){
        stop("x invalida")
    }
    return(c(x.0, x.1, x.2, x.3)[[x+1]])
}

values <- apply(as.matrix(0:3), 1, function(x){
            integrate(f.x(x), 0, 1)$value
})

sum(values)

## b
f.p <- function(p) {
    ( 72 * log(p) ) + ( 54 * log(1 - p) ) + ( 108 * log(p) ) +
    ( 64 * log(1 - p) ) + ( 40 * log(p) ) + ( 40 * log(1 - p) ) + 
    ( 40 * log(2) ) + 
    log( factorial(150) / (factorial(24) * factorial(54) * factorial(32) * factorial(40)) )    
}

## c
max <- optim(runif(1), f.p, method="Brent", 
             control=list(fnscale=-1), lower=0, upper=1)

max$par
 
curve(f.p, from=0, to=1, col="blue", main="Log-Verosimilitud")
points(x=max$par, y=max$value, lwd=2, bg="grey")



