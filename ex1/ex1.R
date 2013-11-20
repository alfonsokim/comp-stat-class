#### ======================================== ####
###        Examen Parcial de Estadistica
###             Alfonso Kim - 145849
#### ---------------------------------------- ####

### Parte 1: Probabilidad
## a) Dados

sumas <- c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
p.sumas <- c(0, 1/36, 2/36, 3/36, 4/36, 5/36,    # el 0 es para poder acceder por indice
             6/36, 5/36, 4/36, 3/36, 2/36, 1/36)

sum(p.sumas)

caras.ganar <- c(7, 11)
caras.perder <- c(2, 3, 12)
caras.comodin <- c(4, 5, 6, 8, 9, 10)

p.ganar.primer.tiro <- sum( p.sumas[ sumas %in% caras.ganar ] )
p.comodin <- sum( p.sumas[ sumas %in% caras.comodin ] )
p.7 <- p.sumas[ 7 ]

p.ganar.comodin <- 0 

for( cara in caras.comodin ){
    x <-  p.sumas[ cara ] / ( p.sumas[ cara ] + p.7 )
    p.ganar.comodin <- p.ganar.comodin + (p.sumas[ cara ] * x)
}

p.ganar <- p.ganar.primer.tiro + p.ganar.comodin

#### ======================================== ####
## b) Laplace
## i)
lambda <- 1/2
m <- 0

f.laplace <- function(x){
    lambda / 2 * exp( -lambda * abs(x - m))
} 

e.laplace <- function(x){
    x * f.laplace(x)
}

E.Laplace <- integrate(e.laplace, -Inf, Inf)

e.laplace.2 <- function(x){
    x^2 * f.laplace(x) }

E.Laplace.2 <- integrate(e.laplace.2, -Inf, Inf)

varianza <- E.Laplace.2$value - E.Laplace$value^2  #2/lambda^2

E.Laplace$value
varianza

#### ---------------------------------------- ####
## ii) Variables Aleatorias X1 X2
set.seed(10102013)
x1 <- rexp(25000, rate=1/2)
x2 <- rexp(25000, rate=1/2)
y <- x1 - x2


# Histograma de densidad
laplace <- function(x){
    l <- 1/2
    (1/2) * (exp(-(abs(x-l))))
}
hist(y, prob=TRUE)
curve(laplace, add=TRUE, lwd=2, col="red")


#### ======================================== ####

### 2: Analisis Exploratorio de Datos

## a: Funcion para tabla de contingencia
# La siguiente funcion toma de argumentos la cadena de caracteres a procesar
# y un argumento adicional k con el tamanio de la tupla. k=3 por defecto
crea.tabla.tuplas <- function(string, k=3) {
    s.vector <- strsplit(string, '')[[1]] 
    tuplas <- c()
    for (i in 1:( nchar(string) - k+1 ) ) {
        tupla <- paste(s.vector[i : (i+k-1)], collapse="")
        tuplas <- c(tuplas, tupla)
    }
    table(tuplas)
}

print(crea.tabla.tuplas("CAGACAAAAC"))

## b: Cadena de pruebas
# La siguiente funcion toma de argumento el numero de caracteres que se
# desean generar en la cadena de pruebas, ademas de un argumento adicional
# con el universo de caracteres disponibles para generar la cadena
crea.cadena.pruebas <- function(num.caracteres, universo="ACGT"){
    elementos <- strsplit(universo, "")[[1]]
    paste(sample(elementos, num.caracteres, replace=TRUE), collapse="")
}

cadena.prueba <- crea.cadena.pruebas(10000)
print(crea.tabla.tuplas(cadena.prueba))
# k=5 para ver que pasa
print(crea.tabla.tuplas(cadena.prueba, k=5))

# Una prueba con los caracteres del alfabeto
prueba.2 <- crea.cadena.pruebas(100, universo=paste(LETTERS, collapse=""))
print(crea.tabla.tuplas(prueba.2, k=2))


#### ---------------------------------------- ####
## c: Coeficiente de variacion
cv <- function(observaciones){
    sd(observaciones) / mean(observaciones)
}

num.observaciones <- 20
media.observaciones <- 2

coeficientes <- c()
for (i in 1:500){
    muestra <- rexp(num.observaciones, 1/media.observaciones)
    coeficientes <- c(coeficientes, cv(muestra))
}

hist(coeficientes)


#### ======================================== ####
### Funciones

# Primero una funcion que solo evalua un valor de X
f <- function(x){
    ret <- 0
    if(x < 0){
        ret <- x^2 + 2*x + 3
    } else if (x >= 0 && x < 2){
        ret <- x + 3
    } else {
        ret <- x^2 + 4*x - 7
    }
    ret
}

# Luego la evaluacion de todos los valores de X
fvec <- function(xvec){
    yvec <- c()
    for(x in xvec){
        yvec <- c(yvec, f(x))
    }
    yvec
}

# Finalmente se grafica la funcion
curve(fvec, from=-3, to=3)



#http://mathworld.wolfram.com/Craps.html
#http://en.wikibooks.org/wiki/LaTeX/Mathematics
#http://www.physicsforums.com/showthread.php?t=350905

install.packages("xkcd")
library(ggplot2)
library(xkcd)
volunteers <- data.frame(year=c(2007:2011),
                         number=c(56470, 56998,59686, 61783, 64251))
p <- ggplot() + xkcdrect(aes(xmin = year,
                             xmax= year +0.3,
                             ymin=number,
                             ymax = number + 3600),
                         volunteers,
                         fill="red", colour="black")
p