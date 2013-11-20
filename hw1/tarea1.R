
#### 1. Operaciones Básicas ####

# a.
a <- seq(from=5, to=100, by=5)
a

# b.
Tratamiento <- c(rep("Tratamiento 1", 20), 
                 rep("Tratamiento 2", 18), 
                 rep("Tratamiento 3", 22))
Tratamiento

# c.
x <- 5
y <- 7
z <- x^y
z

# d.
u <- c(1,2,5,4)
v <- c(2,2,1,1)
u; v
# d.i
z.1 <- which(u == 5)
z.1
# d.ii
z.2 <- which(v >= 2)
z.2
# d.iii
pvec <- c(u[2]*v[3]-u[3]*v[2], u[3]*v[1]-u[1]*v[3], u[1]*v[2]-u[2]*v[1])
pvec
# d.iv
sum(u * v)
# d.v
X <- matrix(c(u, v), nrow=2, byrow=TRUE) 
X
k <- 2
Y <- t(X) * k 
Y
W <- X %*% Y
W
# d.vi
det = (W[1,1] * W[2,2]) - (W[1,2] * W[2,1])
det
t <- matrix(c(W[2,2], -W[1,2], -W[2,1], W[1,1]), nrow=2, byrow=TRUE)
t
invW <- t * (1 / det)
invW
solve(W)

# e.
A <- matrix(c(1,2,3,4,5,2,1,2,3,4,3,2,1,2,3,4,3,2,1,2,5,4,3,2,1), nrow=5, ncol=5)
y <- c(7,-1,-3,5,17)
invA <- solve(A)
x <- invA %*% y
x
y.t <- A %*% x
y.t


#### 2. funciones ####

# a.
temps <- seq(from=10, to=100, by=5)
for (celcius in temps) {
    fahrenheit <- (9 / 5 * celcius + 32)
    kelvin <- (celcius - 273)
    cat(sprintf("%iC = %iF = %iK\n", celcius, fahrenheit, kelvin))
}

# b.
magic <- function( a ) {
    if(a == 1) {
        s <- c(1)
    } else if(a == 2) {
        s <- c(1, 2)
    } else {
        s <- c(1, 2)
        for (i in 3:a) {
            s <- append(s, s[i-1] + ( 2 / s[i-1] ))
        }
    }
    s
}

magic(1)
magic(2)
magic(15)


# c.
random.walk <- function(n) {
    s <- sample(c(-1, 1), n, replace=TRUE)
    steps <- (0)
    sum <- 0
    for (i in 2:length(s)) {
        sum <- sum + s[i]
        steps <- append(steps, sum)
    }
    steps
}

set.seed(10203040)
random.walk(10)
