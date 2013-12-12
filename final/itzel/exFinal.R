

# Estadistica Computacional
# Examen final
# Itzel Muñoz


set.seed(122803)

# Ejercicio 1

# a)

v <- c(5,-5)
p <- c(18/38,20/38)

# esta funci?n calcula la suma de las ganancias de los 20 juegos de la ruleta
juegos20 <- function(){
  juegos <- sample(v, 20, replace = TRUE, p)
  suma <- sum(juegos)
}
ganancias <- juegos20() # ganancia total de los 20 juegos
ganancias 


# para encontrar la probabilidad aproximada de que la ganancia total de los
# 20 juegos sea positiva:
juegos100 <- replicate(100,sample(v, 20, replace = TRUE, p),simplify=TRUE)

pos <- 0
for(i in 1:100){
  if( sum(juegos100[,i]) > 0){
    pos <- pos + 1
  }
}

prob.gan.pos <- pos/100 #probabilidad aproximada de que la ganancia total de los 20 juegos
# sea positiva
prob.gan.pos


# b)


# X se distribuye binomial con par?metros n = 20 (total de juegos) 
# y p = 18/38 (probabilidad de ganar $5)

# la probabibilidad de que la ganancia total sea positiva es igual a la probabilidad
# de que gane $5 en 11 ? m?s tiros de la ruleta (de los 20 en total), que es igual
# a 1 menos la probabilidad de que gane $5 en 10 ? menos tiros de la ruleta (de los 20)

prob.gan.pos2 <- 1 - pbinom(10,20,18/38)
prob.gan.pos2
# la probabilidad exacta es 0.3223 y la simulada aproximadamente 0.32, la cual es 
# bastante cercano a la exacta

# c)

ganancias <-  sample(v, 20, replace = TRUE, p)
gan.acum <- cumsum(ganancias)

P <- 0
for(i in 1:20){
  if( gan.acum[i] > 0){
    P <- P + 1
  }
}
P

Pvector <- array(0,500)
for (i in 1:500){
  ganancias <-  sample(v, 20, replace = TRUE, p)
  gan.acum <- cumsum(ganancias)
  P <- 0
  for(j in 1:20){
    if( gan.acum[j] > 0){
      P <- P + 1
    }
  }
  Pvector[i] <- P 
}


hist.P <- hist(Pvector,breaks=seq(0,20,1),plot=F)

# Construir la tabla de frecuencias
n <- length(hist.P$breaks)
tab.P <- cbind(hist.P$breaks[-n],hist.P$breaks[-1],hist.P$counts,hist.P$counts/sum(hist.P$counts),
                     cumsum(hist.P$counts),cumsum(hist.P$counts/sum(hist.P$counts)))
dimnames(tab.P)[[2]]<-c("Linf","Lsup","f","fr","F","Fr")
print(tab.P)
# otra tabla de frecuencias
table(Pvector)

# Graficar
hist(Pvector,breaks=seq(0,20,1),col="blue",xlab="longitud", main="Histograma de P", prob=TRUE)

# por lo tanto los valores m?s probables que toma P son 0 y 1


# Ejercicio 2

# a)

# como u~U(0,5) => A debe de ser menor o igual a 1, de hecho A debe de ser un n?mero peque?o
# para as? poder generar tambi?n valores peque?os de u, como no hay grandes diferencias
# entre tomar n?meros menores a 1/2 y tomar 1/2, se elige como ?ptimo A = 1/2 (valores
# menores realizan muchas iteraciones y toman mucho tiempo en generar la muestra)
A <- 1/2

# b)

n<-2000
k<-0   
j<-0  
y<-numeric(n)


while(k<n){
  u<-runif(1,min=0,max=5)
  j<-j+1
  v<-runif(1,min=0,max=A)
  if(v^2*(5-v)*sin(v)^2>u){
    k<-k+1
    y[k]<-v   
  }    
}
j #total de iteraciones necesarias para generar la muestra de tama?o n

# c)

plot(density(y),main="Densidad de X",xlab="x")


# Ejercicio 3

install.packages("bootstrap")
library(bootstrap)

data(scor)
attach(scor)

# a) 
pairs(scor[1:88,1:5])
cor(scor)

# se observa que todas las variables est?n muy correlacionadas pues todas (a excepci?n de
# mec y sta) tienen una correlaci?n cercana a .5 o mayor a .5; hay que mencionar tambi?n
# que todas tienen correlaci?n positiva. En la matriz de dispersi?n se observa que
# hay variables en las cuales resalta su dependencia lineal, por ejemplo vec y alg, 
# mientras que hay otras en las que no, por ejemplo mec y sta.

# b)

#install.packages("boot")
library(boot)

# obtener los intervalos de confianza para la correlaci?n

out<-boot(scor[,1:2],R=3000,statistic=corr)
c.i.mec.vec<-boot.ci(out,type=c("basic","norm","perc"))
c.i.mec.vec

out<-boot(scor[,3:4],R=3000,statistic=corr)
c.i.alg.ana<-boot.ci(out,type=c("basic","norm","perc"))
c.i.alg.ana

out<-boot(scor[,3:5],R=3000,statistic=corr)
c.i.alg.sta<-boot.ci(out,type=c("basic","norm","perc"))
c.i.alg.sta

out<-boot(scor[,4:5],R=3000,statistic=corr)
c.i.ana.sta<-boot.ci(out,type=c("basic","norm","perc"))
c.i.ana.sta

out<-boot(scor[,1:4],R=3000,statistic=corr)
c.i.mec.ana<-boot.ci(out,type=c("basic","norm","perc"))
c.i.mec.ana


# obtener los errores est?ndar
B<-500         
R<-numeric(B)     
n<-nrow(scor)   

for(b in 1:B){
  i<-sample(1:n,size=n,replace=TRUE)  # con reemplazo
  b.mec<-mec[i]
  b.vec<-vec[i]
  R[b]<-cor(b.mec,b.vec)  
}


cor.boot<-R       
error.boot.mec.vec<-sd(R)   
error.boot.mec.vec

for(b in 1:B){
  i<-sample(1:n,size=n,replace=TRUE)  # con reemplazo
  b.alg<-alg[i]
  b.ana<-ana[i]
  R[b]<-cor(b.alg,b.ana)  
}


cor.boot<-R       
error.boot.alg.ana<-sd(R)   
error.boot.alg.ana   

for(b in 1:B){
  i<-sample(1:n,size=n,replace=TRUE)  # con reemplazo
  b.alg<-alg[i]
  b.sta<-sta[i]
  R[b]<-cor(b.alg,b.sta)  
}


cor.boot<-R       
error.boot.alg.sta<-sd(R)   
error.boot.alg.sta

for(b in 1:B){
  i<-sample(1:n,size=n,replace=TRUE)  # con reemplazo
  b.ana<-ana[i]
  b.sta<-sta[i]
  R[b]<-cor(b.ana,b.sta)  
}


cor.boot<-R       
error.boot.ana.sta<-sd(R)   
error.boot.ana.sta


for(b in 1:B){
  i<-sample(1:n,size=n,replace=TRUE)  # con reemplazo
  b.mec<-ana[i]
  b.ana<-ana[i]
  R[b]<-cor(b.mec,b.ana)  
}


cor.boot<-R       
error.boot.mec.ana<-sd(R)   
error.boot.mec.ana



# Ejercicio 4

# a)
# desarrollo en pfd
proba.exacta <- 1 - pweibull(4,2,3)
proba.exacta

# b)

muestra.w <- rweibull(2000,2,3)

cont <- 0
for (i in 1:2000){
  if (muestra.w[i] > 4){
    cont <- cont + 1
  }
}
proba <- cont/2000
proba
# la probabilidad simulada es aproximadamente 0.16 que es cercana a la exacta (0.1690)

# c)

g<-function(x){
  (2/3)*(x/3)*exp(-(x/3)^2)*(x>4)
}

x <- rnorm(10000,mean=4)


fg <- g(x)/dnorm(x,mean=4)

theta.hat <- mean(fg)
theta.hat
# la probabilidad obtenida es aproximadamente 0.169 que es muy cercana a la exacta (0.1690)
# y en general es mejor aproximaci?n que la obtenida en el inciso b)


# Ejercicio 5

# a)

# se utiliza un muestreador Metr?polis pues ambas distribuciones prior son sim?tricas

n <- 10
x0 <- 5
theta <- 1/2
N <- 2000
metropolis <- function(theta,x0,N){
  x<-numeric(N)
  x[1]<-x0
  u<-runif(N)
  k<-0
  for(i in 2:N){
    y <- runif(1) # U(0,1) para as? poder transformarlo en un entero entre 0 y 1
    y <- round(y*10)
    if(u[i]<=(dbinom(y,n,theta))/dbinom(x[i-1],n,theta))
      x[i]<-y else{
        x[i]<-x[i-1]
        k<-k+1
      }
  }
  return(list(x=x,k=k))
}

bin.unif <- metropolis(theta,x0,N)
x.plot <- N*ppoints(N)
plot(x.plot,bin.unif$x,type="l",ylab="X",xlab="",main="Theta = 1/2")
eff <- bin.unif$k/N
eff

burn.in <- 50
plot(x.plot,bin.unif$x[burn.in+1:N],type="l",ylab="X",xlab="",main="Theta = 1/2")



theta.vec <- seq(0.001,0.999,.001)
m <- length(theta.vec)
n2 <- 1
jeff <- numeric(m)
for(i in 1:m){
  jeff[i] <- n2/sqrt(theta.vec[i]*(1-theta.vec[i]))
}

metropolis2 <- function(theta,x0,N){
  x<-numeric(N)
  x[1]<-x0
  u<-runif(N)
  k<-0
  for(i in 2:N){
    j <- sample(10:990,size =1, replace =TRUE) #obtener valores "aleatorios" de la funci?n Jeffrey
    y <- round(jeff[j]) #transformalo en un entero entre 0 y 1
    if(u[i]<=(dbinom(y,n,theta))/dbinom(x[i-1],n,theta))
      x[i]<-y else{
        x[i]<-x[i-1]
        k<-k+1
      }
  }
  return(list(x=x,k=k))
}

bin.jeff <- metropolis2(theta,x0,N)
x.plot <- N*ppoints(N)
plot(x.plot,bin.jeff$x,type="l",ylab="X",xlab="",main="Theta = 1/2")
eff <- bin.jeff$k/N
eff


burn.in2 <- 50
plot(x.plot,bin.jeff$x[burn.in2+1:N],type="l",ylab="X",xlab="",main="Theta = 1/2")


# b)

x.plot <- seq(0,100)
plot(x.plot,dunif(x.plot),type="l",xlab="theta",xlim= c(0,1),ylim=c(0,1.5),main="Distribuci?n prior uniforme")


theta.vec <- seq(0.001,0.999,.001)
m <- length(theta.vec)
n2 <- 1
jeff <- numeric(m)
for(i in 1:m){
  jeff[i] <- n2/sqrt(theta.vec[i]*(1-theta.vec[i]))
}

plot(theta.vec,jeff,main="Distribuci?n prior Jeffrey",type="l")


# c)


# tomado de batchmeans

bm <- function(vals,bs="sqroot",warn=FALSE)
{
  N <- length(vals)
  if (N<1000)
  {
    if (warn) # if warning
      cat("WARNING: too few samples (less than 1000)\n")
    if (N<10)
      return(NA)
  }
  
  if (bs=="sqroot") 
  {
    b <- floor(sqrt(N)) # batch size
    a <- floor(N/b) # number of batches
  }
  else
    if (bs=="cuberoot") 
    {
      b <- floor(N^(1/3)) # batch size
      a <- floor(N/b) # number of batches
    }
  else # batch size provided
  {
    stopifnot(is.numeric(bs))  
    b <- floor(bs) # batch size
    if (b > 1) # batch size valid
      a <- floor(N/b) # number of batches
    else
      stop("batch size invalid (bs=",bs,")")
  }
  
  Ys <- sapply(1:a,function(k) return(mean(vals[((k-1)*b+1):(k*b)])))
  
  muhat <- mean(Ys)
  sigmahatsq <- b*sum((Ys-muhat)^2)/(a-1)
  
  bmse <- sqrt(sigmahatsq/N)
  
  return(list(est=muhat,se=bmse))
}

##############

# en ambos casos se utiliz? una cadena de tama?o 1950, con un valor de theta de 1/2

# error est?ndar asociado a montecarlo
bm(bin.unif$x[burn.in:N])

bm(bin.jeff$x[burn.in2:N])

# gr?ficas de autocorrelaci?n para cada prior
par(mfrow=c(1,2))
acf(bin.unif$x,plot=TRUE)
acf(bin.jeff$x,plot=TRUE)


# d)

theta <- 28/50

bin.unif <- metropolis(theta,x0,N)
x.plot <- N*ppoints(N)
#plot(x.plot,bin.unif$x,type="l",ylab="X",xlab="",main="Theta = 28/50")
eff <- bin.unif$k/N
eff

burn.in <- 50
plot(x.plot,bin.unif$x[burn.in+1:N],type="l",ylab="X",xlab="",main="Theta = 28/50")


bin.jeff <- metropolis2(theta,x0,N)
x.plot <- N*ppoints(N)
#plot(x.plot,bin.jeff$x,type="l",ylab="X",xlab="",main="Theta = 28/50")
eff <- bin.jeff$k/N
eff


burn.in2 <- 50
plot(x.plot,bin.jeff$x[burn.in2+1:N],type="l",ylab="X",xlab="",main="Theta = 28/50")

bm(bin.unif$x[burn.in:N])
# su valor promedio es aproximadamente 5.5 =n*theta, por lo tanto el valor promedio
# de theta es aproximadamente 5.5/n = 5.5/10 =.55

bm(bin.jeff$x[burn.in2:N])
# su valor promedio es aproximadamente 3.8 =n*theta, por lo tanto el valor promedio
# de theta es aproximadamente 3.8/n = 3.8/10 =.38

# e)


theta <- 280/500 # igual a 28/50

bin.unif <- metropolis(theta,x0,N)
x.plot <- N*ppoints(N)
#plot(x.plot,bin.unif$x,type="l",ylab="X",xlab="",main="Theta = 28/50")
eff <- bin.unif$k/N
eff

burn.in <- 50
plot(x.plot,bin.unif$x[burn.in+1:N],type="l",ylab="X",xlab="",main="Theta = 280/500")


bin.jeff <- metropolis2(theta,x0,N)
x.plot <- N*ppoints(N)
#plot(x.plot,bin.jeff$x,type="l",ylab="X",xlab="",main="Theta = 28/50")
eff <- bin.jeff$k/N
eff


burn.in2 <- 50
plot(x.plot,bin.jeff$x[burn.in2+1:N],type="l",ylab="X",xlab="",main="Theta = 280/500")



bm(bin.unif$x[burn.in:N])
# su valor promedio es aproximadamente 5.5 =n*theta, por lo tanto el valor promedio
# de theta es aproximadamente 5.5/n = 5.5/10 =.55

bm(bin.jeff$x[burn.in2:N])
# su valor promedio es aproximadamente 3.8 =n*theta, por lo tanto el valor promedio
# de theta es aproximadamente 3.8/n = 3.8/10 =.38

# f)

theta <- .0001 #si se asignara el valor de 0 a theta todos los resultados ser?an 0


bin.unif <- metropolis(theta,x0,N)
x.plot <- N*ppoints(N)
#plot(x.plot,bin.unif$x,type="l",ylab="X",xlab="",main="Theta = .0001")
eff <- bin.unif$k/N
eff

burn.in <- 50
plot(x.plot,bin.unif$x[burn.in+1:N],type="l",ylab="X",xlab="",main="Theta = .0001")


bin.jeff <- metropolis2(theta,x0,N)
x.plot <- N*ppoints(N)
#plot(x.plot,bin.jeff$x,type="l",ylab="X",xlab="",main="Theta = .0001")
eff <- bin.jeff$k/N
eff


burn.in2 <- 50
plot(x.plot,bin.jeff$x[burn.in2+1:N],type="l",ylab="X",xlab="",main="Theta = .0001")

bm(bin.unif$x[burn.in:N])
# su valor promedio es aproximadamente 0 =n*theta, por lo tanto el valor promedio
# de theta es aproximadamente 0/n = 0/10 = 0

bm(bin.jeff$x[burn.in2:N])
# su valor promedio es aproximadamente 2 =n*theta, por lo tanto el valor promedio
# de theta es aproximadamente 2/n = 2/10 = .2


