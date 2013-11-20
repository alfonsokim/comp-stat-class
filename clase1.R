matrix.ex1 <- matrix(c(10:15, 5:10), nrow=4, byrow=F)
matrix.ex1

z <- rnorm(500, mean=10, sd=50)
quantile(z, 0.9)

s <- summary(z)
str(s)
s[1:5]

?range

sin( c(0,60,90,120) )*pi/180

install.packages("boot")

?shapiro.test

shapiro.test(z)

4+1

#1

alpha <- 0.01; n <- 50; m <- 1000
datos <- matrix()

ls()

#Borra todo lo que hay
rm(list=ls())

gc()

x <- factor(c(1,2,2,1,1,2,1,2,1))
summary(x)

#Clases sociales
probs <- c(0.45,0.05,0.01, 0.48,0.70,0.50, 0.07,0.25,0.49)
P <- matrix(probs, nrow=3, ncol=3)
rownames(P) <- colnames(P) <- c("inferior", "medio", "superior")
rowSums(P)
colSums(P)
summary(P)
plot(P)
n <- rnorm(1000, mean=50, sd=100)
plot(n)