x <- matrix(rpois(20, 1.5), nrow=4)
rownames(x) <- rownames(x, do.NULL=FALSE, prefix="Muestra.X")
colnames(x) <- c("aspirina", "paracetamol", "dalai", "alka", "xray")

# 2=columna, 1=renglon
x <- rbind(x, apply(x, 2, mean)); rownames(x) <- c(1:4, "media")
x <- cbind(x, apply(x, 1, var))
colnames(x) <- c(1:5, "varianza", "varianza2", "varianza2")
x

library(ElemStatLearn)

data()
data(USArrests)
summary(USArrests)

USArrests["California", "Murder"]

USArrests[, c("Assault", "Murder")]

sort(USArrests$Murder)
hist(USArrests$Assault)

library(MASS)

truehist(USArrests$Assault)
hist(USArrests$Assault, prob=TRUE, breaks="scott")
?hist
pairs(USArrests)
?pairs

cor(USArrests)
cor(USArrests$UrbanPop, USArrests$Murder)

w <- with(USArrests, expr={murder.pct=100*Murder/(Murder+Assault+Rape)})

data(presidents)
help(presidents)

range(presidents, na.rm=TRUE)
?which
which.min(presidents)
which.max(presidents)

peso <- c(19,27,39, 10,58,17, 65,41, 63)
peso[peso < 20 & peso != 10]

trat <- c(rep("A", 3), rep("B", 3), rep("C", 3))
trat

peso[trat=="A" | trat=="B"]
joe <- split(peso, trat)
joe$A

x <- 1:5; y <- c(1, 3, 7:10)

union(x, y)
intersect(x, y)
setdiff(x, y)
setdiff(y, x)

f <- function(x, y){
    x*x + y*y
}

f(2,3)

z <- outer(x, y, f)
?outer
z

?runif

par.impar <- function(x){
    ifelse(round(x) %% 2 == 1, "Impar", "Par")
}

par.impar(10)
par.impar(3)
par.impar(-12)
par.impar(-5)
par.impar(3.4)
par.impar(3.6)

?gc

data(airquality)
attach(airquality)
names(airquality)

plot(Ozone, type="h", xaxt="t", mail="Aire en NY", xlab="", ylab="Ozono [ppb]", ylim=c(0,175))
axis(1, subset(1:153, Day==1), paste(month.abb[5:9], "1", sep=" "))
axis(4, c(120, 140, 160), cex.axis=0.7)
abline(h=c(120, 140, 160), col=c("blue", "green", "red"), lty=2)
wh=subset(1:153, Ozone >= 120)
wh
text(wh, Ozone[wh]+5, paste(month.abb[Month][wh], Day[wh], sep=" "))

a <- seq(0, 300, 10)
plot(Solar.R, Ozone, main="Modelo Lineal", pch=16)

?gc