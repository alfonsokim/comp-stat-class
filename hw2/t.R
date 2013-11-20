# Ejemplos de graficacion en R

# Ejemplo 1: airquality data

data(airquality)
attach(airquality)
names(airquality)
dim(airquality)

# grafica ozono vs tiempo y concentraciones limite

plot(Ozone, type="h", xaxt="n", main="Calidad del aire en NY",
     xlab="", ylab="Concentracion de Ozono [ppb]", ylim=c(0,175))
axis(1, subset(1:153, Day==1), paste(month.abb[5:9], "1", sep=" "))
axis(4,c(120,140,160),cex.axis=0.7)
abline(h=c(120,140,160),col=c("blue","green","red"),lty=2)
wh=subset(1:153,Ozone>=120)
text(wh,Ozone[wh]+5,paste(month.abb[Month][wh],Day[wh],sep=" "))

# b) grafica ozono vs radiacion UV

plot(Solar.R,Ozone,main="Ozono y radiacion UV",xlab="Radiacion solar",ylab="Concentracion de ozono [ppb]",pch=20)

# Ajustamos una funcion a la grafica anterior

# 1. Regresion lineal

fit.1<-lm(Ozone~Solar.R)  # Simple ajuste lineal a los datos del ozono.
summary(fit.1)   # resultado de la regresion
plot(fit.1)      # graficas derivadas de la regresion

# Si quiero graficar la curva de ajuste por separado

a<-seq(0,300,10)    # secuencia para las graficas de ajuste
plot(Solar.R,Ozone,main="Modelo Lineal",pch=16)
yhat1<-fit.1$coef[1]+fit.1$coef[2]*a
lines(a,yhat1,lwd=2,col="red")

# 2. Regresion cuadratica

fit.2<-lm(Ozone~Solar.R+I(Solar.R^2))
plot(Solar.R,Ozone,main="Modelo cuadratico",pch=16)
yhat2<-fit.2$coef[1]+fit.2$coef[2]*a+fit.2$coef[3]*a^2
lines(a,yhat2,lwd=2,col="blue")

# 3. Regresion no lineal

fit.3<-nls(Ozone~I(Solar.R^power),start=list(power=1))
plot(Solar.R,Ozone,main="Modelo no lineal",pch=16)
lines(a,a^0.7,type="l",col="green")

# c) Graficas condicionales

coplot(Ozone~Solar.R|Wind)
coplot(Ozone~Solar.R|Temp)

wt<-Wind*Temp
coplot(Ozone~Solar.R|wt)


# Ahora con xyplot. Para ello instalo el paquete lattice

install.packages("lattice")
library(lattice)
wind=equal.count(Wind,number=6)  # es para hacer 6 intervalos
temp=equal.count(Temp,number=6)  # es para hacer 6 intervalos

trellis.device(col=F)    # 
xyplot(Ozone~Solar.R|wind)
xyplot(Ozone~Solar.R|temp)
xyplot(Ozone~Solar.R|wind*temp)

################################################################################
################################################################################

# Ejemplo 2: Dibujar graficas de funciones

curve(100*(x^3-x^2)+15,from=0,to=2,
      xlab=expression(alpha),
      ylab=expression(100 %*% (alpha^3-alpha^2)+15),
      main=expression(paste("Funcion:",
                            f(alpha)==100 %*% (alpha^3-alpha^2)+15)))

myMu<-0.5
mySigma<-0.25
par(usr=c(0,1,0,1))   # cambia las coordenadas dentro de la grafica
text(0.1,0.1,bquote(sigma[alpha]==.(mySigma)),cex=1.25)   #bquote sirve para permitir la evaluacion de ciertas partes
text(0.5,0.5,paste("(La media es ",myMu,")",sep=""),cex=1.25)
text(0.5,0.9,bquote(paste("sigma^2=",sigma^2==.(format(mySigma^2,2)))))


################################################################################
################################################################################

# Ejemplo 3: Visualizacion de datos multivariados

data(iris)      # 150 obs de 5 variables
pairs(iris[1:150,1:4])   # grafica de pares de variables (matriz)

# Curva de densidad de probabilidad a lo largo de la diagonal

dens.diag<-function(x){
    usr<-par("usr")   # parametro grafico que especifica los extremos de la region grafica
    on.exit(par(usr))
    par(usr=c(usr[1:2],0,0.5))
    lines(density(x))
}

# Aplicamos scale para estandarizar

x<-scale(iris[1:150,1:4])
pairs(x,diag.panel=dens.diag)


################################################################################
################################################################################

# Ejemplo 4: Visualizacion de datos multivariados (superficie)

# Normal bivariada estandar

func.binormal<-function(x,y){
    z<-(1/(2*pi))*exp(-0.5*(x^2+y^2))
}

x<-seq(-3,3,length=100)
y<-seq(-3,3,length=100)
z<-outer(x,y,func.binormal)   # calcula la densidad para (x,y)


persp(x,y,z)     # paquete graphics

persp(x,y,z,theta=45,phi=35,expand=0.8,ltheta=120,shade=0.8,xlab="x",ylab="y",zlab="f(x,y)")


################################################################################
################################################################################

# Ejemplo 5: Visualizacion de datos multivariados (3D scatterplot)

library(lattice)
attach(iris)

print(cloud(Petal.Length~Sepal.Length*Sepal.Width,data=iris,groups=Species))

# Para ver los 4 en pantalla:

print(cloud(Sepal.Length~Petal.Length*Petal.Width,data=iris,groups=Species,
            pch=1:3,screen=list(z=30,x=-75,y=0)),split=c(1,1,2,2),more=TRUE)

print(cloud(Sepal.Width~Petal.Length*Petal.Width,data=iris,groups=Species,
            pch=1:3,screen=list(z=30,x=-75,y=0)),split=c(2,1,2,2),more=TRUE)

print(cloud(Petal.Length~Sepal.Length*Sepal.Width,data=iris,groups=Species,
            pch=1:3,screen=list(z=30,x=-75,y=0)),split=c(1,2,2,2),more=TRUE)

print(cloud(Petal.Width~Sepal.Length*Sepal.Width,data=iris,groups=Species,
            pch=1:3,screen=list(z=30,x=-75,y=0)),split=c(2,2,2,2),more=TRUE)



################################################################################
################################################################################

# Ejemplo 6: Visualizacion de datos multivariados (contorno)


# se usa contour(graphics) y contourplot(lattice)

data(volcano)  # gives topographic information for Maunga Whau on a 10m by 10m grid. 

contour(volcano,asp=1,labcex=2)


library(lattice)
contourplot(volcano)

#############################################################

# Ejemplo 7: Grafica de correlacion

# Correlaciones pueden ser vistas como elipses y su forma indica el grado de
# correlacion entre variables.

# Cada variable esta correlacionada consigo misma (diagonal muestra lineas rectas),
# y los circulos perfectos indican NO correlacion.

install.packages("ellipse")
library(ellipse)
data(airquality)

complete<-airquality[complete.cases(airquality),]   # remover datos faltantes
plotcorr(cor(complete))

# Otros datos

install.packages("isdals")
library(isdals)
data(fev)
model<-lm(FEV~Ht+I(Ht^2)+Gender+Smoke+Age,data=fev)

corr2<-cov2cor(vcov(model))   # matriz de correlacion
plotcorr(corr2,type="lower",diag=TRUE)
par(new=TRUE)     # mantiene la primera grafica
plotcorr(corr2,type="upper",diag=TRUE,numbers=TRUE)












