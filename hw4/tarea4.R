#### Tarea 4 Estadistica

## 1
azul<-c(4,69,87,35,39,79,31,79,65,95,68,62,70,80,84,79,66,75,59,77,36,
        86,39,85,74,72,69,85,85,72)
rojo<-c(62,80,82,83,0,81,28,69,48,90,63,77,0,55,83,85,54,72,58,68,88,83,
        78,30,58,45,78,64,87,65)

datos = data.frame(
    identificador = c(rep(0, 20), rep(1, 10)),
    azul=azul, rojo=rojo
    )

## 3
?read.table
setwd("~/r-workspace/comp-stats/hw4")
ozone <- read.table("ozone.txt", header=T, sep=" ")
head(ozone)

?lm
lm.model <- lm(maxO3 ~ T9 + T12 + T15 + Ne9 + Ne12 + Ne15 + 
                   Wx9 + Wx12 + Wx15 + maxO3y, data=ozone)

coef(lm.model)
#Ecuacion: maxO3 = 12.24 -0.01T9 + 2.22T12 + 0.56T15 - 
#                  2.19Ne9 - 0.42N12 + 0.18Ne15 + 
#                  0.94Wx9 + 0.03Wx12 + 0.42Wx15 + 0.35max03y



