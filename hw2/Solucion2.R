setwd("~/r-workspace/comp-stats/hw2/")

cable <- read.table("cableTV.dat",header=TRUE)
attach(cable)

hist_tvtot <- hist(tvtot,col="blue")
text(max(tvtot)-3, 5.5 , paste("Maximo =", max(tvtot), sep = " "))
text(min(tvtot)+5, 2.5 , paste("Minimo =", min(tvtot), sep = " "))
text(mean(tvtot), 3 , paste("Media =", mean(tvtot), sep = " "))

?ogive

n <- length(hist_tvtot$breaks)
tabla_tvtot <- cbind(hist_tvtot$breaks[-n],hist_tvtot$breaks[-1],hist_tvtot$counts)
dimnames(tabla_tvtot)[[2]]<-c("Linf","Lsup","f")
print(tabla_tvtot)

tvtot
breaks = seq(1.5, 5.5, by=0.5) 
tvtot.cut = cut(tvtot, breaks=10, right=FALSE) 
tvtot.freq = table(tvtot.cut)

cumfreq0 = c(0, cumsum(prop.table(tvtot.freq))) 
plot(breaks, cumfreq0)            # plot the data 
       +   main="TVs",  # main title 
       +   xlab="X",        # x−axis label 
       +   ylab="Y")   # y−axis label 
lines(breaks, cumfreq0)           # join the points


hist_renta <- hist(renta,col="red")
text(max(renta), 2 , paste("Maximo =", max(renta), sep = " "))
text(min(renta)+6, 3 , paste("Maximo =", min(renta), sep = " "))
text(mean(renta), 10 , paste("Media =", mean(renta), sep = " "))
n <- length(hist_tvtot$breaks)
tabla_renta <- cbind(hist_renta$breaks[-n],hist_renta$breaks[-1],hist_renta$counts)
dimnames(tabla_renta)[[2]]<-c("Linf","Lsup","f")
print(tabla_renta)

hist_valor <- hist(valor,col="green")
text(max(valor)-5000, 4 , paste("M?ximo =", max(valor), sep = " "))
text(min(valor)+6000, 2.5 , paste("M?nimo =", min(valor), sep = " "))
text(mean(valor), 9 , paste("Media =", mean(valor), sep = " "))
n <- length(hist_valor$breaks)
tabla_valor <- cbind(hist_valor$breaks[-n],hist_valor$breaks[-1],hist_valor$counts)
dimnames(tabla_valor)[[2]]<-c("Linf","Lsup","f")
print(tabla_valor)
