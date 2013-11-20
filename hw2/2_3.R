
merol <- with(ssn, ssn.stats)

names(merol)
?aggregate
names(ssn)
joe <- aggregate(. ~ YEAR + SSN + DEV, data=ssn,
                 FUN=function(x) c(SUM=sum(x), MEAN=mean(x),
                                   QUANT=quantile(x, probs=c(0.25, 0.50, 0.75))))
names(joe)

#try mil
merol <- split(ssn, ssn$YEAR)
merol$"1984"
str(merol)
merolandia <- lapply(merol ,function(year) sum(year$SSN))
merolandia
?do.call
?rbind
camellos <- do.call(rbind, merolandia)
str(camellos)
camellos$"1984"
class(camellos)
camellos[,1:1]
dim(camellos)
library(data.table)
DT <- data.table(ssn)

DT[, .SD[sum(SSN), ], by=YEAR]



names(joe)
head(joe)
str(joe$SSN)
joe$SSN
joe.test <- merge(joe, ssn)
names(joe.test)
#ssn.years <- merge(ssn.stats, ssn)
names(ssn.stats)
head(ssn.stats, 30)

nrow(ssn.years) - (2013-1749) ## ?? hay diferencia de 1 columna
(nrow(ssn.years)-6) / 11
ssn.years$CICLO <- c(1:6, rep(1:11, 23), 1:6)
#ciclo <- rep(1, 6)
#for(i in 2:24){
#    ciclo <- c(ciclo, rep(i, 11))
#}
#ciclo <- c(ciclo, rep(25, 6))
#ssn.years$CICLO.2 <- ciclo

head(ssn.years, 10)

# Esto funciona si se enumeran los ciclos y no los años
#split(ssn.years, c(1:6, rep(1:11, 23), 1:6))
names(ssn.years)
ssn.cicles = aggregate(SSN.SUM ~ CICLO.2, data=ssn.years, 
                       FUN=function(x) c(SUM=sum(x)))







### ================================================================================
### ================================================================================
### ================================================================================


ssn.count <- ssn.stats[1, ]$SSN.SUM
ssn.sum <- c()
ssn.year <- c()
ssn.list <- list()
for(c.row in 2:nrow(ssn.stats)){
    if(ssn.stats[c.row, ]$CYCLE == 1){
        # Si el indicador de ciclo = 1 entonces se guarda el 
        # contador de manchas y se reinicia para el sig. ciclo
        ssn.sum <- c(ssn.sum, ssn.count)
        ssn.list[[c.row]] <- data.frame(sum=ssn.stats[c.row, ]$SSN.SUM, year=ssn.year)
        ssn.year <- c()
        ssn.count <- 0
    }
    ssn.year <- c(ssn.year, ssn.stats[c.row, ]$YEAR)
    ssn.count <- ssn.count + ssn.stats[c.row, ]$SSN.SUM
}
# El ultimo ciclo no se cierra
ssn.sum <- c(ssn.sum, ssn.count)
ssn.cycles <- data.frame(ssn.count=ssn.sum, ssn.cycle=seq(1, length(ssn.sum)))


?split

?which
?factor
factor(ssn.stats, levels=1:11)
which(ssn.cycles == min(ssn.cycles))

ggplot(ssn.cycles, aes(x=ssn.cycle)) + geom_histogram()

qplot(ssn.cycle, data=ssn.cycles, weight=ssn.count, geom="histogram", binwidth=1)
axis(1, at=ssn.cycles$ssn.cycle, labels=T)

names(ssn.stats)
ggplot(ssn.stats, aes(x=CICLO), binwidth=1) + geom_histogram()

ssn.stats

?palette
?plot
?lines


### ================================================================================
### ================================================================================
### ================================================================================