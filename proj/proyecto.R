## Gstat: http://cran.r-project.org/web/packages/gstat/index.html
## Clustering de Puntos: http://casoilresource.lawr.ucdavis.edu/drupal/node/340
## Ejemplo: http://pages.stern.nyu.edu/~achinco/programming_examples/Example__PlotGeographicDensity.html
## RgoogleMaps: http://cran.r-project.org/web/packages/RgoogleMaps/index.html

library(ggplot2)
library(ggmap)
library(plyr)
library(reshape2)

## Bajar el mapa
g.map <- get_map(location = c(lon = -99.1393, lat = 19.3772), 
              zoom = 11, maptype = 'roadmap')
map <- ggmap(g.map)

setwd("~/r-workspace/comp-stats/proj")

happy.coordinates <- read.csv(file="coord_felices.txt", header=F)
names(happy.coordinates) <- c("x", "y")
head(happy.coordinates)

sad.coordinates <- read.csv(file="coord_tristes.txt", header=F)
names(sad.coordinates) <- c("x", "y")
head(sad.coordinates)

all.tweets <- rbind(happy.coordinates,sad.coordinates)

#Dibuja todos los tweets
#coord_map <- map + 
#    geom_point(data = happy.coordinates, aes(x = x, y = y), colour = "green", size = 2) + 
#    geom_point(data = sad.coordinates, aes(x=x, y = y), colour="red", size=2)
#coord_map

#########################
# Tomamos una muestra de los tweets
#########################
sample.size <- .05
n.sad <- dim(sad.coordinates)[1]
n.happy <- dim(happy.coordinates)[1]

sad.sample <- data.frame(
    sad.coordinates[sample(1:n.sad, size = n.sad * sample.size), ], 
    type=as.factor(0))

happy.sample <- data.frame(
    happy.coordinates[sample(1:n.happy, size = n.happy * sample.size), ], 
    type=as.factor(1))

sample <- data.frame(rbind(happy.sample, sad.sample))

map + geom_point(data = sample, aes(x = x, y = y, colour = type))



######################
# Devuelve un DF con las coordenadas de la rejilla
# Params:
#     cut.length - longitud de corte en x
#     cut.width  - longitud de corte en y
#     xmin, xmax - dimensiones de la caja a cuadricular en x
#     ymin, ymax - dimensiones de la caja a cuadricular en y
######################
build.lattice <- function(cut.length, cut.width, xmin, xmax, ymin, ymax){
  x.seq <- seq(xmin, xmax, cut.length)
  y.seq <- seq(ymin, ymax, cut.width)
  expand.grid(x = x.seq, y = y.seq)
}

x_min = -99.3
x_max = -98.98
dx = 0.009

y_min = 19.20
y_max = 19.58
dy = 0.009

bounds <- build.lattice(dx, dy, 
                        xmin = x_min, xmax = x_max, 
                        ymin = y_min, ymax = y_max)

map + geom_point(data = sample, aes(x = x, y = y, colour = type)) + 
    geom_point(data=bounds, aes(x = x, y = y), color="green", size=1)
    
##################
# Devuelve la cuenta de tweets dentro de la caja formada por (upright.corner, upright.corner + length)
# Params:
#         data - dataframe con las coordenadas de los tweets en x y y.
#         upleft.corner - dataframe con las coordenadas de la esquina superior izquierda en x y y.
#         x.length - longitud de la caja en x
#         y.length - longitud de la caja en y
##################
cuenta.tweets <- function(data, upleft.corner, x.length, y.length){
  sum(data$x > upleft.corner$x & data$x < upleft.corner$x + x.length & data$y > upleft.corner$y & data$y < upleft.corner$y + y.length)
}

tweets.por.cuadricula <- sapply(1:dim(bounds)[1], function(i){
    c(cuenta.tweets(happy.coordinates, bounds[i,], dx, dy), cuenta.tweets(sad.coordinates, bounds[i,], dx, dy))
})

tweets.df <- data.frame(x = seq(x_min,x_max,dx), y = seq(y_min, y_max,dy), pos = tweets.por.cuadricula[1,], neg = tweets.por.cuadricula[2,], c = tweets.por.cuadricula[1,] + tweets.por.cuadricula[2,])
summary(tweets.df)
tweets.melt <- melt(tweets.df[which(tweets.df$pos + tweets.df$neg > 0),], id=c("x","y"))

############# Mapa de las posiciones con mas tweets ################
head(tweets.df)

map + geom_point(aes(x = x, y = y, size = c, colour = c), 
                 data=tweets.df[which(tweets.df$c > 0),])

map + geom_point(aes(x = x, y = y, colour = variable, size = value), 
                 data = tweets.melt)+facet_wrap(~variable) + 
    scale_fill_manual(values = c("pos" = "blue", "neg" = "red", "c" = "purple"))

############# HEATMAP ##################
## Todos los tweets
map + stat_density2d(aes(x = x, y = y, fill = ..level.., alpha = ..level..),data=all.tweets, size = 2, bins = 4, geom = 'polygon') + scale_fill_gradient(high="#ff0000")+scale_alpha(range=c(.4,.75), guide=FALSE)+guides(fill = guide_colorbar(bardwidht=1.5, barweight=10))



## ======================================================================
##                      PRUEBAS DE HIPOTESIS
## ======================================================================
##
# Hipotesis 0: El número de tweets en cualquier punto de la ciudad es igual
#              ie., la distribución de tweets es uniforme en la cuadricula
# Hipotesis 1: La distribución no es uniforme en la cuadricula
#
# Bajo H0, tenemos 1548 puntos que tienen que tener el mismo numero de tweets cada uno (total / 1548).
# Podemos utilizar el estadistico Chi para medir que tan buena es la aproximación uniforme
# a los datos encontrados.
chisq.test(tweets.df$c)
# Evidentemente, rechazamos la hipótesis nula de que todos los tweets se 
# encuentran dispersos uniformemente

##
#
# Considerando solamente tweets negativos
#
chisq.test(tweets.df$neg)


## ======================================================================
## ======================================================================
## ======================================================================
## ======================================================================
#### Usando tiempo
setwd("~/r-workspace/comp-stats/proj")
tweets <- read.csv("tuits_24nov.txt", header=F)
names(tweets) <- c("mood", "datetime", "x", "y")
tweets$mood <- as.factor(tweets$mood)
### Arreglar la zona horaria, cambio de horario = 27 de Octubre
tweets$datetime <- as.POSIXct(strptime(tweets$datetime, "%Y-%m-%d %H:%M:%S"), 
                              tz="Europe/London")
tweets$mex_datetime <- as.POSIXct(tweets$datetime, tz="America/Mexico_City")
attributes(tweets$mex_datetime)$tzone <- "America/Mexico_City"
summary(tweets)

as.POSIXlt(tweets$datetime[1])$wday
as.POSIXlt(tweets$mex_datetime[1])$hour

# wday: extrae el dia de la semana de una fecha. Domingo = 0
# hour: lo mismo para la hora
tweets$weekday <- as.factor(as.POSIXlt(tweets$mex_datetime)$wday)
tweets$hour <- as.factor(as.POSIXlt(tweets$mex_datetime)$hour)
summary(tweets)

?subset
nrow(subset(tweets, hour %in% (0:6)))
nrow(subset(tweets, hour %in% (7:12)))
nrow(subset(tweets, hour %in% (13:18)))
nrow(subset(tweets, hour %in% (19:23)))

tweets$hour.frame <- 0
tweets[tweets$hour %in% (0:6), ]$hour.frame <- 0
tweets[tweets$hour %in% (7:12), ]$hour.frame <- 1
tweets[tweets$hour %in% (13:18), ]$hour.frame <- 2
tweets[tweets$hour %in% (19:23), ]$hour.frame <- 3
tweets$hour.frame <- as.factor(tweets$hour.frame)
summary(tweets)

g.map <- get_map(location = c(lon = -99.1393, lat = 19.3772), 
                 zoom = 11, maptype = 'roadmap')
map <- ggmap(g.map)

happy.map <- map + 
    geom_point(data = subset(tweets, hour %in% (0:6) & mood == 1 & weekday == 1), 
               aes(x = x, y = y), colour = "green", size = 2)
happy.map

map.grid.mood <- expand.grid(hour.frame=0:3, weekday=0:6, mood=c(-1, 1))
map.grid <- expand.grid(hour.frame=0:3, weekday=0:6)

?subset
?sapply

maps <- list()

?ggtitle
ddply(map.grid, c("hour.frame", "weekday"), function(p) {
    this.points <- subset(tweets, 
                          hour.frame == p$hour.frame & weekday == p$weekday,
                          select = c("x", "y", "mood"))
    map.name = paste("hora_", p$hour.frame, "_dia_", p$weekday, ".jpg", sep="")
    map <- (map + geom_point(data = this.points, 
                          aes(x = x, y = y, colour = mood), size = 1) + 
                ggtitle(map.name))
    ggsave(filename=map.name, plot=map)
    NULL
})


map + geom_point(data = subset(tweets, 
                               hour.frame == 0 & weekday == 0,
                               select = c("x", "y", "mood")), 
                         aes(x = x, y = y, colour = mood), size = 1)

?geom_point
maps
length(all.maps$x)
nrow(tweets)

perro <- read.csv('/Users/Alfonso/Documents/MCC/Aprendizaje/Proyecto/0_array.csv', header=F)
m.perro <- as.matrix(perro)
m.perro[1,]
nrow(m.perro)
?image
image(m.perro)
image(m.perro[20:nrow(m.perro), 20:(ncol(m.perro))])


image(as.matrix(
    read.csv("~/Documents/MCC/Aprendizaje/Proyecto/trim_dog/10.csv", header=F),
))

# http://opencvpython.blogspot.mx/2012/06/hi-this-article-is-tutorial-which-try.html
# http://stackoverflow.com/questions/16538774/dealing-with-contours-and-bounding-rectangle-in-opencv-2-4-python-2-7
# http://www.mathworks.com/help/matlab/ref/isempty.html
# http://www.mathworks.com/help/images/ref/imcrop.html
# http://www.mathworks.com/matlabcentral/fileexchange/23629-exportfig/content/export_fig.m
# http://stackoverflow.com/questions/2123968/r-array-manipulation
# http://enumap.wordpress.com/2012/11/23/python-opencv-resize-image/
# http://stackoverflow.com/questions/10965417/how-to-convert-numpy-array-to-pil-image-applying-matplotlib-colormap
# 
