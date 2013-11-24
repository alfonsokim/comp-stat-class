## Gstat: http://cran.r-project.org/web/packages/gstat/index.html
## Clustering de Puntos: http://casoilresource.lawr.ucdavis.edu/drupal/node/340
## Ejemplo: http://pages.stern.nyu.edu/~achinco/programming_examples/Example__PlotGeographicDensity.html
## RgoogleMaps: http://cran.r-project.org/web/packages/RgoogleMaps/index.html

library(ggmap)
library(ggplot2)

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

bounds <- build.lattice(.009, .009, 
                        xmin = -99.3, xmax = -99.0, 
                        ymin = 19.25, ymax = 19.58)

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
    cuenta.tweets(sample, bounds[i,], 0.009, 0.009)
})



