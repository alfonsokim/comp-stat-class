## Gstat: http://cran.r-project.org/web/packages/gstat/index.html
## Clustering de Puntos: http://casoilresource.lawr.ucdavis.edu/drupal/node/340
## Ejemplo: http://pages.stern.nyu.edu/~achinco/programming_examples/Example__PlotGeographicDensity.html
## RgoogleMaps: http://cran.r-project.org/web/packages/RgoogleMaps/index.html

library(ggmap)
library(ggplot2)
g.map <- get_map(location = c(lon = -99.1393, lat = 19.3772), 
              zoom = 11, maptype = 'roadmap')
map <- ggmap(g.map)

happy.coordinates <- read.csv(file="coord_felices.txt", header=F)
names(happy.coordinates) <- c("x", "y")
head(happy.coordinates)

sad.coordinates <- read.csv(file="coord_tristes.txt", header=F)
names(sad.coordinates) <- c("x", "y")
head(sad.coordinates)

coord_map <- map + geom_point(data = happy.coordinates, aes(x = x, y = y), colour = "green", size = 2) + geom_point(data = sad.coordinates, aes(x=x, y = y), colour="red", size=2)
coord_map

#Dibujamos 2 clusters en el mapa (y uno por afuera)
#TODO: revisar por q falla
clusters <- kmeans(coordinates, 3, iter.max = 100, nstart=3)
centers <- data.frame(x = clusters$centers[,1], y = clusters$centers[,2])
cluster_map <- coord_map + geom_point(data = centers, aes(x=x, y=y), colour="blue", size=4)
cluster_map