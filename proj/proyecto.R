## Gstat: http://cran.r-project.org/web/packages/gstat/index.html
## Clustering de Puntos: http://casoilresource.lawr.ucdavis.edu/drupal/node/340
## Ejemplo: http://pages.stern.nyu.edu/~achinco/programming_examples/Example__PlotGeographicDensity.html
## RgoogleMaps: http://cran.r-project.org/web/packages/RgoogleMaps/index.html


library(ggmap)
library(ggplot2)
g.map <- get_map(location = c(lon = -99.1393, lat = 19.3772), 
              zoom = 11, maptype = 'roadmap')
map <- ggmap(g.map)

coordinates <- read.csv(file="100coords.txt", header=F)
names(coordinates) <- c("x", "y")
coordinates

map + geom_point(data = coordinates, aes(x = x, y = y), colour = "red", size = 2)
