library(ggplot2)
library(gstat)
library(sp)
library(maptools)
## Checking rgeos availability: TRUE
setwd("C:/Users/ANDRES FELIPE/Desktop/andres/univesidad nacional/semestre II/exploratoria")

ruido_total<- read.csv2(file = "BD FINAL MAPA DE RUIDO-PROMEDIOS.csv", 
                                   header = TRUE)
names(ruido_total)
ruido_mañana <- subset(ruido_total, jornada < 2)
ruido_mañana_test <- ruido_mañana
ruido_mañana_test$x <- ruido_mañana_test$longitud
ruido_mañana_test$y <- ruido_mañana_test$latitud
coordinates(ruido_mañana_test) = ~x + y
plot(ruido_mañana_test)
x.range <- range(ruido_mañana_test$x) 
y.range <- range(ruido_mañana_test$y)
proporcion <- 0.1
margenes.x <- diff(x.range)*proporcion
margenes.y <- diff(y.range)*proporcion
x.range[1] <- x.range[1] - margenes.x
x.range[2] <- x.range[2] + margenes.x
y.range[1] <- y.range[1] - margenes.y
y.range[2] <- y.range[2] + margenes.y
grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 0.00001),
                   y = seq(from = y.range[1], to = y.range[2], by = 0.00001))  
coordinates(grd) <- ~x + y
gridded(grd) <- TRUE
plot(grd, cex = 1.5, col = "grey")
points(ruido_mañana_test, pch = 1, col = "red", cex = 1)
idw <- idw(formula = prom ~ 1, locations = ruido_mañana_test, grd)  
## [inverse distance weighted interpolation]
idw.output = as.data.frame(idw)  
names(idw.output)[1:3] <- c("long", "lat", "var1.pred")
require(ggplot2)
ggplot() + geom_raster(data = idw.output, aes(x = long, y = lat, fill = var1.pred)) + 
  scale_fill_gradientn(name= "Decibeles",colours = c("green"," orange", "red", "darkred")) +
  geom_point(data = ruido_mañana_test@data, aes(x = longitud, y = latitud), shape = 19, 
             colour = "darkblue") + coord_fixed() + scale_colour_gradient()

ruido_tarde <- subset(ruido_total, jornada > 1)
ruido_tarde_test <- ruido_tarde
ruido_tarde_test$x <- ruido_tarde_test$longitud
ruido_tarde_test$y <- ruido_tarde_test$latitud
coordinates(ruido_tarde_test) = ~x + y
plot(ruido_tarde_test)
x.range <- range(ruido_tarde_test$x) 
y.range <- range(ruido_tarde_test$y)
proporcion <- 0.1
margenes.x <- diff(x.range)*proporcion
margenes.y <- diff(y.range)*proporcion
x.range[1] <- x.range[1] - margenes.x
x.range[2] <- x.range[2] + margenes.x
y.range[1] <- y.range[1] - margenes.y
y.range[2] <- y.range[2] + margenes.y
grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 0.00001),
                   y = seq(from = y.range[1], to = y.range[2], by = 0.00001))  
coordinates(grd) <- ~x + y
gridded(grd) <- TRUE
plot(grd, cex = 1.5, col = "grey")
points(ruido_tarde_test, pch = 1, col = "red", cex = 1)
idw <- idw(formula = prom ~ 1, locations = ruido_tarde_test, newdata = grd)  
## [inverse distance weighted interpolation]
idw.output = as.data.frame(idw)  
names(idw.output)[1:3] <- c("long", "lat", "var1.pred")
require(ggplot2)
ggplot() + geom_raster(data = idw.output, aes(x = long, y = lat, fill = var1.pred)) + 
  scale_fill_gradientn(name= "Decibeles",colours = c("green"," orange", "red", "darkred")) +
  geom_point(data = ruido_tarde_test@data, aes(x = longitud, y = latitud), shape = 19, 
             colour = "darkblue") + coord_fixed() + scale_colour_gradient()

require(raster)
Nucl_volador <- brick("mapa campus volador.tiff")
plotRGB(Nucl_volador)
extent(Nucl_volador) <- c(-75.579211,-75.575345, 6.259005, 6.266511)
