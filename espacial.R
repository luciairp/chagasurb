rm(list=ls())

# paquetes que oy a usar
library(maptools)
library(spatstat)
library(rgdal)
library(sp)

# cargo datos que vienen de limpieza sobre datos en chagas6
# write.csv(viv_trab, "data_cata.csv")
data <- read.csv("data_cata.csv",header=T,sep=",",dec=".")
data$latitud <- as.numeric(data$latitud)
data$longitud <- as.numeric(data$longitud)
data <- data[complete.cases(data$longitud), ]  #sacar los NA
data$infestans <- as.factor(ifelse(data$posiTi > 0,
                                   "presence", "absence")) #defino una nueva variable para presencia/ausencia

# proyeccion utm
crs_utm <- "+proj=utm +zone=19 +south +datum=WGS84 +units=m +no_defs +type=crs"

# Convierte datos lat/lon en UTM 
cord.dec <-  SpatialPoints(cbind(data$longitud, data$latitud), proj4string=CRS("+proj=longlat"))
cord.UTM <- spTransform(cord.dec, CRS(crs_utm))
# agrego columna utm a base de datos
data$x <- cord.UTM$coords.x1
data$y <- cord.UTM$coords.x2

head(data)

# generar patrón de puntos

x_infestans <- ppp(data$x, data$y, 
                   window= ripras(data$x, data$y, shape="convex", 1.1),
                   unitname=c("metre","metres"))

plot(x_infestans, main = "T. infestans Catamarca", pch = 20)



# miro distanca entre vecinos
media <- mean(nndist(x_infestans))
stderr(nndist(x_infestans))
sd(nndist(x_infestans))
SE <- (sd(nndist(x_infestans)))/sqrt(length(nndist(x_infestans))-1)
#
hist(nndist(x_infestans))


# patrón marcado

x_infestans_marks <- ppp(data$x, data$y, marks = data$infestans,
                         window= ripras(data$x, data$y, shape="convex", 1.1),
                         unitname=c("metre","metres"))

split(x_infestans_marks) #summary by marks

plot(split(x_infestans_marks), main = "T. infestans Catamarca", pch = 20)

# descriptivo hotspots
raster_density <- Smooth(x_infestans_marks, sigma=100, geometric=T)
plot(raster_density)

# K Ripley marcado
# viviendas al azar con presencia de infestación al azar
Lbug <- envelope(x_infestans, Lest, nsim = 99, rank = 1, i =
                   "presence", global = F)

# sitios fijos
Lmulti <- envelope(x_infestans_marks, Lcross, nsim = 99,rank = 1, i =
                     "presence", global = FALSE, simulate =
                     expression(rlabel(x_infestans_marks)))

plot(Lbug) #agregado a todos los radios
plot(Lmulti) # agregado hasta 100 m teniendo en cuenta disponibilidad de casas
