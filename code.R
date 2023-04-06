
library(geoR); library(gstat) ;library(sf);library(raster)
library(rgdal)
library(vegan)
library(ade4)
library(tidyverse)
require(magrittr)
library(tmaptools) # Geocodificación y visualización
library(tidygeocoder) # Geocodificación
library(leaflet) # Visualización 
library(sm)
require(gstat)
require(MASS)



col <- st_read("databases/MGN_DPTO_POLITICO.shp") # Archivo de interés
plot(col$geometry) 

antioquia <- col[14,"geometry"]
border_ant <- st_coordinates(antioquia) #matriz del sistema coordenadas
border_ant <- as.data.frame(border_ant[,1:2])



dt0 <- read_csv("databases/temperature.csv")
head(dt0)



dt2 <- dt0 %>% filter(date == "2019-01-10") %>% na.omit()
dt2 <- dt2[-c(12, 10, 11, 17, 28, 31, 29, 4, 1),]



leaflet(antioquia) %>%
  addPolygons() %>% 
  addTiles() %>% 
  addProviderTiles(providers$OpenTopoMap) %>% 
  addMarkers(lng = dt2$long, lat = dt2$lat,
             popup = paste0("Elevación (sobre el nivel del mar): ",dt2$elev, "m"))



# Conversión de coordenadas geográficas (LongLat)
# a coordenadas planas (UTM)

datos2 <- as.matrix(data.frame(x=as.numeric(dt2$long),
                              y=as.numeric(dt2$lat)))



x1.2 <- as.numeric(dt2$tmin)
x2.2 <- as.numeric(dt2$tmax)
x3.2 <- as.numeric(dt2$elev)
df2 <- cbind(datos2, x1.2,x2.2,x3.2)
df2 <- data.frame(df2)
colnames(df2)<-c("X","Y", "Temp min", "Temp max", "elev")

geo_temp.max2 <- as.geodata(df2, coords = 1:2, data.col = 4, borders=TRUE)
geo_temp.max2$borders <- border_ant
datos2.sp <- df2
coordinates(datos2.sp) <- ~X+Y
proj4string(datos2.sp) <- CRS("+init=epsg:4326")



points(geo_temp.max2, col = "blue3", pt.divide = "equal",
       xlab = "Longitud", ylab = "Latitud")
plot(geo_temp.max2, lowess=TRUE)



plot(geo_temp.max2, lowess=TRUE, trend = "2nd")



qqnorm(df2$`Temp max`, pch = 19)
qqline(df2$`Temp max`)



shapiro.test(df2$`Temp max`)



dt <- dt0 %>% filter(date == "2019-03-10") %>% na.omit()
dt <- dt[-c(12, 10, 11, 17, 28, 31, 29, 4, 1),]

datos <- as.matrix(data.frame(x=as.numeric(dt$long),
                              y=as.numeric(dt$lat)))

x1 <- as.numeric(dt$tmin)
x2 <- as.numeric(dt$tmax)
x3 <- as.numeric(dt$elev)
df <- cbind(datos, x1,x2,x3)
df <- data.frame(df)
colnames(df)<-c("X","Y", "Temp min", "Temp max", "elev")

geo_temp.max <- as.geodata(df, coords = 1:2, data.col = 4, borders=TRUE)
geo_temp.max$borders <- border_ant
datos.sp <- df
coordinates(datos.sp) <- ~X+Y
proj4string(datos.sp) <- CRS("+init=epsg:4326")

plot(geo_temp.max, lowess=TRUE, trend = "2nd")

# Conversión de coordenadas geográficas (LongLat)
# a coordenadas planas (UTM)

datos.sp.utm <- spTransform(datos.sp, CRS("+init=epsg:9377 +units=m"))
datos.sp.utm <- as.data.frame(datos.sp.utm)

coordinates(border_ant) <- ~X+Y
proj4string(border_ant) <- CRS("+init=epsg:4326")
border_ant.utm <- spTransform(border_ant, CRS("+init=epsg:9377 +units=m")) %>% as.data.frame()

geo_temp.max.utm <- as.geodata(datos.sp.utm, coords = 4:5, data.col = 2, borders =TRUE)
geo_temp.max.utm$borders <- border_ant.utm

# Estadístico de resumen con escala UTM
plot(geo_temp.max.utm, lowess=TRUE,trend = "2nd")

qqnorm(df$`Temp max`, pch = 19)
qqline(df$`Temp max`)

shapiro.test(df$`Temp max`)

# UTM
distancia <- dist(datos.sp.utm[,4:5], diag=TRUE, upper=TRUE)

dif.temp <- dist(datos.sp.utm[,2], diag=TRUE, upper=TRUE)^2

# Latitud longitud

distancia1 <- dist(coordinates(datos.sp), diag=TRUE, upper=TRUE)

dif.temp1 <- dist(datos.sp$`Temp max`, diag=TRUE, upper=TRUE)^2

distancia2 <- dist(df2[,1:2], diag=TRUE, upper=TRUE)

dif.temp2 <- dist(df2[,4], diag=TRUE, upper=TRUE)^2


datosmatriz <- data.frame(dif.prof=dif.temp[lower.tri(dif.temp)],
                           distancia=distancia[lower.tri(distancia)])

datosmatriz1 <- data.frame(dif.prof=dif.temp1[lower.tri(dif.temp1)],
                           distancia1=distancia1[lower.tri(distancia1)])

datosmatriz2 <- data.frame(dif.prof=dif.temp2[lower.tri(dif.temp2)],
                           distancia2=distancia2[lower.tri(distancia2)])


par(mfrow = c(1,3))

# (2019-03-10) [Normal]
plot(x = datosmatriz1$distancia1, y = datosmatriz1$dif.prof,
     main = "2019-03-10 (Normal)", ylab = "Diferencia en temperatura",
     xlab = "Distancia (m)")
abline(lm(dif.prof~distancia1, data = datosmatriz1), lwd = 2,
       col = "red")

plot(x = datosmatriz$distancia, y = datosmatriz$dif.prof,
     main = "2019-03-10 (Normal-UTM)", ylab = "Diferencia en temperatura",
     xlab = "Distancia (m)")
abline(lm(dif.prof~distancia, data = datosmatriz), lwd = 2,
       col = "red")

# (2019-01-10) [NO Normal]
plot(x = datosmatriz2$distancia2, y = datosmatriz2$dif.prof,
     main = "2019-01-10 (No Normal)", ylab = "Diferencia en temperatura",
     xlab = "Distancia (m)")
abline(lm(dif.prof~distancia2, data = datosmatriz2), lwd = 2,
       col = "red")


mantel(distancia, dif.temp, na.rm = T)

mantel.rtest(distancia, dif.temp, nrepet=999)

mantel(distancia1, dif.temp1)
mantel.rtest(distancia1, dif.temp1)


mantel(distancia2, dif.temp2)
mantel.rtest(distancia2, dif.temp2)



resumen <- summary(geo_temp.max)
distancia <- resumen$distances.summary
dm <- distancia[2]/2; dm



vario1 <- variog(geo_temp.max, option = "cloud")
vario2 <- variog(geo_temp.max, option = "cloud", max.dist = dm)
par(mfrow=c(1,2))
plot(vario1, pch=21, bg="blue", lwd="black",
     main="Variograma sin ajuste\ncon regla empírica")
plot(vario2, pch=21, bg="red", lwd="black",
     main="Variograma ajuste\ncon regla empírica")



bin1 <- variog(geo_temp.max,option="bin")
bin2 <- variog(geo_temp.max,option="bin",max.dist=dm)

par(mfrow=c(1,2))
plot(bin1, pch=21, bg="blue", lwd="black",
     main="Semivariograma sin ajuste\ncon regla empírica")
plot(bin2, pch=21, bg="red", lwd="black",
     main="Semivariograma ajuste\ncon regla empírica")



par(mfrow = c(1,1))
sm.variogram(datos.sp@coords, datos.sp@data$`Temp max`,model="independent",
             ylim = c(0,20))



ini.vals1 <- c(11.02,0.6)
fit_matern <- variofit(vario = bin2,ini.cov.pars = ini.vals1, fix.nugget =F,
                   weights = "npairs", min="optim", nugget = 0.03)

plot(bin2, pch=21, bg="blue", lwd="black",
     main="Semivariograma con ajuste Matern")
lines(fit_matern, lwd = 2, col = "red")



eyefit(bin1) # exponencial siga 17.34 phi 1.48 tausq 2.17 range 4.4336



eyefit(bin1)



eyefit(bin1)



eyefit(bin1)



eyefit(bin1)

