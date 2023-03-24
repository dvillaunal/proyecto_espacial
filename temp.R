
library(geoR); library(gstat) ;library(sf);library(raster)
library(rgdal)
library(vegan)
library(ade4)
library(GADMTools)
library(readxl)
library(tidyverse)
require(magrittr)
library(tmaptools) # Geocodificación y visualización
library(tidygeocoder) # Geocodificación
library(leaflet) # Visualización 
library(sm)
require(gstat)
require(MASS)

municipios<-gadm_sf_loadCountries(c("COL"), level=2, basefile="./")    

municipios$sf %>% head()




# Municipios de Estudio dentro de la región:
Antioquia <- gadm_subset(municipios, level=1,
                            regions=("Antioquia"))

gadm_plot(Antioquia, title = "Antioquia (División Politíca)") %>%
  gadm_showNorth("tl") %>% gadm_showScale('bl')



borderland <- st_coordinates(Antioquia$sf)
borderland <- as.data.frame(borderland[,1:2])



dt0 <- read_csv("temperature.csv")
head(dt0)



dt2 <- dt0 %>% filter(date == "2019-01-10") %>% na.omit()



# Conversión de coordenadas geográficas (LongLat)
# a coordenadas planas (UTM)

datos2 <- as.matrix(data.frame(x=as.numeric(dt2$lat),
                              y=as.numeric(dt2$long)))

head(datos2)



x1.2 <- as.numeric(dt2$tmin)
x2.2 <- as.numeric(dt2$tmax)
x3.2 <- as.numeric(dt2$elev)
df2 <- cbind(datos2, x1.2,x2.2,x3.2)
df2 <- data.frame(df2)
colnames(df2)<-c("X","Y", "Temp min", "Temp max", "elev")

geo_temp.max2 <- as.geodata(df2, coords = 1:2, data.col = 4, borders=TRUE)
geo_temp.max2$borderland<- borderland
datos2.sp <- df2
coordinates(datos2.sp) <- ~X+Y



plot(geo_temp.max2, lowess=TRUE)



plot(geo_temp.max2, lowess=TRUE, trend = "2nd")



qqnorm(df2$`Temp max`, pch = 19)
qqline(df2$`Temp max`)



shapiro.test(df2$`Temp max`)



dt <- dt0 %>% filter(date == "2019-03-10") %>% na.omit()

# Conversión de coordenadas geográficas (LongLat)
# a coordenadas planas (UTM)

datos <- as.matrix(data.frame(x=as.numeric(dt$lat),
                              y=as.numeric(dt$long)))

x1 <- as.numeric(dt$tmin)
x2 <- as.numeric(dt$tmax)
x3 <- as.numeric(dt$elev)
df <- cbind(datos, x1,x2,x3)
df <- data.frame(df)
colnames(df)<-c("X","Y", "Temp min", "Temp max", "elev")

geo_temp.max <- as.geodata(df, coords = 1:2, data.col = 4, borders=TRUE)
geo_temp.max$borderland<- borderland
datos.sp <- df
coordinates(datos.sp) <- ~X+Y

plot(geo_temp.max, lowess=TRUE)



qqnorm(df$`Temp max`)
qqline(df$`Temp max`)

shapiro.test(df$`Temp max`)



distancia1 <- dist(df[,1:2], diag=TRUE, upper=TRUE)

dif.temp1 <- dist(df[,4], diag=TRUE, upper=TRUE)^2

distancia2 <- dist(df2[,1:2], diag=TRUE, upper=TRUE)

dif.temp2 <- dist(df2[,4], diag=TRUE, upper=TRUE)^2



datosmatriz1 <- data.frame(dif.prof=dif.temp1[lower.tri(dif.temp1)],
                           distancia1=distancia1[lower.tri(distancia1)])

datosmatriz2 <- data.frame(dif.prof=dif.temp2[lower.tri(dif.temp2)],
                           distancia2=distancia2[lower.tri(distancia2)])


par(mfrow = c(1,2))

# (2019-03-10) [Normal]
plot(x = datosmatriz1$distancia1, y = datosmatriz1$dif.prof,
     main = "2019-03-10 (Normal)", ylab = "Diferencia en temperatura",
     xlab = "Distancia (m)")
abline(lm(dif.prof~distancia1, data = datosmatriz1), lwd = 2,
       col = "red")

# (2019-01-10) [NO Normal]
plot(x = datosmatriz2$distancia2, y = datosmatriz2$dif.prof,
     main = "2019-01-10 (No Normal)", ylab = "Diferencia en temperatura",
     xlab = "Distancia (m)")
abline(lm(dif.prof~distancia2, data = datosmatriz2), lwd = 2,
       col = "red")



mantel(distancia1, dif.temp1, na.rm = T)



mantel.rtest(distancia1, dif.temp1, nrepet=999)



mantel(distancia2, dif.temp2)
mantel.rtest(distancia2, dif.temp2)



resumen <- summary(geo_temp.max)
distancia <- resumen$distances.summary
dm <- distancia[2]/2; dm


s


bin1 <- variog(geo_temp.max,option="bin")
bin2 <- variog(geo_temp.max,option="bin",max.dist=dm)

par(mfrow=c(1,2))
plot(bin1, pch=21, bg="blue", lwd="black",
     main="Semivariograma sin ajuste\ncon regla empírica")
plot(bin2, pch=21, bg="red", lwd="black",
     main="Semivariograma ajuste\ncon regla empírica")



par(mfrow = c(1,1))
sm.variogram(datos.sp@coords, datos.sp@data$`Temp max`,model="independent",
             ylim = c(0,70))


par(mfrow= c(1,1))
ini.vals1 <- c(16.201,0.531)
fit_matern <- variofit(vario = bin2,ini.cov.pars = ini.vals1, fix.nugget =F,
                   weights = "npairs", min="optim", nugget = 0.03)

plot(bin2, pch=21, bg="blue", lwd="black",
     main="Semivariograma con ajuste Matern")
lines(fit_matern)



eyefit(bin1) # exponencial siga 17.34 phi 1.48 tausq 2.17 range 4.4336



eyefit(bin1)



eyefit(bin1)



eyefit(bin1)



eyefit(bin1)


library(gstat)

df %<>% clean_names()
df$temp_max <- as.numeric(df$temp_max)

# crear un objeto SpatialPointsDataFrame a partir de df
coordinates(df) <- c("x", "y")

# definir la proyección (en este caso, la misma que la de latitud/longitud)
proj4string(df) <- CRS("+proj=longlat +datum=WGS84")

# calcular el variograma
v <- variogram(temp_max ~ 1, data = df)

# fit variogram model
vgm <- fit.variogram(v, vgm("Mat", psill = 1, range = 10, nugget = 0.1))

# Extraer los parámetros del modelo ajustado
ini.vals1 <- c(psill = vgm$psill, range = vgm$range)

v_geoR <- as.georVariogram(v)

# ajustar el modelo materno
fit_matern <- variofit(vario = v_geoR, cov.model = "matern",
                       fix.nugget = FALSE, weights = "npairs",
                       min.method="optim", nugget = 0.03,
                       cov.pars = ini.vals1, max.dist = NULL)



