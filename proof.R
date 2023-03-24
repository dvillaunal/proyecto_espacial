
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



dt <- dt0 %>% filter(date == "2019-03-10")



# Conversión de coordenadas geográficas (LongLat)
# a coordenadas planas (UTM)

datos <- as.matrix(data.frame(x=as.numeric(dt$lat),
                              y=as.numeric(dt$long)))

head(datos)



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

