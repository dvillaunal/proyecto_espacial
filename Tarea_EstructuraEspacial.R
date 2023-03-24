library(readxl)
library(geoR)
library(ade4)
library(vegan)

contorno <- read.table('cienaga_border.txt', header = T)
base.cienaga <- read_excel('base_cienaga.xls')

geo.datos <- as.geodata(base.cienaga, coords = 1:2, data.col = 4, borders = TRUE)
geo.datos$borders <- contorno

sm <- summary(geo.datos)
dm <- sm$distances.summary[2]/2
dm

bin1 <- variog(geo.datos, option = "bin")
bin2 <- variog(geo.datos, option = "bin", trend = "1st", max.dist = dm)

par(mfrow = c(1,2))
plot(bin1)
plot(bin2)

bin3 <- variog(geo.datos, option = "bin", estimator.tpe = "modulus",
               max.dist = dm)
plot(bin2, main = "Estimador clásico")
plot(bin3, main = "Estimador robusto")

test.ace <- variog.mc.env(geo.datos, obj.variog = bin2)

par(mfrow = c(1,2))

plot(bin2, main = "Estimador clásico", pch = 19)
lines(test.ace, lwd = 2)

plot(bin3, main = "Estimador robusto", pch = 19)
lines(test.ace, lwd = 2)

# Función para simular el semivariograma experimental ---------------------

coords <-  geo.datos[[1]]
data = geo.datos[[2]]

variog.mc.env2 <- function(coords, data, nsim = 99,
                           estimador.type = "classical", option = "bin"){
  
  require(magrittr)
  require(geoR)
  # Coords => Matrix
  # Data => Vector
  # estimator.type = c("classical", "modulus")
  # option = c("bin", "cloud", "smooth")
  
  n <- length(data)
  
  # Crear vectores para contener la información de los puntos
  vy <- vector(mode = "list", length = nsim)
  vx <- vector(mode = "list", length = nsim)
  
  for (i in seq_len(nsim)) {
    # Permutar la variable según su número de observaciones
    s <- sample(1:n, size = n, replace = F)
    
    # Calculo del semivariograma
    t <- variog(coords = coords,
                data = data[s],option = option,
                estimator.tpe = estimador.type, max.dist = dm,
                messages = FALSE)
    
    # Sacar los valores respectivo del `t`:
    x = t$u; y = t$v
    
    # Guardamos los valores en una lista
    vy[[i]] <- y
    vx[[i]] <- x
    
  }
  
  # Quitar el formato lista de  los puntos
  y <- unlist(vy); x <- unlist(vx)
  
  # Calculamos los valores pertinentes
  max.x <- max(x)
  my <- matrix(y, nrow = nsim, byrow = TRUE)
  
  # Calcular los valores mínimos y máximos de cada punto:
  min.t <- vector(mode = "numeric", length = ncol(my))
  max.t <- vector(mode = "numeric", length = ncol(my))
  for (i in 1:ncol(my)) {
    min.t[i] <- my[,i] %>% min()
    max.t[i] <- my[,i] %>% max()
  }
  
  t <- list(supy = max.t, infy = min.t,maxx = max.x)
  
  return(t)
  
}




variograma <- variog.mc.env2(coords = coords, data = data)

par(mfrow = c(1,1))

bin3$u


plot(bin3, pch = 19)
lines(y = variograma[[1]], x = bin3$u,lwd = 2,
     xlim = c(0,variograma[[3]]), ylim = c(0,5), type = "l", col = "blue")

lines(y = variograma[[2]], x = bin3$u,lwd = 2,
      xlim = c(0,variograma[[3]]), ylim = c(0,5), type = "l", col = "blue")




