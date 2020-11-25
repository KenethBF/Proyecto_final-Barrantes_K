####### Cargar archivo de datos

library(readr)
Metazygia <- read_delim("Metazygia.csv", 
                        ";", escape_double = FALSE, col_types = cols(area = col_number(), 
                                                                     lugar = col_factor(levels = c("bosquecito", 
                                                                                                   "carteles")), luz = col_logical(), 
                                                                     posicion = col_factor(levels = c("alto", 
                                                                                                      "bajo", "medio"))), trim_ws = TRUE)
View(Metazygia)

####### Supuestos param�tricos

# Prueba de normalidad

area.test <- shapiro.test(Metazygia$area) # Prueba Shapiro para normalidad
print(area.test) # Distribuci�n no normal p>

plotn <- function(x,main="Histograma de frecuencias y distribuci�n normal",
                  xlab="Area",ylab="Variabilidad") {
  min <- min(x)
  max <- max(x)
  media <- mean(x)
  dt <- sd(x)
  hist(x,freq=F,main=main,xlab=xlab,ylab=ylab)
  curve(dnorm(x,media,dt), min, max,add = T,col="blue")
} ## Grafico distribuci�n

plotn(Metazygia$area,main="Distribuci�n normal") # Graficamente distribuci�n tampoco es normal

# Prueba de homogeneidad de varianzas

bartlett.test(Metazygia$area ~ Metazygia$lugar)  # Las varianzas son homogeneas 
bartlett.test(Metazygia$area ~ Metazygia$luz)  # Las varianzas no son homogeneas
bartlett.test(Metazygia$area ~ Metazygia$posicion)  # Las varianzas son homogeneas

# Homocedasticidad

library(car) # Para homocedasticidad
leveneTest(Metazygia$area ~ Metazygia$lugar) # Las variaciones de los lugares no son equivalentes
leveneTest(Metazygia$area ~ Metazygia$posicion) # Las variaciones de las posiciones no son equivalentes

# No se pueden realizar pruebas param�tricas al incumplir supuestos de normalidad y varianzas.


sessionInfo()