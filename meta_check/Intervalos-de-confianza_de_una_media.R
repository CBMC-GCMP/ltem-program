###CALCULAR EL INTERVALO DE CONFIANZA DE UNA MEDIA###
###La formula es media +/- z (error estándar)###
### Error estándar:
### desviación/raiz cuadrada de n

##calculamos la media

mean(Size/Quantity)
media <- mean("Size")

### ID puntución crítica 95%
z <- 1.96 


### ID número de casos
n <- length("Size")

##desviación estándar

sd("Size")
desviacion <- sd("Size")

## error estándar
errorst <- desviacion/sqrt(n)

## calcular límite inferior

lim_inf <- media - (z*errorst)

##Límite superior
 lim_sup <- media + (z*errorst)
 
 ##listado de valores
 interval_m <- data.frame(n, media, desviacion, z, errorst, lim_inf)
 
 