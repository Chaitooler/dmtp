library(jsonlite)
library(mongolite)
library(dplyr)
install.packages("scatterplot3d")
library(scatterplot3d)
library(readr)

install.packages("Rlof")
library(Rlof)

sucursales = stream_in(file("sucursales.json",open="r"))
productos = stream_in(file("productos.json",open="r"))
precios = stream_in(file("C:/Users/cmarcusa/Desktop/Maestria/DM/dmtp/data/precios.json",open="r"))

length(precios)

summary(sucursales)
summary(precios)
summary(productos)


######## SUCURSALES ########
names(sucursales)
View(sucursales)
pie(table(sucursales$localidad))
##Esto arroja que hay muchos datos amorfos para capital federal

sucursalesclean = sucursales[ , -which(names(sucursales) %in% c("provincia","banderaDescripcion", "localidad", "direccion", "sucursalNombre"))]

pie(table(sucursalesclean$comercioRazonSocial))
##Esto arroja el grafico de tortas de las racones sociales.... DIA e INC.SA los mayores

pie(table(sucursalesclean$sucursalTipo))
##Esto arroja que casi un 80% son autoservicios.


######## PRECIOS ##############
View(precios)
pie(table(precios$medicion))

View(productos)
pie(table(productos$marca))

head(precios)
str(precios)

preciosclean = precios[ , -which(names(precios) %in% c("_id"))]

preciostb <- tbl_df (preciosclean)

## MEDICIONES ##
medicion1 = preciosclean %>% 
  filter(medicion==1)
medicion2 = preciosclean %>% 
  filter(medicion==2)
medicion3 = preciosclean %>% 
  filter(medicion==3)
medicion4 = preciosclean %>% 
  filter(medicion==4)
medicion5 = preciosclean %>% 
  filter(medicion==5)
medicion6 = preciosclean %>% 
  filter(medicion==6)
medicion7 = preciosclean %>% 
  filter(medicion==7)
medicion8 = preciosclean %>% 
  filter(medicion==8)
medicion9 = preciosclean %>% 
  filter(medicion==9)

preciosPorProductoPorSucursal = preciosclean %>%
  group_by(producto, sucursal) %>%
  summarise(avg_precio = mean(precio, na.rm=TRUE), var = sd(precio, na.rm = TRUE), mediciones = n())
## Aca vemos que hay mediciones faltantes para algunos productos en algunas sucursales

## DATOS FALTANTES ##

preciosPorProductoPorSucursalFaltantes = preciosclean %>%
  group_by(producto, sucursal) %>%
  summarise(avg_precio = mean(precio, na.rm=TRUE), var = sd(precio, na.rm = TRUE), mediciones = n()) %>%
  filter(mediciones<10)

preciosPorProductoPorSucursalNOFaltantes = preciosclean %>%
  group_by(producto, sucursal) %>%
  summarise(avg_precio = mean(precio, na.rm=TRUE), var = sd(precio, na.rm = TRUE), mediciones = n()) %>%
  filter(mediciones==10)

#vemos que hay muchso datos faltantes. Se verá a futuro si hacen falta en el análisis o no.

## OUTLIERS ###

preciosPorProducto = preciosclean %>%
  group_by(producto) %>%
  summarise(avg_precio = mean(precio, na.rm=TRUE), mediana = median(precio, na.rm=TRUE), var = sd(precio, na.rm = TRUE), minimo = min(precio, na.rm= TRUE), maximo = max(precio, na.rm = TRUE))

## Pregunta 1 Media de precio vs Varianza.
boxplot(preciosPorProducto$avg_precio)
plot(sort(preciosPorProducto$avg_precio, decreasing = FALSE), preciosPorProducto$var)
scatterplot3d(preciosPorProducto$producto, preciosPorProducto$avg_precio, preciosPorProducto$var)

pregunta1 = preciosPorProducto %>%
  filter(preciosPorProducto$var>40)

joined= inner_join(pregunta1, productos, by=c("producto" = "id"))


### Pregunta 2 Marcas con mayor modificacion de precios

pregunta2 = inner_join(preciosPorProducto, productos, by=c("producto"="id"))

pregunta2joineada = pregunta2 %>%
  group_by(marca) %>%
  summarise(avg_var = median(var, na.rm=TRUE))
  

### Agrupaciones por producto

preciosPorProductoTotal = preciosclean %>%
  group_by(producto) %>%
  summarise(avg_precio = mean(precio, na.rm=TRUE), var = sd(precio, na.rm = TRUE), presenciaEnSucursales = n_distinct(sucursal))
## OUTLIERS PRODUCTOS:

preciosPorProductoTotal$score<-lof(preciosPorProductoTotal[ , -which(names(preciosPorProductoTotal) %in% c("producto"))], k=3)
umbral<-4
preciosPorProductoTotal$outlier <- (preciosPorProductoTotal$score>umbral)
preciosPorProductoTotal <- na.omit(preciosPorProductoTotal)
preciosPorProductoTotal$color <- ifelse(preciosPorProductoTotal$outlier, "red", "black")
scatterplot3d(preciosPorProductoTotal$avg_precio, preciosPorProductoTotal$var, preciosPorProductoTotal$presenciaEnSucursales, color = preciosPorProductoTotal$color)

##Vemos los rojos:
outliersPorProducto = preciosPorProductoTotal %>%
  filter(color=="red")

##SUCURSALES QUE MAS VARIARON LOS PRECIOS ABSURDAMENTE

### Agrupaciones por sucursal

preciosPorSucursal = preciosclean %>%
  group_by(sucursal, producto) %>%
  summarise(avg_precio = mean(precio, na.rm=TRUE), mediana = median(precio, na.rm=TRUE), var = sd(precio, na.rm = TRUE), minimo = min(precio, na.rm= TRUE), maximo = max(precio, na.rm = TRUE))

scatterplot3d(as.factor(preciosPorSucursal$sucursal), preciosPorSucursal$avg_precio, preciosPorSucursal$var)


productosPorSucursal = preciosclean %>%
  group_by(sucursal) %>%
  summarize(avg_precio = mean(precio,na.rm=TRUE), var=sd(precio,na.rm=TRUE), cant_productos=n())
scatterplot3d(productosPorSucursal$avg_precio, productosPorSucursal$var, productosPorSucursal$cant_productos)

##Vemos outliers con LOF por sucursal
productosPorSucursalClean = productosPorSucursal[ , -which(names(productosPorSucursal) %in% c("sucursal"))]
productosPorSucursalClean$score<-lof(productosPorSucursalClean, k=3)
umbral<-4
productosPorSucursalClean$outlier <- (productosPorSucursalClean$score>umbral)
productosPorSucursalClean <- na.omit(productosPorSucursalClean)
productosPorSucursalClean$color <- ifelse(productosPorSucursalClean$outlier, "red", "black")
scatterplot3d(productosPorSucursalClean$avg_precio, productosPorSucursalClean$var, productosPorSucursalClean$cant_productos, color = productosPorSucursalClean$color)

##Vemos los rojos:
outliersPorSucursal = productosPorSucursalClean %>%
  filter(color=="red")

##SUCURSALES QUE MAS VARIARON LOS PRECIOS ABSURDAMENTE


## Agrupaciones por medicion

preciosPorMedicion = preciosclean %>%
  group_by(medicion) %>%
  summarise(avg_precio = mean(precio, na.rm=TRUE), mediana = median(precio, na.rm=TRUE), var = sd(precio, na.rm = TRUE), minimo = min(precio, na.rm= TRUE), maximo = max(precio, na.rm = TRUE))

scatterplot3d(preciosPorMedicion$medicion, preciosPorMedicion$avg_precio, preciosPorMedicion$var)


preciosPorMedicion = preciosclean %>%
  group_by(medicion) %>%
  summarize(avg_precio = mean(precio,na.rm=TRUE), var=sd(precio,na.rm=TRUE), cant_mediciones=n())
scatterplot3d(preciosPorMedicion$avg_precio, preciosPorMedicion$var, preciosPorMedicion$cant_mediciones)

##Vemos outliers con LOF por medicion
preciosPorMedicion$score<-lof(preciosPorMedicion[ , -which(names(productosPorSucursal) %in% c("sucursal"))], k=3)
umbral<-4
preciosPorMedicion$outlier <- (preciosPorMedicion$score>umbral)
preciosPorMedicion <- na.omit(preciosPorMedicion)
preciosPorMedicion$color <- ifelse(preciosPorMedicion$outlier, "red", "black")
scatterplot3d(preciosPorMedicion$avg_precio, preciosPorMedicion$var, preciosPorMedicion$cant_productos, color = preciosPorMedicion$color)

##Vemos los rojos:
outliersPorMedicion = preciosPorMedicion %>%
  filter(color=="red")


############# ANALISIS #################
## plots de precios
boxplot(preciosclean$precio ~ preciosclean$medicion)


## plots de precios por producto
boxplot(preciosPorProducto)


########## VALORES FALTANTES Y REGRESION
uniquevalues <- expand.grid(producto = unique(preciosclean$producto),
                            sucursal = unique(preciosclean$sucursal),
                            medicion = unique(preciosclean$medicion))

preciosconfaltantes <- merge(preciosclean, uniquevalues, all = TRUE)

preciosconfaltantes <- preciosconfaltantes[ , -which(names(precios) %in% c("fecha"))]

### Probamos MICE

install.packages("mice")
library(mice)

md.pattern(preciosconfaltantes)
imputed_Data <- mice(preciosconfaltantes,m=5,maxit=3,method='pmm',seed=500)

## Prueba que fecha y medicion aportan misma info
plot(preciosconfaltantes$fecha, preciosconfaltantes$medicion, xlab = "fecha", ylab = "medicion")


### PREGUNTAS

## Productos que sufrieron mayores y menores variaciones en el tiempo.
