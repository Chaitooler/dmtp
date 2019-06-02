library(jsonlite)
library(mongolite)
library(dplyr)
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


## OUTLIERS ###

preciosPorProducto = preciosclean %>%
  group_by(producto) %>%
  summarise(avg_precio = mean(precio, na.rm=TRUE), mediana = median(precio, na.rm=TRUE), var = sd(precio, na.rm = TRUE), minimo = min(precio, na.rm= TRUE), maximo = max(precio, na.rm = TRUE))
