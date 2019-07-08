install.packages("jsonlite")
library(jsonlite)
install.packages("mongolite")
library(mongolite)
install.packages("dplyr")
library(dplyr)
install.packages("scatterplot3d")
library(scatterplot3d)
install.packages("readr")
library(readr)

install.packages("Rlof")
library(Rlof)

sucursales = stream_in(file("C:/Users/chait/Desktop/Facultad/dmtp/data/sucursales.json",open="r"))
productos = stream_in(file("C:/Users/chait/Desktop/Facultad/dmtp/data/productos.json",open="r"))
precios = stream_in(file("C:/Users/chait/Desktop/Facultad/dmtp/data/precios.json",open="r"))

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
summary(medicion1)
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
medicion10 = preciosclean %>% 
  filter(medicion==10)
summary(medicion10)

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


### Pregunta 2 y 3 Marcas con mayor modificacion de precios

pregunta2 = inner_join(preciosPorProducto, productos, by=c("producto"="id"))

pregunta2joineada = pregunta2 %>%
  group_by(marca) %>%
  summarise(avg_var = median(var, na.rm=TRUE))

### Pregunta 4 Hipermercados vs supermercados

pregunta4 = inner_join(preciosclean, sucursales, by=c("sucursal"="id"))

pregunta4joineada = pregunta4 %>%
  group_by(sucursal, sucursalTipo) %>%
  filter(sucursalTipo %in% c("Supermercado", "Hipermercado")) %>%
  summarise(avg_precio = mean(precio, na.rm=FALSE), 
            var = sd(precio, na.rm=FALSE), 
            cantidadProd=n_distinct(producto),
            maximo = max(precio, na.rm = FALSE),
            minimo = min(precio, na.rm=FALSE))
  

pregunta4resumida = pregunta4joineada %>%
  group_by(sucursalTipo) %>%
  summarise(avg_mean = mean(avg_precio, na.rm=FALSE), avg_var = mean(var, na.rm=FALSE), promedioProductos=mean(cantidadProd, na.rm=FALSE))


### Pregunta 5 : En que per�odo se produjo el mayor incremento de precios?

pregunta5 = preciosclean %>%
  group_by(medicion) %>%
  summarise(avg_precio = mean(precio, na.rm=FALSE),
            max_precio=max(precio, na.rm=FALSE),
            min_precio=min(precio, na.rm=FALSE))
  

boxplot(preciosclean$precio ~ preciosclean$medicion)  


###Faltantes
preciossinfaltantes= preciosclean %>%
  filter()
  
  
  
### Pregunta 6:
summary(preciosclean$precio)

Q = preciosclean %>%
  filter(precio>42, precio<95.99)

O = preciosclean %>%
  filter(precio>200)
  
boxplot(Q$precio ~ Q$medicion)

boxplot(O$precio  ~  O$medicion)
  
  
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
scatterplot3d(preciosPorProductoTotal$avg_precio, preciosPorProductoTotal$var, preciosPorProductoTotal$presenciaEnSucursales, color = preciosPorProductoTotal$color,  pch=16, grid=TRUE, box=FALSE, type="h")

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
  summarize(avg_precio = mean(precio,na.rm=TRUE), var=sd(precio,na.rm=TRUE), cant_productos=n_distinct(producto))
scatterplot3d(productosPorSucursal$avg_precio, productosPorSucursal$var, productosPorSucursal$cant_productos)

##Vemos outliers con LOF por sucursal
productosPorSucursalClean = productosPorSucursal
productosPorSucursalClean$score<-lof(productosPorSucursalClean[ , -which(names(productosPorSucursal) %in% c("sucursal"))], k=3)
umbral<-4
productosPorSucursalClean$outlier <- (productosPorSucursalClean$score>umbral)
productosPorSucursalClean <- na.omit(productosPorSucursalClean)
productosPorSucursalClean$color <- ifelse(productosPorSucursalClean$outlier, "red", "black")
scatterplot3d(productosPorSucursalClean$avg_precio, productosPorSucursalClean$var, productosPorSucursalClean$cant_productos, color = productosPorSucursalClean$color, pch=16, grid=TRUE, box=FALSE, type="h")

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
scatterplot3d(preciosPorMedicion$avg_precio, preciosPorMedicion$var, preciosPorMedicion$cant_productos, color = preciosPorMedicion$color, pch=16, grid=TRUE, box=FALSE, type="h")

##Vemos los rojos:
outliersPorMedicion = preciosPorMedicion %>%
  filter(color=="red")


boxplot(preciosclean$precio)

### OUTLIERS SEGUN TODO
preciosparaoutliers = preciosclean %>%
  mutate_if(sapply(preciosclean, is.character), as.factor)
preciosparaoutliers = preciosparaoutliers[ , -which(names(preciosparaoutliers) %in% c("fecha"))]

preciosparaoutliers$score<-lof(preciosparaoutliers, k=3)
umbral <- 4
preciosparaoutliers$outlier <- preciosparaoutliers$score > umbral
preciosparaoutliers <- na.omit(preciosparaoutliers)
preciosparaoutliers$color <- ifelse(preciosparaoutliers$outlier, "red", "black")
scatterplot3d(preciosparaoutliers$producto, preciosparaoutliers$sucursal, preciosparaoutliers$medicion)

summary(preciosparaoutliers)
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

#md.pattern(preciosconfaltantes)
#imputed_Data <- mice(preciosconfaltantes,m=5,maxit=3,method='pmm',seed=500)

## Prueba que fecha y medicion aportan misma info
plot(preciosconfaltantes$fecha, preciosconfaltantes$medicion, xlab = "fecha", ylab = "medicion")


### PREGUNTAS

## Productos que sufrieron mayores y menores variaciones en el tiempo.













#########################################
## TP2
#########################################

install.packages('tidyr')
library(tidyr)
preciosHorizontal =  preciosclean %>%
  select(-fecha) %>%
  spread(medicion, precio) 



#### Faltantes ALGORITMO PLANO


completarFaltantes <- function(dataframe) {
  cols = 3:12
  print(cols)
  modify = 0
  for (i in 1:nrow(dataframe)) {
    row <- dataframe[i,]
    
    for(z in 1:10) {
      for (j in 3:12) {
        if (is.na(row[,j])) {
          if (((j-1) %in% cols) && !is.na(row[,j-1])) {
            dataframe[i,j] <- row[,j-1]
            row = dataframe[i,]
          } else if (((j+1) %in% cols) && !is.na(row[,j+1])) {
            dataframe[i,j] <- row[,j+1]
            row = dataframe[i,]
          }
        }
      }
    }
  }
  return (dataframe)
}

###TEst

headph = head(preciosHorizontal, n=100)
headphback = headph

headph <- completarFaltantes(headph)

print(modify)

#### Precios por periodo

preciosHorizontalSinFaltantes <- completarFaltantes(preciosHorizontal)


### Columnas por periodos

agregarColumnasPeriodos <- function(dataframe){
  for ( i in 1:nrow(dataframe)) {
    row <- dataframe[i,]
    
    dataframe[i,"p1"] <- (row$`1` + row$`2` + row$`3`)/3
    dataframe[i,"p2"] <- (row$`4` + row$`5` )/2
    dataframe[i,"p3"] <- (row$`6` + row$`7` )/2
    dataframe[i,"p4"] <- (row$`8` + row$`9` + row$`10`)/3
    dataframe[i,"precioPromedio"] <- (row$`1` + row$`2` + row$`3` + row$`4` + row$`5` + row$`6` + row$`7` + row$`8` + row$`9` + row$`9`)/10
  }
  
  return (dataframe)
}

## Test

headph = head(preciosHorizontalSinFaltantes, n=100)
headphback = headph

headph <- agregarColumnasPeriodos(headph)

### Variaciones

agregarVariaciones <- function(dataframe){
  for ( i in 1:nrow(dataframe)) {
    row <- dataframe[i,]
    
    dataframe[i,"v1"] <- (row$`p2`-row$`p1` ) / row$`p1`
    dataframe[i,"v2"] <- (row$`p3`-row$`p2` ) / row$`p2`
    dataframe[i,"v3"] <- (row$`p4`-row$`p3` ) / row$`p3`
    dataframe[i,"variacionTotal"] <- (row$`p4`-row$`p1` )/row$`1`
  }
  
  return (dataframe)
}


### Test
headph = head(preciosHorizontalSinFaltantes, n=100)
headphback = headph
headph<- agregarColumnasPeriodos(headph)
headph <- agregarVariaciones(headph)