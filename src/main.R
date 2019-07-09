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
    

      for (j in 3:12) {
        if (is.na(row[,j])) {
          if (((j-1) %in% cols) && !is.na(row[,j-1]) && ((j+1) %in% cols) && !is.na(row[,j+1])) {
            dataframe[i,j] <- (row[,j-1]+row[,j+1])/2 
            row = dataframe[i,]
          } else if (!((j-1) %in% cols)) {
            dataframe[i,j] <- row[,j+1]
            row = dataframe[i,]
          } else if (!((j+1) %in% cols)) {
            dataframe[i,j] <- row[,j-1]
            row = dataframe[i,]
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

###Prod
work = preciosHorizontal
work = completarFaltantes(work)

### Eliminar faltantes
library(tidyr)
eliminarFaltantes <- function(dataframe) {
  return (dataframe %>% drop_na() ) 
}

### Test
headph = head(preciosHorizontal, n=100)
headphback = headph

headph <- completarFaltantes(headph)
headph <- eliminarFaltantes(headph)

#Prod
work = eliminarFaltantes(work)

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

###Prod
work = agregarColumnasPeriodos(work)

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

###Prod
work = agregarVariaciones(work)

### Discretizacion

discretizacionVariacion <- function(variacion) {
  if (variacion < -0.05) {
    return ("Disminucion Fuerte")
  }
  if (variacion < -0.02) {
    return ("Disminucion Media")
  }
  if (variacion < -0.005) {
    return ("Disminucion Leve")
  }
  if (variacion < 0.005) {
    return ("Mantiene")
  }
  if (variacion < 0.02) {
    return ("Aumento Leve")
  }
  if (variacion < 0.05) {
    return ("Aumento Medio")
  }
  
  return ("Aumento Fuerte")
}



### Test
headph = head(preciosHorizontalSinFaltantes, n=100)
headphback = headph
headph<- agregarColumnasPeriodos(headph)
headph <- agregarVariaciones(headph)
headph <- discretizacionDeVariaciones(headph)

###Prod
work <- discretizacionDeVariaciones(work)

#### Agrupaciones por producto
library(dplyr)
mediasPorProducto <- function(dataframe) {
  dataframe = dataframe %>% 
    group_by(producto) %>%
    summarise(avg_p1 = mean(p1, na.rm=TRUE), 
              avg_p2 = mean(p2, na.rm=TRUE), 
              avg_p3 = mean(p3, na.rm=TRUE), 
              avg_p4 = mean(p4, na.rm=TRUE), 
              avg_pt = mean(precioPromedio, na.rm=TRUE))
  
  return (dataframe)
}

### Test
headph = head(preciosHorizontalSinFaltantes, n=100)
headphback = headph
headph<- agregarColumnasPeriodos(headph)
avgPorProducto <- mediasPorProducto(headph)

### Prod
avgPorProducto = mediasPorProducto(work)


### Precios relativos
preciosRelativoProducto <- function (producto, avgProducto) {

  return (avgProducto [which(avgProducto$producto == producto),])
}


preciosRelativos <- function(dataframe, avgPorProducto) {
  for ( i in 1:nrow(dataframe)) {
    row <- dataframe[i,]
    precioRelativoProducto = preciosRelativoProducto(row$producto, avgPorProducto)

    dataframe[i,"pr1"] <- (row$p1 - precioRelativoProducto$`avg_p1`) / precioRelativoProducto$`avg_p1`
    dataframe[i,"pr2"] <- (row$p2 - precioRelativoProducto$`avg_p2`) / precioRelativoProducto$`avg_p2`
    dataframe[i,"pr3"] <- (row$p3 - precioRelativoProducto$`avg_p3`) / precioRelativoProducto$`avg_p3`
    dataframe[i,"pr4"] <- (row$p4 - precioRelativoProducto$`avg_p4`) / precioRelativoProducto$`avg_p4`
    dataframe[i,"prt"] <- (row$precioPromedio - precioRelativoProducto$`avg_pt`) / precioRelativoProducto$`avg_pt`
  }
  
  return (dataframe)
  
}
### TEst
headph = head(preciosHorizontalSinFaltantes, n=100)
headphback = headph
headph<- agregarColumnasPeriodos(headph)
avgPorProducto <- mediasPorProducto(headph)
headph <- preciosRelativos (headph, avgPorProducto)

###Prod
work = preciosRelativos(work, avgPorProducto)

###DiscretizacionPrecios
discretizacionPrecio <- function(precio) {
  if (precio < -0.1) {
    return ("Muy barato")
  }
  if (precio < -0.05) {
    return ("Medianamente barato")
  }
  if (precio < -0.01) {
    return ("Levemente barato")
  }
  if (precio < 0.01) {
    return ("Medio")
  }
  if (precio < 0.05) {
    return ("Levemente caro")
  }
  if (precio < 0.1) {
    return ("Medianamente caro")
  }
  
  return ("Muy caro")
}

discretizacionDePrecios <- function(dataframe) {
  
  for ( i in 1:nrow(dataframe)) {
    row <- dataframe[i,]
    
    dataframe[i,"pr1d"] <- discretizacionPrecio( row$`pr1`)
    dataframe[i,"pr2d"] <- discretizacionPrecio( row$`pr2`)
    dataframe[i,"pr3d"] <- discretizacionPrecio( row$`pr3`)
    dataframe[i,"pr4d"] <- discretizacionPrecio( row$`pr4`)
    dataframe[i,"prtd"] <- discretizacionPrecio( row$`prt`)
}
  
  return (dataframe)
}

### Test

headph = head(preciosHorizontalSinFaltantes, n=100)
headphback = headph
headph<- agregarColumnasPeriodos(headph)
avgPorProducto <- mediasPorProducto(headph)
headph <- preciosRelativos (headph, avgPorProducto)
headph <- discretizacionDePrecios(headph)

### Prod
work = discretizacionDePrecios(work)
workbackup = work
### Coordenadas geograficas

install.packages("sf")
install.packages("tidyverse")
library(sf)
library(tidyverse)
radios <- st_read("https://bitsandbricks.github.io/data/CABA_rc.geojson")

ggplot() + geom_sf(data=radios)
ggplot() + geom_sf(data = radios, aes(fill = BARRIO), color = NA)
plot(radios["BARRIO"], border = NA)


summary(radios)
View(radios)

exampleLat = -34.55212
exampleLong = -58.49841

exampleCord = c(exampleLat, exampleLong)

inter <- radios %>%
  filter(st_intersection(geometry, exampleCord))


geom = st_geometry(radios)


sucursalLatLong = sucursalesclean %>% select(id, lat, lng)
write.csv(sucursalLatLong, "C:/Users/chait/Desktop/Facultad/dmtp/data/sucursaleslatlong.csv
          ")



####https://api.opencagedata.com/geocode/v1/json?key=6a672e7c63824ffea1bd8d2d3e2f5b6b&q=-34.55212%2C+-58.49841&pretty=1&no_annotations=1  



barrios = stream_in(file("C:/Users/chait/Desktop/Facultad/dmtp/data/barrios.json",open="r"))


### Terminos de productos

productostextos = productos %>% select(id, nombre, presentacion, marca)

### TOlower
productostextos$nombre = tolower(productostextos$nombre)
productostextos$marca = tolower(productostextos$marca)
productostextos$presentacion = tolower(productostextos$presentacion)

## REmove invalids
install.packages('tm')
library(tm)

productostextos$nombre = removeNumbers(productostextos$nombre)
productostextos$marca = removeNumbers(productostextos$marca)
productostextos$presentacion = removeNumbers(productostextos$presentacion)

productostextos$nombre = removePunctuation(productostextos$nombre)
productostextos$marca = removePunctuation(productostextos$marca)
productostextos$presentacion = removePunctuation(productostextos$presentacion)

## Acentos

install.packages('stringi')
library(stringi)

productostextos$nombre = stri_trans_general(productostextos$nombre, "Latin-ASCII")
productostextos$marca = stri_trans_general(productostextos$marca, "Latin-ASCII")
productostextos$presentacion = stri_trans_general(productostextos$presentacion, "Latin-ASCII")


productostextos$nombre = stripWhitespace(productostextos$nombre)
productostextos$marca = stripWhitespace(productostextos$marca)
productostextos$presentacion = stripWhitespace(productostextos$presentacion)

unidadesUnique = unique(productostextos$presentacion)
unidadesUnique = stripWhitespace(unidadesUnique)

marcasUnique = unique(productostextos$marca)

productostextos$nombre = removeWords(productostextos$nombre, unidadesUnique)
productostextos$nombre = removeWords(productostextos$nombre, marcasUnique)


stopw = stopwords(kind='sp')

productostextos$nombre = removeWords(productostextos$nombre, stopw)
productostextos$nombre = stripWhitespace(productostextos$nombre)

write.csv(productostextos, "C:/Users/chait/Desktop/Facultad/dmtp/data/previoprocesamientoreglas.csv")

##################

### Freq terms
install.packages("tm")
library(tm)

install.packages('arules')
library(arules)

voctable = data.frame(sentence = productostextos$nombre)
myCorpus = Corpus(VectorSource(voctable$sentence))
tdm = TermDocumentMatrix(myCorpus)

##Elegimos frecuencia de 20
inspect(tdm)
vocabulariotm = findFreqTerms(x = tdm, lowfreq = 20, highfreq = Inf)
View(vocabulariotm)
print(vocabulariotm)
library(stringr)
vocabularioVsNombre = productostextos

for ( i in 1:nrow(vocabularioVsNombre)) {
  row = vocabularioVsNombre[i,]
  
  for (j in vocabulariotm) {
    if (str_detect(row$nombre, j) ) {
      print(row$nombre)
      print(j)
      vocabularioVsNombre[i,j] <- TRUE
    } else {
      vocabularioVsNombre[i,j] <- NA
    }
      
  }
}


### A PRIORI
install.packages("apriori")
library(apriori)


#### Sobre productos
rules = apriori(vocabularioVsNombre[,5:36], parameter = list(support=0.01, confidence=0.01, target='rules'))
print(rules)
inspect(rules)

inspect(sort(rules, by="lift", decreasing = TRUE))


#### Sobre sucursales
workgigante = merge(work, vocabularioVsNombre, by.x='producto', by.y='id')
workgigante = merge(workgigante, barrios, by.x='sucursal', by.y='id')


datasetreglas = workgigante %>% select(
  v1d, v2d, v3d, variacionTotalDiscreta,
  pr1d, pr2d, pr3d, pr4d, prtd,
  presentacion, marca, 
  chocolate, desodorante, galletitas, crema, dulce, leche, pack, aerosol, blanco, agua, jabon, liquido, doypack, limon, cafe, manzana, polvo, jugo, naranja, vainilla, light, frutilla, queso, yogur, fideos, mate, vino, tinto, malbec, gas, saborizada, gaseosa,
  barrio)

datasetreglasback = datasetreglas

rules = apriori(datasetreglas, parameter = list(support=0.1, confidence=0.5, target='rules'))
print(rules)
inspect(sort(rules, by="lift", decreasing = TRUE))


### PRoductos muy caros
productosMuyCaros = datasetreglas %>% filter(prtd=='Muy caro') %>% select(-prtd, -pr2d, -pr1d, -pr3d, -pr4d)
reglasmuycaros = apriori(productosMuyCaros, parameter = list(support=0.1, confidence=0.5, target='rules'))
inspect(sort(reglasmuycaros, by="lift", decreasing = TRUE))

### PRoductos muy baratos
productosMuyBaratos = datasetreglas %>% filter(prtd=='Muy barato') %>% select(-prtd, -pr2d, -pr1d, -pr3d, -pr4d)
reglasmuybaratos = apriori(productosMuyBaratos, parameter = list(support=0.1, confidence=0.4, target='rules'))
inspect(sort(reglasmuybaratos, by="lift", decreasing = TRUE))


###Reglas por barrios
productosDeRecoleta = datasetreglas %>% filter(barrio=='Recoleta') %>% select(-barrio, -pr2d, -pr3d, -pr1d, -pr4d, -v1d, -v2d, -v3d) ##select(-prtd, -pr2d, -pr1d, -pr3d, -pr4d)
reglasrecoleta = apriori(productosDeRecoleta, parameter = list(support=0.05, confidence=0.4, target='rules'))
inspect(sort(reglasrecoleta, by="lift", decreasing = TRUE))

productosDeLugano = datasetreglas %>% filter(barrio=='Villa Lugano') %>% select(-barrio, -pr2d, -pr3d, -pr1d, -pr4d, -v1d, -v2d, -v3d)
reglaslugano = apriori(productosDeLugano, parameter = list(support=0.1, confidence=0.5, target='rules'))
inspect(sort(reglaslugano, by="lift", decreasing = TRUE))


#### REfinando el dataset

datasetreglas = datasetreglas %>% select(-tinto, -malbec, -blanco, -liquido, -doypack, -gas, -polvo)


### Desaceleracion ultimo periodo
datasetultimoperiodo = datasetreglas %>% filter(v3d %in% c('Mantiene', 'Disminucion leve', 'Disminucion fuerte', 'Disminucion media'))
datasetultimoperiodo = datasetreglas
reglasultimo = apriori(datasetultimoperiodo, parameter = list(support=0.05, confidence=0.5, target='rules'))


rules_subset <- subset(reglasultimo, (rhs %in% paste0("v3d=", unique(datasetultimoperiodo$v3d))))
inspect(rules_subset)


inspect(sort(reglasultimo, by="lift", decreasing = TRUE))


### Galletitas
datasetgalletitas = datasetreglas %>% filter(galletitas == TRUE) %>% select(-galletitas, -presentacion)
reglasgalletitas = apriori(datasetgalletitas, parameter = list(support=0.2, confidence=0.5, target='rules'))
inspect(sort(reglasgalletitas, by="lift", decreasing = TRUE))


#### ANalisis predictivo
rules = apriori(datasetreglas, parameter = list(support=0.1, confidence=0.5, target='rules'))
print(rules)
inspect(sort(rules, by="lift", decreasing = TRUE))

rules_subset <- subset(rules, ((rhs %in% paste0("pr1d=", unique(datasetultimoperiodo$pr1d)))))
inspect(rules_subset)


datasetsinultimo = datasetreglas %>% select(-pr4d, -v3d, -variacionTotalDiscreta, -prtd)
rulessinultimo = apriori(datasetsinultimo, parameter = list(support=0.05, confidence=0.5, target='rules'))
inspect(sort(rulessinultimo, by="lift", decreasing = TRUE))


datasetmetricasultimo = datasetreglas %>% filter(pr3d=='Levemente barato',presentacion==' gr')
library(ggplot2)
df = datasetmetricasultimo %>% group_by(v3d) %>% summarise(counts=n())
ggplot(df, aes(x = v3d, y=counts)) +
  geom_bar(fill="#0073C2FF", stat='identity') +
  geom_text(aes(label=counts), vjust = -0.3)


datasetmetricasultimo = datasetreglas %>% filter(v1d=='Mantiene',pr2d=='Medio', pr3d=='Medio')
library(ggplot2)
df = datasetmetricasultimo %>% group_by(v3d) %>% summarise(counts=n())
ggplot(df, aes(x = v3d, y=counts)) +
  geom_bar(fill="#0073C2FF", stat='identity') +
  geom_text(aes(label=counts), vjust = -0.3)

datasetmetricasultimo = datasetreglas %>% filter(v1d=='Mantiene',pr1d=='Levemente caro')
library(ggplot2)
df = datasetmetricasultimo %>% group_by(v3d) %>% summarise(counts=n())
ggplot(df, aes(x = v3d, y=counts)) +
  geom_bar(fill="#0073C2FF", stat='identity') +
  geom_text(aes(label=counts), vjust = -0.3)
     
     