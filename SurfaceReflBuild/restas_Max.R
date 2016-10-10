library(raster)
library(rgdal)

source("/Users/ccgss/Desktop/R Proyecto J/Paso3/resta_raster.R")

ruta<-"/Users/ccgss/Desktop/R Proyecto J/Mask/mosaic"
ruta.salida<-"/Users/ccgss/Desktop/R Proyecto J/Mask/resta"

dir.create(file.path(ruta.salida), showWarnings = FALSE)
shape<-shapefile("/Users/ccgss/Desktop/R Proyecto J/Paso2/corte")
setwd(ruta)

#Esta como tiene diferente extensión hay que remuestrearla
#im79<-raster("/Volumes/CCGSS_3/Max_Chidos/Max_Lacandona_1979.tif")

#Jalar las imagenes de mascara de agua y localidades
mask_agua<-raster(paste0("/Users/ccgss/Desktop/R Proyecto J/Paso3/Mask_Cuerpo Agua1.tif"))
mask_localidades<-raster(paste0("/Users/ccgss/Desktop/R Proyecto J/Paso3/Mask_Localidades1.tif"))

lista<-list.files(file.path(paste0(getwd())),pattern="*.tif$",full.names=TRUE)
#Por si quieren esoger algunas imagenes en particular y no todas
#lista<-lista[c(5,20)]
lista

#Carga las imagenes como una lista de rasters
raster_lista<-lapply(lista,raster)

#Pa cortar todaslas imagenes a la extension más chiquita de los mosaicos
exte<-lapply(raster_lista,extent)
xmin<-max(unlist(lapply(exte,function(x) x[1])))
xmax<-min(unlist(lapply(exte,function(x) x[2])))
ymin<-max(unlist(lapply(exte,function(x) x[3])))
ymax<-min(unlist(lapply(exte,function(x) x[4])))
raster_lista2<-lapply(raster_lista,function(x) crop(x,extent(c(xmin,xmax,ymin,ymax))))

#final<-raster(raster_lista[[1]])

#Hace un brick de las imagenes
#!!!!Revisar que la imagen vieja vaya primero que la nueva
brick_max<-brick(raster_lista2)

#Correrlo si sale un error de different extents

#cortadas<-vector("list",length=length(raster_lista))
#for(i in 1:length(raster_lista))
#{
	#cortadas[[i]]<-resample(raster_lista[[i]],raster_lista[[1]],method="bilinear")
	#print(paste("Esperate pinche Mati Va en la",i,"de",length(raster_lista)))
#}
#brick_max<-brick(cortadas)

#Crea y escribe la resta
resta_raster(brick_max,mask_agua,mask_localidades,ruta.salida)


