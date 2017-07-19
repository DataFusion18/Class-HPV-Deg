#Cargar paquetes
library(raster)
library(rgdal)
library(rgeos)
library(dismo)

source("/Users/ccgss/Documents/R/R Proyecto J/Paso4/reclass_func.R")

y1<-"1993"
y2<-"2015"
cuad<-"5"

#1 Pa sacar la media automática
#0 pa sacar valor con el que le pones abajo
auto<-1
valor<-(-1500)

#Cambiar ruta de trabajo
ruta<-"/Users/ccgss/Documents/espa2015-2016/Class"
#dir.create(ruta)
setwd(ruta)

#Cargar imágenes

#!!Nuevos sitios de entrenamiento!!
#Imagen extract de los sitios de entrenamiento
list.files("~/Documents/espa2015-2016/cortes",pattern="*.tif$",full.names=T)
#extracto<-raster(paste0("cortes/","m",cuad,"_max",y2,"-",y1,".tif"))
extracto<-raster(paste0("~/Documents/espa2015-2016/cortes/extract_",cuad,"_",y2,"-",y1,".tif"))
#Imagen de corte
im1<-raster(paste0("~/Documents/espa2015-2016/cortes/",cuad,"_max",y2,"-",y1,".tif"))

#Cargar imagen de periodo inicial
mask_inicio<-raster(paste0("~/Documents/espa2015-2016/cortes/",cuad,"_max",y1,".tif"))
#Cargar imagen de periodo final 
mask_fin<-raster(paste0("~/Documents/espa2015-2016/cortes/",cuad,"_max",y2,".tif"))
#Cargar shape de ANP
#shape<-shapefile("/Users/ccgss/Documents/Proyecto J/shape")

#Cargar máscara de agua
mask_agua<-shapefile("/Users/ccgss/Documents/R/R Proyecto J/Paso4/BUFF_60_2")

num_ptos<-25 

reclass_func(im1,mask_inicio,mask_fin,mask_agua,extracto,num_ptos,auto,valor)


