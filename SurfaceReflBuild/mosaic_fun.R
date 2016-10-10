#mosaic_fun

#English
#This function makes annual mosaics of available images

#carpeta_entrada; Folder containing images to mosaic, images must be organized by folders corresponding to each year
#carpeta_salida; Output destination folder
#anios_num; vector containing the years for which mosaics are going to be constructed
#mosaic_fun; function used to construct the mosaic
#shape; shape containing the output mosaic extents (mosaic of only an area of interest)

#Español
#Esta función crea mosaicos anuales de imágenes 

#carpeta_entrada; Folder donde se encuentran las imágenes que se desean mosaiquear
#carpeta_salida; Folder de salida 
#anios_num; vector que contenga los años que se desean mosaiquear
#mosaic_fun; Función con la que se desea construir los mosaicos
#shape; Archivo shape que contenga el área de interés si es que existe

#Requires raster library
#library(raster)

mosaic_fun<-function(carpeta_entrada,carpeta_salida,anios_num,mosaic_fun=max,shape)
  {
    for(l in anios_num[1]:anios_num[length(anios_num)])
      {
        if(l %in% anios_num==T)
          {
            #Loads images
            imgs_ndvi<-list.files(file.path(carpeta_entrada,l),pattern="*ndvi.tif$", full.names=TRUE)
            imgs_rast_n<-lapply(imgs_ndvi,raster)
            
            #If shape exists
            if(missing(shape))
              {
                bounding.raster<-imgs_rast_n[[1]]
              }else{
                bounding.raster<-raster(extent(shape),crs=projection(imgs_rast_n[[1]]))
                res(bounding.raster)<-res(imgs_rast_n[[1]])
              }
            
            resampled.rasters_nM<-lapply(imgs_rast_n,function(x) {
              resample(x,bounding.raster,method="bilinear")
            })
            resampled.rasters_nM$fun <- mosaic_fun
            rast.mosaic_ndviM<-do.call(mosaic,resampled.rasters_nM)
            
            #Output
            writeRaster(rast.mosaic_ndviM,paste0(carpeta_salida,"/mosaic_ndvi_max",l,".tif"),format="GTiff",datatype="FLT4S",overwrite=T)
            print(paste0("Listo year ",l))
          }else{
            l<-l+1		
          }		
      }
  }
	