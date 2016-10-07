#Jonathan V. Solórzano
#2016

#ndvi_mask
#Masks clouds and other non-vegetated surfaces as water from surface reflectance images, processed and downloaded via ESPA (usgs) 

#Needs raster library
#library(raster)

ndvi_mask<-function(carpeta_entrada,carpeta_salida,shape,anios_num,thresh_cfmask=0,thresh_cloud_l=40,thresh_cloud_u=60,thresh_cfmaskconf=2,trim_u=10000,trim_l=-10000)
  {
    #create folders for each year analyzed
    mapply(dir.create,file.path(carpeta_salida,anios_num))
    
    #Lists de ndvi files from the path
    filenames<-list.files(file.path(carpeta_entrada),pattern="*_sr_ndvi.tif$", full.names=TRUE)
    
    #Create vector of images names, paths and years
    temp<-strsplit(filenames,"_sr_ndvi")
    corte_filenamesMaster<-unlist(unique(lapply(temp, function(x) x[[1]])))
    temp<-strsplit(corte_filenamesMaster,"/")
    id_imgMaster<-unlist(unique(lapply(temp, function(x) x[[length(temp[[1]])]])))
    years<-substr(id_imgMaster,10,13)
    
    #Cycle for every year being analyzed
    for(l in anios_num[1]:anios_num[length(anios_num)])
      {
      
        #Check if the following year is inside the actual years vector, if not just ignore and pass to next year
        if(l %in% anios_num==T)
          {
          
            
            files_year<-which(years==l)
            
            #If there is at least on valid image file
            if(length(files_year)>=1)
              {
              
                #Id image names
                corte_filenames<-corte_filenamesMaster[files_year]
                id_img<-id_imgMaster[files_year]
                
                #Cycle for masking every image
                for(i in 1:length(corte_filenames))
                  {
                  
                    #Obtain satellite id
                    sat<-substr(id_img[i],3,3)
                    
                    #Load ndvi raster
                    ndvi<-raster(paste0(corte_filenames[i],"_sr_ndvi.tif"))
                    ndvi<-crop(ndvi,shape)
                    
                    #Load masks
                    #cfmask
                    cfmask<-raster(paste0(corte_filenames[i],"_cfmask.tif"))	
                    cfmask<-crop(cfmask,extent(shape))
                    cfmask[cfmask>thresh_cfmask]<-NA
                    
                    #cloudmask
                    if(sat=="8")
                      {
                        cloudmask<-raster(paste0(corte_filenames[i],"_sr_cloud.tif"))
                        cloudmask <-crop(cloudmask,shape)
                        cloudmask[cloudmask>=thresh_cloud_l&cloudmask<=thresh_cloud_u]<-NA	
                      }else{
                        cloudmask<-raster(paste0(corte_filenames[i],"_cfmask_conf.tif"))
                        cloudmask <-crop(cloudmask,shape)
                        cloudmask[cloudmask>=thresh_cfmaskconf]<-NA	
                      }
                    
                    #Combine both masks
                    mask_final<-cfmask*cloudmask
                    mask_final[(mask_final!=NA)]<-1
                    
                    #NDVI
                    ndvi<-mask(ndvi,mask_final)
                    
                    #trim
                    ndvi[ndvi>trim_u]<-NA
                    ndvi[ndvi<(-trim_l)]<-NA				
                    
                    #Output	
                    writeRaster(ndvi,paste0(carpeta_salida,"/",l,"/",id_img[i],"_M_ndvi.tif"),format="GTiff",datatype="FLT4S",overwrite=T)
                    print(paste0("Listo ",id_img[i]," Img ",i," de ",length(id_img)))
                  }
              }else{
                corte_filenames<-vector()
                id_img<-vector()
              }
          }else{
            l<-l+1		
          }		
      }
  }


