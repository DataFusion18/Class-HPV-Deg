#resta_raster

#English
#Makes a difference raster between i+1 and i layers of a raster brick


#brick_max; brick of annual mosaics
#ruta.salida; output folder
#MASK; optional argument indicating a mask layer

#Español
#Hace una diferencia entre las capas i+1 e i de un raster brick

#brick_max; brick of de los mosaicos anuales
#ruta.salida; folder donde se van a guardar los resultados
#MASK; argumento opcional que indica si se provee una máscara


resta_raster<-function(brick_max,ruta.salida,MASK)
{
	for(i in 1:(nlayers(brick_max)-1))
	{
		#Makes difference between mosaics
	  resul<-brick_max[[(i+1)]]-brick_max[[i]]
	  
	  #Apply mask
	  if(missing("MASK"))
  	  {
  	  	print("no mask suppplied")
	    }else{
	      resul<-mask(resul,MASK)
  	  }
	  
	  #Difference output
	  writeRaster(resul,paste0(ruta.salida,"/",brick_max@data@names[(i+1)],"-",brick_max@data@names[i]),format="GTiff",datatype="FLT4S",overwrite=T)
	  print(paste0("Resta hecha para",brick_max@data@names[(i+1)],"-",brick_max@data@names[i]))
	}
}
