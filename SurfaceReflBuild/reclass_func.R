reclass_func<-function(im1,mask_inicio,mask_fin,mask_agua,extracto,num_ptos=25,auto,valor)
{
	#Valor de corte
	if(auto==1)
	{
		corte_val<-cellStats(extracto,mean,na.rm=T)
	}else{
			corte_val<-valor
		}
		
	#mean_val<-cellStats(extracto,mean,na.rm=T)
	#sd_val<-cellStats(extracto,sd,na.rm=T)
	#min_val<-cellStats(extracto,min,na.rm=T)
	#max_val<-cellStats(extracto,max,na.rm=T)	
	
	#Sacar los años que se están sacando
	year1<-unlist(lapply(strsplit(names(mask_inicio),"[^[:digit:]]"),function (x) x[length(unlist(strsplit(names(mask_inicio),"[^[:digit:]]")))]))
	year2<-unlist(lapply(strsplit(names(mask_fin),"[^[:digit:]]"),function (x) x[length(unlist(strsplit(names(mask_fin),"[^[:digit:]]")))]))

	#Pa meter otras máscaras
	#mask_ruido10_11<-shapefile("/Users/ccgss/Documents/Proyecto J/Restas mias/MasK_ruido")
	#crs(mask_ruido10_11)<-crs(im1)
	#mask_loc<- raster("/Users/ccgss/Documents/Proyecto J/Mask/Mask_Localidades1.tif")
	#mask_ruido10_11<-rasterize(mask_ruido10_11,im1)
	#reclass_M<-reclassify(mask_ruido10_11,matrix(c(0.9,NA,1.1,NA,NA,1),nrow=2,ncol=3))

	#Reclasificar la máscara de agua (invertirla)
	mask_agua<-rasterize(mask_agua,im1)
	reclass_A<-reclassify(mask_agua,matrix(c(0.9,NA,1.1,NA,NA,1),nrow=2,ncol=3))
	
	#Crea la máscara inical en función de la media de la imagen
	mask_inicio<-crop(mask_inicio,im1)
	mask_inicio<-resample(mask_inicio,im1)
	val_mask_inicio<-cellStats(mask_inicio,mean,na.rm=T)
	mask_inicio[mask_inicio<val_mask_inicio]<-NA
	mask_inicio[mask_inicio>=val_mask_inicio]<-1
	#Escribe la máscara inicial
	writeRaster(mask_inicio,filename=paste0("Mask_inicio",year2,"_",year1,"_",round(corte_val,2),"val"),format="GTiff",datatype="FLT4S",overwrite=T)
	print("Mascara inicial lista")

	#Crea la máscara final en función de la media de la imagen
	mask_fin<-crop(mask_fin,im1)
	mask_fin<-resample(mask_fin,im1)
	val_mask_fin<-cellStats(mask_fin,mean,na.rm=T)
	mask_fin[mask_fin>=val_mask_fin]<-NA
	mask_fin[mask_fin<val_mask_fin]<-1
	#Escribe la máscara final
	writeRaster(mask_fin,filename=paste0("Mask_fin",year2,"_",year1,"_",round(corte_val,2),"val"),format="GTiff",datatype="FLT4S",overwrite=T)
	print("Mascara final lista")

	#Sacar los años de la imagen de resta
	num<-unlist(strsplit(names(im1),"[^[:digit:]]"))
	num<-unique(as.numeric(num))
	num<-num[-(is.na(num))]

	#Definir la matriz de reclasificacion
	reclas_m<-matrix(ncol=3,nrow=2)
	reclas_m[1,]<-c(cellStats(im1,min,na.rm=T)-0.000001,corte_val,1)
	reclas_m[2,]<-c(corte_val+0.000001,cellStats(im1,max,na.rm=T),NA)

	#im1<-mask(im1,reclass_M)
	#im1<-mask(im1,mask_loc)

	#Enmascara la imagen
	im1<-mask(im1,reclass_A)
	temp_im<-im1
	im1<-mask(im1,mask_inicio)
	im1<-mask(im1,mask_fin)
	print("Listo mascaras aplicadas")

	#Hacer la reclasificación
	im1_reclass<-reclassify(im1,reclas_m)
	#Imprimir la clasificación
	plot(im1_reclass)

	#Quizás valdría meter como última máscara los pixeles de cambio del año anterior y eleminarlos del actual
	#Y verificar pixeles como deforestados con el período posterior, y si no enmascararlos

	#Escribe reclasificación
	writeRaster(im1_reclass,filename=paste0("Rec_",year2,"_",year1,"_",round(corte_val,2),"val"),format="GTiff",datatype="FLT4S",overwrite=T)

	#temp_im<-reclassify(temp_im,reclas_m)
	#Escribe reclasificación
	#writeRaster(temp_im,filename=paste0("Rec_",year1,"_",year2,"_",corte_val,"val",round(val_mask_inicio,2),"iniNoForest"),format="GTiff",datatype="FLT4S",overwrite=T)

	#Enmascarar la imagen reclasficada con el shape de anps
	#im1_reclass<-mask(im1_reclass,shape)
	ha_def<-(cellStats(im1_reclass,sum,na.rm=T)*(res(im1)[1]*res(im1)[2]))/10000
	print(paste0(ha_def,"ha deforestadas en",year2,"_", year1))
	
	#Definir los puntos aleatorios pa verificar
	ptos<-randomPoints(im1_reclass,n=num_ptos)
	ptos_table<-data.frame(ID=seq(1,num_ptos,1),X=ptos[,1],Y=ptos[,2])
	coordinates(ptos_table)<-~X+Y
	proj4string(ptos_table)<-crs(im1)

	#ptos_table<-SpatialPolygonsDataFrame(buff,data=data.frame(ID=1))
	writeOGR(ptos_table,paste0("Shape_25_ptos_", year2,"_", year1),layer="ID",driver="ESRI Shapefile",overwrite=T)
}