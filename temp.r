D:/Claire/GPEM_Postdoc/1_USG_Collaboration/Data/Infrastructure shps/1_High_collab/1_High_infrastructure.shp


for (i in filelist){
print(i)
raster(i)}


allscenarios <- sapply(seq_along(scenFolds), function(i){

	#Read in the polygon for the current scenario
	currScen <- paste0(scenPath, scenFolds[i])
	print(currScen)
	})
	
	
	df1<-cbind(1:10, 20:30, 45:55)
	df2 <- df1*2
	df3 <- df1/0.03
	a<-list(df1, df2, df3)
	d<-sapply(a, colSums)
	b<-matrix(unlist(a),nrow=11, byrow=FALSE) 
	
	require(rgdal)
	names(smallStack) <- sppNames
	outDirStack <- "D:/Claire/GPEM_Postdoc/1_USG_Collaboration/Data/Species/Species_Rasters_50m/181spp_50m_"
	writeRaster(smallStack, outDirStack, format='GTiff', bylayer=TRUE, suffix='names')
	
	