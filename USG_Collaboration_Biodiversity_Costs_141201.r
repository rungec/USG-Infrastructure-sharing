#USG Collaboration Biodiversity benefit
#This script calculates how much of each species' range is lost to infrastructure under four scenarios
library(dplyr)
require(rgdal)
require(rgeos)
library(raster)
library(maptools)


inpPath <- "C:/Claire/GPEM_Postdoc/1_USG_Collaboration/Data/Species/thresholdmaps/181sp/tif/"
scenDir <- "C:/Claire/GPEM_Postdoc/1_USG_Collaboration/Data/Infrastructure shps/Shapefiles_v2/"
scenPaths <- c("1_high_infrastructure_v2.shp","2_medium_infrastructure_v2.shp","3_low_infrastructure_v2.shp","4_none_infrastructure_v2.shp")
templateDir <- "D:/Claire/GPEM_Postdoc/1_USG_Collaboration/Data/Species/Test raster/test_rast.asc"
outDir <- "C:/Claire/GPEM_Postdoc/1_USG_Collaboration/Analysis/Biodiversity Costs/"
outDirSpec <- "C:/Claire/GPEM_Postdoc/1_USG_Collaboration/Data/Species/Species_Scenario_overlay/"

#########################
###PRELIMINARY PROCESSING
#########################

#Create a vector of all the species .tifs
files <- list.files(inpPath, full.names=TRUE)
# #load scenarios
# scenShps <- sapply(scenPaths, function(x){
				# currPath <- paste0(scenDir, x)
				# currPoly <- readShapePoly(currPath)
				# proj4string(currPoly) <- "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs <>"
				# }
				
for (i in files){
		spc <- raster(i)
		print("done raster")
		shp <- rasterToPolygons(spc, fun=NULL, n=4, na.rm=TRUE, digits=12, dissolve=TRUE)
		proj4string(shp) <- "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs <>"
		print("done polygon")
		outfun <-	function(i) {
								a<-gsub("tif", "shp", i) 
								return(gsub(".shp", "", a))
								}
		outPath <- outfun(i)
		writePolyShape(shp, outPath)
		print("moving to next")
}
