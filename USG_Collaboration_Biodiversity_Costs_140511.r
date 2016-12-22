#USG Collaboration Biodiversity benefit
#This script calculates how much of each species' range is lost to infrastructure under four scenarios

library(raster)
library(maptools)
library(dplyr)
require(rgdal)

inpPath <- "Z:/ayesha/USG/Zonation/input_181sp/"
inpFolds <- c("sdmspart1atog","sdmspart2htoo","sdmspart3ptox","inputB_SNES", "inputC_points")
scenPath <- "D:/Claire/GPEM_Postdoc/1_USG_Collaboration/Data/Infrastructure shps"
scenFolds <- c("/1_High_collab/1_High_infrastructure.shp","/2_Medium_collab/2_medium_infrastructure.shp","/3_Low_collab/3_low_infrastructure.shp","/4_No_collab/4_none_infrastructure.shp")
templateDir <- "D:/Claire/GPEM_Postdoc/1_USG_Collaboration/Data/Species/Test raster/test_rast.asc"
outDir <- "D:/Claire/GPEM_Postdoc/1_USG_Collaboration/Analysis/Biodiversity Costs/"
outDirSpec <- "D:/Claire/GPEM_Postdoc/1_USG_Collaboration/Data/Species/Species_Scenario_overlay/"

#########################
###PRELIMINARY PROCESSING
#########################

#Create a vector of all the species .ascs
files <- sapply(seq_along(inpFolds), function(x){ 
					currInp <- paste0(inpPath, inpFolds[x])
					list.files(currInp, full.names=TRUE)
					})
filelist <- unlist(files)
#exclude rasters that don't work
filelist <- filelist[-75]

#Import species distributions as raster stack
#specStack <- stack(filelist)
#Import a template raster
templateRast <- raster(templateDir)
templateRast[]<-NA #replace values with NA

#Make a list of species
sppNames<-filelist %>% 
	basename() %>%
		gsub(pattern=".asc", replacement="", fixed=TRUE)

##########################
###MAIN PROCESSING
##########################
#Create a raster with cell values equal to the amount of polygon in each cell
polyRastFun <- function(i){
	currPolyPath <- paste0(scenPath, i)#Read in the polygon for the current scenario
	currPoly <- readShapePoly(currPolyPath)#scenario shapefile
	proj4string(currPoly) <- "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs <>"
	polyRaster <- rasterize(currPoly, templateRast, getCover=TRUE)
	polyRaster <- polyRaster/100
	outRast <- paste(scenPath, gsub(basename(currPolyPath), pattern=".shp", replacement=".tif", fixed=TRUE), sep="/")
	writeRaster(polyRaster, outRast, format='GTiff')
	return(polyRaster)
}

polyRastList <- sapply(scenFolds, polyRastFun)

#Calculate overall stats
#ttlSum <- cellStats(specStack, stat='sum', na.rm=TRUE)
#ttlMean <- cellStats(specStack, stat='mean', na.rm=TRUE)

###Extract cell values where each scenario overlaps each species distribution
#function
specDF <- c()
for (i in 1:length(filelist)){
				print(paste("Starting ",sppNames[i], Sys.time(), sep=" "))
				#Calculate total area of habitat for each species
				specRast <- raster(filelist[i])
				ttlSum <- cellStats(specRast, stat='sum', na.rm=TRUE)
				ttlMean <- cellStats(specRast, stat='mean', na.rm=TRUE)
				
	currDF <-c(ttlSum, ttlMean)
	for (y in 1:length(scenFolds)){
			#Calculate area of habitat lost to each scenario
			currScen <- gsub(basename(scenFolds[[y]]), pattern="infrastructure.shp", replacement="", fixed=TRUE)
			currPolyRast <- polyRastList[[y]]
			currRast <- currPolyRast*specRast
			outRast <- paste0(outDirSpec, currScen, gsub(basename(filelist[[i]]), pattern=".asc", replacement=".tif", fixed=TRUE))
			writeRaster(currRast, outRast, format='GTiff')	
			currSum <- cellStats(currRast, stat='sum', na.rm=TRUE)
			currMean <- cellStats(currRast, stat='mean', na.rm=TRUE)
			currDF <- append(currDF, c(currSum, currMean))
		}
	specDF <- rbind(specDF, currDF)
	
}
	finalDF <- data.frame(sppNames, specDF)
	names(finalDF) <- c("Species", "Total_habitat", "Mean_habitat", paste0(rep(c("Total_loss_", "Mean_loss_"), 4), rep(c(1,2,3,4), each=2)))
	outPath <- paste0(outDir, "Biodiversity_values_allscenarios.csv")#save as .csv
	write.csv(finalDF, outPath, row.names=FALSE)







