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

#Import species distributions as raster stack
specStack <- stack(filelist)
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
	outRast <- paste(scenPath, gsub(basename(currPolyPath), pattern=".shp", replacement=".tif", fixed=TRUE), sep="/")
	writeRaster(polyRaster, outRast, format='GTiff')
	return(polyRaster)
}

polyRastList <- sapply(scenFolds, polyRastFun)

###Extract cell values where each scenario overlaps each species distribution
#function
for (i in 1:length(scenFolds)){
			currScen <- gsub(basename(scenFolds[[i]]), pattern="infrastructure.shp", replacement="", fixed=TRUE)
			currPolyRast <- polyRastList[[i]]
			currPolyRast <- currPolyRast/100
			
			currDF <-c()
			for (i in 1:length(filelist)){
				
				currRast <- currPolyRast*specStack[[i]]
				outRast <- paste0(outDirSpec, currScen, gsub(basename(filelist[[i]]), pattern=".asc", replacement=".tif", fixed=TRUE))
				writeRaster(currRast, outRast, format='GTiff')	
				
				currSum <- cellStats(currRast, stat='sum', na.rm=TRUE)
				currMean <- cellStats(currRast, stat='mean', na.rm=TRUE)
				ttlSum <- cellStats(specStack[[i]], stat='sum', na.rm=TRUE)
				ttlMean <- cellStats(specStack[[i]], stat='mean', na.rm=TRUE)
				currDF <- rbind(currDF, c(currSum, currMean, ttlSum, ttlMean))
			}

	finalDF <- data.frame(sppNames, currDF)
	names(finalDF) <- c("Species", "Total_infrastructure_loss", "Mean_infrastructure_loss", "Total_species_occurrence", "Mean_species_occurrence")
	outPath <- paste0(outDir, currScen, "Biodiversity_values.csv")#save as .csv
	write.csv(finalDF, outPath, row.names=FALSE)
}

	
#Total habitat per species
totalhabitat <- cellStats(specStack, stat='sum', na.rm=TRUE)
meanhabitat <- cellStats(specStack, stat='mean', na.rm=TRUE)
totalDF <- data.frame(sppNames, totalhabitat, meanhabitat)

outPath <- paste0(outDir, "Total_habitat_per_species.csv")#save as .csv
write.csv(totalDF, outPath, row.names=FALSE)






