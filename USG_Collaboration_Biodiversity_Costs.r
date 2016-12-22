#USG Collaboration Biodiversity benefit
#This script calculates how much of each species' range is lost to infrastructure under four scenarios

library(raster)
library(maptools)
library(dplyr)

inpPath <- "Z:/ayesha/USG/Zonation/input_181sp/"
inpFolds <- c("sdmspart1atog","sdmspart2htoo","sdmspart3ptox","inputB_SNES", "inputC_points")
scenPath <- "D:/Claire/GPEM_Postdoc/1_USG_Collaboration/Data/Infrastructure shps"
scenFolds <- c("/1_High_collab/1_High_infrastructure.shp","/2_Medium_collab/2_medium_infrastructure.shp","/3_Low_collab/3_low_infrastructure.shp","/4_No_collab/4_none_infrastructure.shp")
outDir <- "D:/Claire/GPEM_Postdoc/1_USG_Collaboration/Analysis/Biodiversity Costs/"

#########################
###PRELIMINARY PROCESSING
#########################

#Create a vector of all the species .ascs
files <- sapply(seq_along(inpFolds), function(x){ 
					currInp <- paste0(inpPath, inpFolds[x])
					list.files(currInp, full.names=TRUE)
					})
filelist <- unlist(files)
specStack <- stack(filelist)
smallStack <- disaggregate(specStack, 5)

#rm(specStack)
sppNames<-filelist %>% 
	basename() %>%
		gsub(pattern=".asc", replacement="", fixed=TRUE)

##########################
###MAIN PROCESSING
##########################
###Extract cell values where each scenario overlaps each species distribution
#function
biofun <- function(i){
	
	currScen <- paste0(scenPath, i)#Read in the polygon for the current scenario
	currPoly <- readShapePoly(currScen)#scenario shapefile
	proj4string(currPoly) <- "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs <>"

	biolist <- extract(smallStack, currPoly, fun=NULL, na.rm=FALSE)
	biounlist <- matrix(unlist(biolist), ncol=length(filelist), byrow=FALSE)
	
	fid <- unlist(sapply(c(1:length(biolist)), function(x){
					rep(x, nrow(biolist[[x]]))}))#make a vector of the polygon id for each row
	
	biovalues <- data.frame(fid, biounlist)#turn output into a dataframe
	names(biovalues) <- c("PORT", sppNames)
	
	outFile <- i %>%
				basename() %>%
				gsub(pattern="infrastructure.shp", replacement="Biodiversity_values.csv", fixed=TRUE)
	outPath <- paste0(outDir, outFile)#save as .csv
	write.csv(biovalues, outPath, row.names=FALSE)
	return(biovalues)
	}

allscenarios <- sapply(scenFolds, biofun)

############################
###POST PROCESSING
############################

#Total amount of habitat lost per scenario
habitatlost <- sapply(allscenarios, function(x) colSums(x, na.rm=TRUE))
stats <- data.frame(sppNames, habitatlost)#turn output into a dataframe

#Percentage habitat lost per scenario
totalhabitat <- cellStats(specStack, stat='sum', na.rm=TRUE)
stats <- cbind(stats, totalhabitat)








