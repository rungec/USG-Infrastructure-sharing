#Set libraries
library(raster)
library(sp)
library(rgdal)

#_____________________________________________________________________________________
#Calculate range size of SDM species
#____________________________________________________________________________________

inpDir <-"Z:/ayesha/USG/MaxEnt/outputs/final/revised/mamm/" #edit me
#dir() #check folders in current directory
#inpFolders <- c("revised/mamm/")#edit this with your folder names
outDir <- "Z:/ayesha/USG/MaxEnt/outputs/thresholdmaps/sdm/"#edit me - directory for species maps
outDirsdm <- "Z:/ayesha/USG/MaxEnt/outputs/thresholdmaps/sdm/"#edit me - directory for species maps
outRanges <- "Z:/ayesha/USG/MaxEnt/outputs/USG_Ranges_revmamm.csv"#edit me
outSpecies <- "Z:/ayesha/USG/MaxEnt/outputs/USG_sp_revmamm.csv"#edit me
ThresholdPath <- "maxentResults (2).csv"


#create area vector
AreaVector <- c()

#create the final dataframe with $ column heading but no rows as defined by (0)
Sp_area<-data.frame(Species=character(0), TotalNumCells=numeric(0), Area_m2=numeric(0), Area_km2=numeric(0), threshold=numeric(0))

#loop through folders
 #for (currFolder in inpFolders){
 	currDir <- paste0(inpDir)
  ThresholdDir <- paste0(inpDir, ThresholdPath)
 	filelistsdm <- list.files(currDir, "_avg.asc")
 	thresholds <- read.csv(ThresholdDir, header=TRUE)

# check not using loop
# currFolder<-inpFolders[1]
# currDir <- paste0(inpDir, currFolder)
filelistsdm <- list.files(currDir, "_avg.asc", full.names=TRUE)
# filenamesdm<- list.files(currDir, "_avg.asc")
# ThresholdDir <- paste0(currDir, "/maxentResults.csv")
# thresholds <- read.csv(ThresholdDir, header=TRUE)

	#loop through species
for (i in filelistsdm){
	
    currSpec <- gsub("_avg.asc", " (average)",basename(i))
	#reclassify to presence absence based on threshold
	#currthresh <- 0.5 #for static threshold
    currthresh <- thresholds[thresholds$Species==currSpec, "Equal.test.sensitivity.and.specificity.logistic.threshold"]
    #you may choose a different threshold
	currRastDir <- i
    currRast <- raster(currRastDir)
	reclassRast <- reclassify(currRast, c(0,currthresh,0, currthresh,1.0,1))
	outName <- gsub("_avg.asc", "",basename(i))
    outPath <- paste0(outDir, outName, "_range.tif")#edit this
	writeRaster(reclassRast, outPath,format="GTiff")

	#calculate range size
		TotalNumCells <- cellStats(reclassRast, sum, na.rm=TRUE)
		AreaVector <- append(AreaVector, TotalNumCells)
  
  #Bind results to dataframe
	dataframe<-data.frame(i)
	dataframe$TotalNumCells <-TotalNumCells
	dataframe$Area_m2 <- TotalNumCells*(250^2)
	dataframe$Area_km2 <- TotalNumCells*0.0625
	dataframe$threshold<- currthresh
	Sp_area <- rbind(Sp_area, dataframe) 
  
	}
# # #Calculate total range area in m2 for individual species, creating separate vectors
# TotalArea <- AreaVector*(250^2)
# TotalArea_km2<- AreaVector*0.0625
# write.csv(AreaVector, outRanges)
# specieslist<-list.files(outDirsdm)
# write.csv(specieslist, outSpecies)

#file paths
expPath1<-"Z:/ayesha/USG/MaxEnt/outputs/thresholdmaps/range_speciessdms_revmamm.csv"

#export table
write.table(Sp_area, expPath1, sep=",", row.names=FALSE)


#_____________________________________________________________________________________
#Calculate range size of SNES species
#____________________________________________________________________________________
#inpDirsnes <-"X:/ayesha/Zonation/input_June6/inputB_SNES/" #edit me
dir() #check folders in current directory
outDirsnes <- "X:/ayesha/USG/MNES/snes_reclass_Aug2014/"#edit me - directory for species maps
outRangessnes <- "X:/ayesha/USG/MNES/snes_reclass_Aug2014/USG_snesRanges.csv"#edit me
outSpeciessnes <- "X:/ayesha/USG/MNES/snes_reclass_Aug2014/USG_snessp.csv"#edit me


#get file names
filelistsnes <- list.files("X:/ayesha/Zonation/input_June6/inputB_SNES/", ".asc")
filepathsnes<- list.files(path=file.path("X:/ayesha/Zonation/input_June6/inputB_SNES/"), full.names=TRUE)
filepathsnes[6] #finds the 6th element of the file path
k<-length(filepathsnes)

#create area vector
AreaVector <- c()

#create the final dataframe with $ column heading but no rows as defined by (0)
Sp_area_snes2<-data.frame(Species=character(0), TotalNumCells=numeric(0), Area_m2=numeric(0), Area_km2=numeric(0), threshold=numeric(0))


#calculate range size
#loop through species
for (i in filepathsnes){
  snesthresh <- 0.4
  currRast <- raster(i)
  reclassRast <- reclassify(currRast, c(0,snesthresh,0, snesthresh,1.0,1))
  currSpecsnes<- gsub("X:/ayesha/Zonation/input_June6/inputB_SNES/", "", i)
  currSpecsnes<- gsub(".asc", "",currSpecsnes)
  outPath <- paste0(outDirsnes, currSpecsnes, "_range.tif")#edit this
  #writeRaster(reclassRast, outPath, format="GTiff")
  
  #calculate range size
  TotalNumCells <- cellStats(reclassRast, sum, na.rm=TRUE)
  AreaVector <- append(AreaVector, TotalNumCells)
  
  #Bind results to dataframe
  dataframe<-data.frame(currSpecsnes)
  dataframe$TotalNumCells <- TotalNumCells
  dataframe$Area_m2 <- TotalNumCells*(250^2)
  dataframe$Area_km2 <- TotalNumCells*0.0625
  dataframe$threshold<- currthresh
  Sp_area_snes2 <- rbind(Sp_area_snes2, dataframe) 
  
}

#file paths
expPath2<-"X:/ayesha/USG/MNES/range_speciessnes2.csv"

#export table
write.table(Sp_area_snes2, expPath2, sep=",", row.names=FALSE)

