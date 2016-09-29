#This script creates a single raster where each cell represents the number of EPBC listed species found in that cell (max 183 species)

inpDir <- "C:/Claire/GPEM_Postdoc/1_USG_Collaboration/Data/Species/thresholdmaps/183sp/tif_thresholded/"
outDir <- "C:/Claire/GPEM_Postdoc/1_USG_Collaboration/Data/Species/thresholdmaps/183sp/summed_presence/Summed_presence_183Sp.tif"

library(raster)
library(sp)
library(rgdal)

#Create a list of files
filelist <- list.files(inpDir, full.names=TRUE)

#Make a raster stack for species, then calculate the sum
currStack <- stack(filelist)
currMax <- overlay(x=currStack, fun=function(x){sum(x, na.rm=T)})

	
#Save summed raster to file
writeRaster(currMax, outDir, format="GTiff")

######################
#this was run first as the snes hadn't reclassified properly in .tif (shps used for further processing were ok)
#reclassify snes
inpDir <- "C:/Claire/GPEM_Postdoc/1_USG_Collaboration/Data/Species/thresholdmaps/snes/tif/"
outDir <- "C:/Claire/GPEM_Postdoc/1_USG_Collaboration/Data/Species/thresholdmaps/183sp/tif_thresholded/"

library(raster)
library(sp)
library(rgdal)

#Create a list of files
filelist <- list.files(inpDir, full.names=TRUE)
reclassmat <- m <- c(0, 0.4, 0,  0.4, 1, 1)
rclmat <- matrix(m, ncol=3, byrow=TRUE)

for (i in filelist){
rast <- raster(i)
outPath<-paste0(outDir, basename(i))
reclassify(rast, rclmat, filename=outPath)
}