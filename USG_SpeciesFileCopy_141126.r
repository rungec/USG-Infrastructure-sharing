#Copy files for 181spp

filenames <- "C:/Claire/GPEM_Postdoc/1_USG_Collaboration/Data/Species/Species list/List_of_threshold_filenames.csv"
inpDir <- "C:/Claire/GPEM_Postdoc/1_USG_Collaboration/Data/Species/thresholdmaps/"
app <- c("sdm/", "snes_reclass_Aug2014/")
outDir <- "C:/Claire/GPEM_Postdoc/1_USG_Collaboration/Data/Species/thresholdmaps/181sp/"

files <- read.csv(filenames)

for (i in 1:nrow(files)){
	if (files$SDM[i]=="YES"){
	inpPath <- paste0(inpDir, app[1], files$ThresholdedName[i])
	} else if (files$SNES[i]=="YES"){
	inpPath <- paste0(inpDir, app[2], files$ThresholdedName[i])
	}
	file.copy(inpPath, outDir)
	}