inpDirTTL <- "C:/Claire/GPEM_Postdoc/1_USG_Collaboration/Data/Species/thresholdmaps/191sp/Species_Area/"
inpDirScen <- "C:/Claire/GPEM_Postdoc/1_USG_Collaboration/Data/Species/thresholdmaps/191sp/Species_Scenario_Area/"
outTTL <- "C:/Claire/GPEM_Postdoc/1_USG_Collaboration/Data/Species/Species spreadsheets/183sp/Species_Area_totals.csv"
outScen <- "C:/Claire/GPEM_Postdoc/1_USG_Collaboration/Data/Species/Species spreadsheets/183sp/Species_Areas_"
outPerc <- "C:/Claire/GPEM_Postdoc/1_USG_Collaboration/Data/Species/Species spreadsheets/183sp/Species_Areas_Percent_"
ScenApps <- c("1_high", "2_medium", "3_low", "4_none")

library(foreign)
library(dplyr)
ttlfiles <- list.files(inpDirTTL, pattern="*.dbf", full.names=TRUE)
scenfiles <- list.files(inpDirScen, pattern="*.dbf", full.names=TRUE)

#Make a table of the total area occupied by each species
ttlmat <- c()
for (currFile in ttlfiles){

	AreaDBF <- read.dbf(currFile)
	spp <- paste(strsplit(basename(currFile), "_")[[1]][1], strsplit(basename(currFile), "_")[[1]][2], sep=" ")
	area <- AreaDBF$Area_km2[AreaDBF$Prob==1]
	currstats <- data.frame(spp, area)
	ttlmat <- rbind(ttlmat, currstats)
}
ttlmat <- data.frame(ttlmat, stringsAsFactors=FALSE)
names(ttlmat)<-c("Species", "Total_Area")
write.csv(ttlmat, outTTL, row.names=FALSE)

AreaVec <- ttlmat$Total_Area

#Make a table of the area occupied by each species in each scenario
for (i in 1:4){
	currScen<- ScenApps[i]
	currSubList <- scenfiles[grep(pattern=currScen, scenfiles)]
	idDBF <- read.dbf(currSubList[1])
	idMat <- idDBF[,c("Id", "FIRST_name", "FIRST_Port")]
	spList <- c()
	for (currSp in currSubList){
		currSpName <- paste(strsplit(basename(currSp), "_")[[1]][3], strsplit(basename(currSp),"_")[[1]][4], sep=" ")
		currAreaDBF <- read.dbf(currSp)
		currArea <- currAreaDBF$SUM_Sp_are
		spList <- append(spList, currSpName)
		idMat <- cbind(idMat, currArea)
	}
	idMat <- data.frame(idMat)
	names(idMat) <- c("Id", "FIRST_name", "FIRST_Port", spList)
	outidMat <- paste0(outScen, currScen, ".csv")
	write.csv(idMat, outidMat, row.names=FALSE)
	
	#calculate percent of each species range lost to each mine-port link
	subMat <- idMat[,4:ncol(idMat)]
	
	#divide the species area under each mine-port link by the total area for that species
	PercArea <- sweep(subMat, MARGIN=2, 1/AreaVec, `*`)
	PercArea <- PercArea*100
	PercDF <- data.frame(idMat[,1:3], PercArea)
	outPercpath <- paste0(outPerc, currScen, ".csv")
	write.csv(PercDF, outPercpath, row.names=FALSE)
	
}
	
	
	