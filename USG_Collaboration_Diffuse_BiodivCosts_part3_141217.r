inpDirTTL <- "C:/Claire/GPEM_Postdoc/1_USG_Collaboration/Data/Species/thresholdmaps/191sp/Species_Area/"
inpDirScen <- "C:/Claire/GPEM_Postdoc/1_USG_Collaboration/Data/Species/thresholdmaps/191sp/Species_Scenario_Area_diffuse/"
diffDir <- "C:/Claire/GPEM_Postdoc/1_USG_Collaboration/Data/Species/Species spreadsheets/Species_list_taxa_impacts.csv"
#outTTL <- "C:/Claire/GPEM_Postdoc/1_USG_Collaboration/Data/Species/Species spreadsheets/183sp/Species_Area_totals.csv"
outDir <- "C:/Claire/GPEM_Postdoc/1_USG_Collaboration/Data/Species/Species spreadsheets/183sp/"
outScen <- "C:/Claire/GPEM_Postdoc/1_USG_Collaboration/Data/Species/Species spreadsheets/183sp/Species_Areas_Raw_"
outPerc <- "C:/Claire/GPEM_Postdoc/1_USG_Collaboration/Data/Species/Species spreadsheets/183sp/Species_Areas_ImpactasPercent_"
ScenApps <- c(paste(rep(c("1_High", "2_medium", "3_low", "4_none"), each=3), rep(c("250m", "500m", "1km"), times=4), sep="_"))


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
#write.csv(ttlmat, outTTL, row.names=FALSE)

AreaVec <- ttlmat$Total_Area

#Make a table of the area occupied by each species in each scenario
for (i in 1:12){
	currScen<- ScenApps[i]
	currSubList <- scenfiles[grep(pattern=currScen, scenfiles)]
	idDBF <- read.dbf(currSubList[1])
	idMat <- idDBF[,c("Id", "FIRST_name", "FIRST_Port")]
	spList <- c()
	for (currSp in currSubList){
		currSpName <- paste(strsplit(basename(currSp), "_")[[1]][5], strsplit(basename(currSp),"_")[[1]][6], sep=" ")
		currAreaDBF <- read.dbf(currSp)
		currArea <- currAreaDBF$SUM_Sp_are
		spList <- append(spList, currSpName)
		idMat <- cbind(idMat, currArea)
	}
	idMat <- data.frame(idMat)
	names(idMat) <- c("Id", "FIRST_name", "FIRST_Port", spList)
	outidMat <- paste0(outScen, currScen, "_diffuse.csv")
	write.csv(idMat, outidMat, row.names=FALSE)
}
####
#remove the overlapping portions of each buffered scenario (ie clip out 250m from 500m and 500m from 1km)

Scen1km<- ScenApps[c(3,6,9,12)]
Scen500m<-ScenApps[c(2,5,8,11)]
Scen250m<-ScenApps[c(1,4,7,10)]

for (i in 1:4){
				
				inpPath1km <- paste0(outScen, Scen1km[i], "_diffuse.csv")
				curr1km<-read.csv(inpPath1km, header=TRUE, stringsAsFactors=FALSE)
				currSub1km<- curr1km[,4:ncol(curr1km)]
				inpPath500m <- paste0(outScen, Scen500m[i], "_diffuse.csv")
				curr500m<-read.csv(inpPath500m, header=TRUE, stringsAsFactors=FALSE)
				currSub500m<- curr500m[,4:ncol(curr500m)]
				inpPath250m <- paste0(outScen, Scen250m[i], "_diffuse.csv")
				curr250m<-read.csv(inpPath250m, header=TRUE, stringsAsFactors=FALSE)
				currSub250m<- curr250m[,4:ncol(curr250m)]
				
				newScen1km <- currSub1km-currSub500m
				newScen1km <- data.frame(curr1km[1:3], newScen1km)
				outPath <- paste0(outScen, Scen1km[i], "_diffuse.csv")
				write.csv(newScen1km, outPath, row.names=FALSE)
				
				newScen500m <-currSub500m-currSub250m
				newScen500m <- data.frame(curr500m[1:3], newScen500m)
				outPath <- paste0(outScen, Scen500m[i], "_diffuse.csv")
				write.csv(newScen500m, outPath, row.names=FALSE)
}

#################	
#calculate percent of each species range lost to each mine-port link		
for (i in 1:12){
	currPath <- paste0(outScen, ScenApps[i], "_diffuse.csv")
	idMat <- read.csv(currPath, header=TRUE, stringsAsFactors=FALSE)
	subMat <- idMat[,4:ncol(idMat)]
	
	#multiply the area by the diffuse impact and
	#divide the species area under each mine-port link by the total area for that species
	diffImpacts <- read.csv(diffDir,stringsAsFactors=FALSE)
	diffImpacts2 <- diffImpacts[match(spList, diffImpacts$Species),]
	if (i %in% c(1,4,7,10)){
		currImpact <- diffImpacts$Dist.250
	} else if (i %in% c(2,5,8,11)){
		currImpact <- diffImpacts$Dist.500
	} else {
		currImpact <- diffImpacts$Dist.1000
	}
	print(head(currImpact))
	diffPerc <- sweep(subMat,	MARGIN=2, (1-currImpact)/AreaVec, `*`)
	diffPerc <-diffPerc*100
	diffDF <- data.frame(idMat[,1:3], diffPerc)
	outDiffpath <- paste0(outPerc, ScenApps[i], "_diffuse.csv")
	write.csv(diffDF, outDiffpath, row.names=FALSE)
}

