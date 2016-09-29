#Bind the dataframes and calculate totals
inpDir1<- "C:/Claire/GPEM_Postdoc/1_USG_Collaboration/Data/Species/Species spreadsheets/183sp/Species_Areas_Percent_"
inpDir2<- "C:/Claire/GPEM_Postdoc/1_USG_Collaboration/Data/Species/Species spreadsheets/183sp/Species_Areas_ImpactasPercent_"
ScenApps <- c(paste0(rep(c("1_high", "2_medium", "3_low", "4_none"), each=4), rep(c(".csv", "_250m_diffuse.csv", "_500m_diffuse.csv", "_1km_diffuse.csv"), times=4)))
Scens <- c("1_high", "2_medium", "3_low", "4_none")
numspecies <-183
outDir <- "C:/Claire/GPEM_Postdoc/1_USG_Collaboration/Analysis/tables/"

library(plyr)

filelist <- c(paste0(rep(rep(c(inpDir1, inpDir2), c(1,3)),4), ScenApps))

#Bind all the species loss values by scenario
for (i in 0:3){
	currFiles <- filelist[c(1:4+4*i)]
	impacts <- lapply(currFiles,read.csv,header=TRUE)
	impactsDF <- data.frame(Reduce(cbind, impacts))
	#reorder by species and calculate totals
	numcol <- numspecies+3
	colnums <- c(4:numcol, c(4:numcol)+numcol, c(4:numcol)+2*numcol, c(4:numcol)+3*numcol)
	subimpactsDF <- impactsDF[,colnums]
	subimpactsDF <- subimpactsDF[,c(order(names(subimpactsDF)))]
	SumSpLoss <- rowSums(subimpactsDF[,c(seq(1,ncol(subimpactsDF), 4))])
	SumSpLossDiffuse <- rowSums(subimpactsDF)
	#recombine datasets
	newimpactsDF <- data.frame(impactsDF[,c(1:3)], SumSpLoss, SumSpLossDiffuse, subimpactsDF)
	
	outPath <- paste0(outDir, Scens[i+1], "_diffuse_biodiversity_totals.csv")
	write.csv(newimpactsDF, outPath, row.names=FALSE)
}
