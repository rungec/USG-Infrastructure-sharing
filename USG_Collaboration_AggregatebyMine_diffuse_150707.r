inpDir1 <- "C:/Claire/GPEM_Postdoc/1_USG_Collaboration/Analysis/tables/7_bymine.csv"
inpDir1b <-"C:/Claire/GPEM_Postdoc/1_USG_Collaboration/Analysis/tables/7_lowimpact_shared_data.csv"
inpDir2 <- "C:/Claire/GPEM_Postdoc/1_USG_Collaboration/Analysis/tables/7_lowimpact_shared_diffuse_biodiversity_totals.csv"
#inpDir3 <- "C:/Claire/GPEM_Postdoc/1_USG_Collaboration/Analysis/tables/7_lowimpact_shared_infrastructure_v2.csv"
outDir1 <- "C:/Claire/GPEM_Postdoc/1_USG_Collaboration/Analysis/tables/7_lowimpact_shared_diffuse_biodiversity_bymine_split.csv"
outDir2 <- "C:/Claire/GPEM_Postdoc/1_USG_Collaboration/Analysis/tables/7_lowimpact_shared_diffuse_biodiversity_bymine.csv"
outDir3 <- "C:/Claire/GPEM_Postdoc/1_USG_Collaboration/Analysis/tables/final costs/7_lowimpact_shared_finalcosts.csv"



###########
#Extract only the rows that are in scenario 5 (dataNum & dataOriginal were files  created by manually combining scenario 1 & scenario 3 files, this section deletes the unneeded rows) - only need to do this once

# dataTemplate <- read.csv(inpDir3) #list of the FIDs and mines associated with each
# dataNum <- read.csv(inpDir1b)
# dataOriginal <- read.csv(inpDir2)
# dataNumNew <- merge(dataTemplate, dataNum, by.x=c("Id","Scen"), by.y=c("Id","Scen"), all.x=TRUE)[,c("Id", "Scen", "FID", "name", "Port", "Num_shared.x", "Shared", "Area_km2.y","Length_km.y", "Num_shared.y", "Total.area.per.scenario", "Average.loss.per.species","Sum.species.loss", "MEAN_NPV_p", "SUM_AgriPr", "Number.of.properties")]
# names(dataNumNew) <- c("Id", "Scen", "FID", "name", "Port", "Num_shared", "Shared", "Area_km2","Length_km", "Num_shared", "Total.area.per.scenario", "Average.loss.per.species","Sum.species.loss", "MEAN_NPV_p", "SUM_AgriPr", "Number.of.properties")
# dataOriginalNew <- merge(dataTemplate, dataOriginal, by.x=c("Id","Scen"), by.y=c("Id","Scen"), all.x=TRUE)

# write.csv(dataNumNew, inpDir1b, row.names=FALSE)
# write.csv(dataOriginalNew, inpDir2, row.names=FALSE)

###
#For scenario 7 low impact shared, I only needed to add FID 27 from scenario 3 unshared, and delele some rows from scenario 1 shared
#this was done manually in excel

###########

dataSplit <- read.csv(inpDir1)
dataOriginal <- read.csv(inpDir2)
dataNum <- read.csv(inpDir1b)

#Merge dataset
#dataMerge <- merge(dataNum, dataOriginal[,c(1,2,12,13)], by.x=c("Id","Scen"), by.y=c("Id","Scen")) #scenario 5
dataMerge <- merge(dataNum, dataOriginal[,c(1,2,5,6)], by.x=c("Id","Scen"), by.y=c("Id","Scen")) #scenario 7

#divide each column by the number of mines sharing the line
oldData <- dataMerge[,c("Num_shared", "Area_km2", "Length_km", "Total.area.per.scenario", "Average.loss.per.species", "Sum.species.loss", "MEAN_NPV_p", "SUM_AgriPr", "Number.of.properties", "SumSpLoss", "SumSpLossDiffuse"),]
oldData[oldData$Num_shared==0, "Num_shared"] <- 1
newData <- sweep(oldData[,2:ncol(oldData)], MARGIN=1, 1/oldData$Num_shared, '*')
newData2 <- data.frame(dataMerge[,c("Id", "Scen")], newData)

#Match up this data with the dataset that has multiple rows per line
dataSplitUpdate <- merge(dataSplit, newData2, by.x=c("Id","Scen"), by.y=c("Id","Scen"), all.x=TRUE)
names(dataSplitUpdate) <- c("Id", "Scen", "FID", "name", "Port", "Area_km2", "Length_km", 
"Num", "Shared", "mine", "Area_km2_attributedtomine", "Length_km_attributedtomine", "Total.area.per.scenario", "Average.loss.per.species", "Sum.species.loss", "MEAN_NPV_p", "SUM_AgriPr", "Number.of.properties","SumSpLoss", "SumSpLossDiffuse")
write.csv(dataSplitUpdate, outDir1, row.names=FALSE)

#Aggregate by mine
Mine <- matrix(unique(dataSplitUpdate$mine))
sumData <- c()
for (currMine in Mine){
	subData <- subset(dataSplitUpdate, mine==currMine)
	currSum <- apply(subData[,c("Area_km2_attributedtomine", "Length_km_attributedtomine", "Total.area.per.scenario", "Average.loss.per.species", "Sum.species.loss", "MEAN_NPV_p", "SUM_AgriPr", "Number.of.properties", "SumSpLoss", "SumSpLossDiffuse")],MARGIN=2, sum)
	sumData <- rbind(sumData, currSum)
	}

outAgg <- data.frame(Mine, sumData)
write.csv(outAgg, outDir2, row.names=FALSE)

################
#Calculate financial costs fore each mine

outAgg$TransCost <- outAgg$Number.of.properties*10000
outAgg$RailCostExtraHigh <- outAgg$Length_km_attributedtomine*4000000

names(outAgg) <- c("Mine", "Area", "Len", "TotalSpLoss", "AvSpLoss", "SumSpLoss", "NPV", "AgriLoss", "NumProperties", "SumSpLoss183sp", "SumSpLoss183spDiffuse", "TransCost", "RailCostExtraHigh")


write.csv(outAgg, outDir3, row.names=FALSE)

#####################
#Calculate impacts by species
#impactBySpecies <- matrix(colSums(dataOriginal[,14:745]), ncol=4, byrow=TRUE) #scenario 5
impactBySpecies <- matrix(colSums(dataOriginal[,7:738]), ncol=4, byrow=TRUE) #scenario 7

#total number of species impacted
length(which(rowSums(impactBySpecies)!=0))


#Mean habitat loss per species (% of total habitat)
mean(rowSums(impactBySpecies))


	