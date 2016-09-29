inpDir1<- "C:/Claire/GPEM_Postdoc/1_USG_Collaboration/Analysis/tables/1_bymine.csv"
inpDir2<- "C:/Claire/GPEM_Postdoc/1_USG_Collaboration/Analysis/tables/1_high_data.csv"
outDir1 <- "C:/Claire/GPEM_Postdoc/1_USG_Collaboration/Analysis/tables/1_high_data_bymine_split.csv"
outDir2 <- "C:/Claire/GPEM_Postdoc/1_USG_Collaboration/Analysis/tables/1_high_data_bymine.csv"

dataSplit <- read.csv(inpDir1)
dataOriginal <- read.csv(inpDir2)

#divide each column by the number of mines sharing the line
oldData <- dataOriginal[,c("Area_km2", "Length_km","Num_shared", "Total.area.per.scenario","Average.loss.per.species", "Sum.species.loss", "SUM_AgriPr", "Number.of.properties"),]
newData <- sweep(oldData, MARGIN=1, 1/dataOriginal$Num_shared, '*')
newData2 <- data.frame(dataOriginal[,c("Id", "FIRST_name", "FIRST_Port", "MEAN_NPV_p"), ], newData)

#Match up this data with the other dataset by FID
dataSplitUpdate <- newData2[match(dataSplit$FID,newData2$Id), ]
outData <- data.frame(dataSplit, dataSplitUpdate)

write.csv(outData, outDir1, row.names=FALSE)

#Aggregate by mine
Mine <- matrix(unique(outData$mine))
MEAN_NPV <- c()
sumData <- c()
for (currMine in Mine){
	subData <- subset(outData, mine==currMine)
	currSum <- apply(subData[,c("Area_km2", "Length_km","Num_shared", "Total.area.per.scenario", "Average.loss.per.species", "Sum.species.loss", "SUM_AgriPr", "Number.of.properties")],MARGIN=2, sum)
	currMean <- mean(subData$MEAN_NPV_p)
	sumData <- rbind(sumData, currSum)
	MEAN_NPV <- append(MEAN_NPV, currMean)
	}

outAgg <- data.frame(Mine, sumData, MEAN_NPV)
write.csv(outAgg, outDir2, row.names=FALSE)

	