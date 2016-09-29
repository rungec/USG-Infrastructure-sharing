inpDir1<- "C:/Claire/GPEM_Postdoc/1_USG_Collaboration/Analysis/tables/1_bymine.csv"
inpDir1b<-"C:/Claire/GPEM_Postdoc/1_USG_Collaboration/Analysis/tables/1_high_data.csv"
inpDir2<- "C:/Claire/GPEM_Postdoc/1_USG_Collaboration/Analysis/tables/1_high_diffuse_biodiversity_totals.csv"
outDir1 <- "C:/Claire/GPEM_Postdoc/1_USG_Collaboration/Analysis/tables/1_high_diffuse_biodiversity_bymine_split.csv"
outDir2 <- "C:/Claire/GPEM_Postdoc/1_USG_Collaboration/Analysis/tables/1_high_diffuse_biodiversity_bymine.csv"

dataSplit <- read.csv(inpDir1)
dataNum <- read.csv(inpDir1b)
dataOriginal <- read.csv(inpDir2)

#set up data
data2<-cbind(dataOriginal[,1:6], dataNum[,c(4:6)])

#divide each column by the number of mines sharing the line
oldData <- dataOriginal[,c("SumSpLoss", "SumSpLossDiffuse"),]
newData <- sweep(oldData, MARGIN=1, 1/dataNum$Num_shared, '*')
newData2 <- data.frame(dataOriginal[,c("Id", "FIRST_name", "FIRST_Port"), ], newData)

#Match up this data with the other dataset by FID
dataSplitUpdate <- newData2[match(dataSplit$FID,newData2$Id), ]
outData <- data.frame(dataSplit, dataSplitUpdate)

write.csv(outData, outDir1, row.names=FALSE)

#Aggregate by mine
Mine <- matrix(unique(outData$mine))
sumData <- c()
for (currMine in Mine){
	subData <- subset(outData, mine==currMine)
	currSum <- apply(subData[,c("SumSpLoss", "SumSpLossDiffuse")],MARGIN=2, sum)
	sumData <- rbind(sumData, currSum)
	}

outAgg <- data.frame(Mine, sumData)
write.csv(outAgg, outDir2, row.names=FALSE)

	