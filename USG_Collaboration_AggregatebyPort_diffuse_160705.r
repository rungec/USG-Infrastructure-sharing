inpDir1<-"C:/Claire/GPEM_Postdoc/1_USG_Collaboration/Analysis/tables/1_high_data.csv"
inpDir2<- "C:/Claire/GPEM_Postdoc/1_USG_Collaboration/Analysis/tables/1_high_diffuse_biodiversity_totals.csv"
outDir1 <- "C:/Claire/GPEM_Postdoc/1_USG_Collaboration/Analysis/tables/1_high_diffuse_biodiversity_byport.csv"

library(plyr)

dataNum <- read.csv(inpDir1)
dataOriginal <- read.csv(inpDir2)

#set up data
data2<-data.frame(dataOriginal[,3:5], dataNum[,c(4:5)])

outdat <- sapply(levels(data2$FIRST_Port), function(x){
 a <- subset(data2, data2$FIRST_Port==x)
 b <- colSums(a[,2:5])
 })
 
outdat <- rbind(outdat, SumSpTotal=colSums(outdat[1:2,]))
 
write.csv(outdat, outDir1, row.names=TRUE)
