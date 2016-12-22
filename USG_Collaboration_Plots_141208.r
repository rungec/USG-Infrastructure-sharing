inpDir1 <- "C:/Claire/GPEM_Postdoc/1_USG_Collaboration/Analysis/tables/1_high_finalcosts_141208.csv"
inpDir2 <- "C:/Claire/GPEM_Postdoc/1_USG_Collaboration/Analysis/tables/2_medium_finalcosts_141208.csv"
inpDir3 <- "C:/Claire/GPEM_Postdoc/1_USG_Collaboration/Analysis/tables/3_low_finalcosts_141208.csv"
inpDir4 <- "C:/Claire/GPEM_Postdoc/1_USG_Collaboration/Analysis/tables/4_none_finalcosts_141208.csv"
outDir <- "C:/Claire/GPEM_Postdoc/1_USG_Collaboration/Analysis/figures/dataplots/"

library(vioplot)

data1 <- read.csv(inpDir1, stringsAsFactors=FALSE)
data2 <- read.csv(inpDir2, stringsAsFactors=FALSE)
data3 <- read.csv(inpDir3, stringsAsFactors=FALSE)
data4 <- read.csv(inpDir4, stringsAsFactors=FALSE)

dataAll <- data.frame(rep(c(1:4), each=28), rbind(data1, data2, data3, data4))
names(dataAll)[1]<-"Scenario"
dataAll$RailCostHighM <- dataAll$RailCostHigh/1000000
dataAll$RailCostLowM <- dataAll$RailCostLow/1000000
dataAll$RoadCostM <- dataAll$RoadCost/1000000
dataAll$TotalCostRoad <- rowSums(dataAll[,c("AgriLoss", "TransCost", "RoadCost")])/1000000
dataAll$TotalCostRail <- rowSums(dataAll[,c("AgriLoss", "TransCost", "RailCostLow")])/1000000

altNames <- c("3PortShared", "8PortShared", "3PortUnshare", "8PortUnshare")

sapply(list(data1, data2, data3, data4), nrow)

#############
#Plot biodiversity by scenario
# outPlot <- paste0(outDir, "Biodiversity_violinplot.png")
# png(outPlot)
# vioplot(data1$SumSpLoss, data2$SumSpLoss, data3$SumSpLoss, data4$SumSpLoss, names=altNames, col="darkseagreen1", border="darkseagreen2", rectCol="darkseagreen")
# dev.off()

#################
#Plot costs by scenario
Scenarios <- dataAll[,c("Scenario")]
Costs <- dataAll[,c("SumSpLoss", "AgriLoss", "TransCost", "RoadCostM", "RailCostLowM", "RailCostHighM", "TotalCostRail", "TotalCostRoad", "Len")]
plotcols <- c("darkseagreen1", "wheat1", "mistyrose", "lightsteelblue", "lightskyblue1", "lightskyblue", "", "", "grey70")

boxplotfun <- function(i){
		outPlot <- paste0(outDir, names(Costs)[i], "_boxplot.png")
		currCol <- Costs[,i]
		png(outPlot)
			boxplot(currCol~Scenarios, axes=FALSE, col=plotcols[i], xlab="Scenarios", ylab=names(Costs)[i], notch=TRUE)
			#plot(biodiv, xlab="Scenarios", ylab="Summed Species Loss", pch=16, axes=FALSE)
			axis(1, labels=altNames, at=c(1,2,3,4))
			axis(2)
			box()
		dev.off()
}

sapply(c(1:8), boxplotfun)

#######################
#Plot biodiversity loss against cost
outPlot <- paste0(outDir, "Biodiversity_vs_TotalCost_road.png")
png(outPlot)
	#get range
	xrange <- range(dataAll$TotalCostRoad)
	yrange <- range(dataAll$SumSpLoss)
	#set up plot 
	plot(xrange, yrange, type="n", xlab="Total cost road ($ million)", ylab="Summed Biodiversity Loss")
	mycol<-c("black", "grey70", "slateblue", "red4")
	#linetype<-c(1:4)
	#plotchar<-seq(15, 15+4, 1)
	
	#add lines
	for (i in 1:4){
		currAll <- subset(dataAll, dataAll$Scenario==i)
		currAll <- currAll[order(currAll$SumSpLoss),]
		#points(currAll$TotalCostRoad, currAll$SumSpLoss, col=mycol[i], pch=plotchar[i]) 
		points(currAll$TotalCostRoad, currAll$SumSpLoss, col=mycol[i], pch=16) 
	}	
	# add a legend 
	legend(xrange[1], yrange[2], altNames, cex=0.8, col=mycol,
		pch=16, title="Scenario")

dev.off()

#######################
#Plot cost by mine

#Get data in order
dataAll <- dataAll[order(dataAll$Mine),]
data4Mine <- dataAll[dataAll$Scenario==4, c("Mine", "SumSpLoss", "TotalCostRoad")]
currIndex <- order(data4Mine$TotalCostRoad)
data4Mine <- data4Mine[currIndex,]

data1Mine <- dataAll[dataAll$Scenario==1, c("Mine", "SumSpLoss", "TotalCostRoad")]
data1Mine <- data1Mine[currIndex,]
data2Mine <- dataAll[dataAll$Scenario==2, c("Mine", "SumSpLoss", "TotalCostRoad")]
data2Mine <- data2Mine[currIndex,]
data3Mine <- dataAll[dataAll$Scenario==3, c("Mine", "SumSpLoss", "TotalCostRoad")]
data3Mine <- data3Mine[currIndex,]

dataAllMines <- data.frame(data1Mine, data2Mine, data3Mine, data4Mine)

outPlot <- paste0(outDir, "TotalCost_by_mine_road.png")
png(outPlot)
	#get range
	yrange <- range(dataAll$TotalCostRoad)
	xrange <- range(1:28)
	#set up plot
	plot(xrange, yrange, type="n", ylab="Total cost road ($ million)", xlab="Mine ID")
	mycol<-c("red3", "orchid4", "slateblue", "royalblue3")
	#linetype<-c(1:4)
	#plotchar<-seq(15, 15+4, 1)
	
	#add lines
	for (i in 1:4){
		#points(currAll$TotalCostRoad, currAll$SumSpLoss, col=mycol[i], pch=plotchar[i]) 
		lines(c(1:28), dataAllMines[,i*3], col=mycol[i], pch=16, lty="solid",lwd=1.5) 
	}	
	# add a legend 
	legend(xrange[1], yrange[2], altNames, cex=0.8, col=mycol,
		lty="solid", title="Scenario")

dev.off()

outPlot <- paste0(outDir, "TotalBiodiversityLoss_by_mine_road.png")
png(outPlot)
	#get range
	yrange <- range(dataAll$SumSpLoss)
	xrange <- range(1:28)
	#set up plot
	plot(xrange, yrange, type="n", ylab="Summed Biodiversity Loss", xlab="Mine ID")
	mycol<-c("red3", "orchid4", "slateblue", "black")
	#linetype<-c(1:4)
	#plotchar<-seq(15, 15+4, 1)
	
	#add lines
	for (i in 1:4){
		#points(currAll$TotalCostRoad, currAll$SumSpLoss, col=mycol[i], pch=plotchar[i]) 
		lines(c(1:28), dataAllMines[,(i*3)-1], col=mycol[i], pch=16, lty="solid",lwd=1.5) 
	}	
	# add a legend 
	legend(xrange[1], yrange[2], altNames, cex=0.8, col=mycol,
		lty="solid", title="Scenario")

dev.off()

######################
#Cost vs Biodiversity
sumCosts <- colSums(dataAllMines[,c(3,6,9,12)])
sumBio <- colSums(dataAllMines[,c(2,5,8,11)])/100
mycol2<-c("red2", "orchid3", "slateblue1", "grey70")

outPlot <- paste0(outDir, "TotalCost_vs_biodiversity_road.png")
png(outPlot)

	plot(range(sumCosts), range(sumBio), type="n", ylab="Biodiversity Loss (number of species)", xlab="Total Cost road ($ million)")
	points(sumCosts, sumBio, pch=19, col=mycol)
	legend(range(sumCosts)[1], range(sumBio)[2], altNames, cex=0.8, col=mycol, pch=16, title="Scenario")
dev.off()

#########################
meanCosts <- colMeans(dataAllMines[,c(3,6,9,12)])
meanBio <- colMeans(dataAllMines[,c(2,5,8,11)])/100

outPlot <- paste0(outDir, "TotalCost_vs_biodiversity_mean_road.png")
png(outPlot)

	plot(range(dataAll$TotalCostRoad), range(dataAll$SumSpLoss), type="n", ylab="Biodiversity Loss (number of species)", xlab="Total Cost road ($ million)")
	points(meanCosts, meanBio, pch=19, col=mycol)
	for (i in 1:4){
		points(dataAllMines[,i*3], dataAllMines[,(i*3)-1], pch=3, col=mycol2)
	}
	legend(range(dataAll$TotalCostRoad)[1], range(dataAll$SumSpLoss)[2], altNames, cex=0.8, col=mycol, pch=19, title="Scenario")
dev.off()
##
outPlot <- paste0(outDir, "TotalCost_vs_biodiversity_mean_road_v2.png")
png(outPlot)

	plot(meanCosts, meanBio, type="n", ylab="Biodiversity Loss (number of species)", xlab="Total Cost road ($ million)")
	points(meanCosts, meanBio, pch=19, col=mycol)
	legend("topleft", altNames, cex=0.8, col=mycol, pch=19, title="Scenario")
dev.off()

###################
###################
#######################
#Plot biodiversity loss against cost
outPlot <- paste0(outDir, "Biodiversity_vs_TotalCost_rail.png")
png(outPlot)
	#get range
	xrange <- range(dataAll$TotalCostRail)
	yrange <- range(dataAll$SumSpLoss)
	#set up plot 
	plot(xrange, yrange, type="n", xlab="Total cost rail ($ million)", ylab="Summed Biodiversity Loss")
	mycol<-c("black", "grey70", "slateblue", "red4")
	#linetype<-c(1:4)
	#plotchar<-seq(15, 15+4, 1)
	
	#add lines
	for (i in 1:4){
		currAll <- subset(dataAll, dataAll$Scenario==i)
		currAll <- currAll[order(currAll$SumSpLoss),]
		#points(currAll$TotalCostRail, currAll$SumSpLoss, col=mycol[i], pch=plotchar[i]) 
		points(currAll$TotalCostRail, currAll$SumSpLoss, col=mycol[i], pch=16) 
	}	
	# add a legend 
	legend(xrange[1], yrange[2], altNames, cex=0.8, col=mycol,
		pch=16, title="Scenario")

dev.off()

#######################
#Plot cost by mine

#Get data in order
dataAll <- dataAll[order(dataAll$Mine),]
data4Mine <- dataAll[dataAll$Scenario==4, c("Mine", "SumSpLoss", "TotalCostRail")]
currIndex <- order(data4Mine$TotalCostRail)
data4Mine <- data4Mine[currIndex,]

data1Mine <- dataAll[dataAll$Scenario==1, c("Mine", "SumSpLoss", "TotalCostRail")]
data1Mine <- data1Mine[currIndex,]
data2Mine <- dataAll[dataAll$Scenario==2, c("Mine", "SumSpLoss", "TotalCostRail")]
data2Mine <- data2Mine[currIndex,]
data3Mine <- dataAll[dataAll$Scenario==3, c("Mine", "SumSpLoss", "TotalCostRail")]
data3Mine <- data3Mine[currIndex,]

dataAllMines <- data.frame(data1Mine, data2Mine, data3Mine, data4Mine)

outPlot <- paste0(outDir, "TotalCost_by_mine_rail.png")
png(outPlot)
	#get range
	yrange <- range(dataAll$TotalCostRail)
	xrange <- range(1:28)
	#set up plot
	plot(xrange, yrange, type="n", ylab="Total cost rail ($ million)", xlab="Mine ID")
	mycol<-c("red3", "orchid4", "slateblue", "royalblue3")
	#linetype<-c(1:4)
	#plotchar<-seq(15, 15+4, 1)
	
	#add lines
	for (i in 1:4){
		#points(currAll$TotalCostRail, currAll$SumSpLoss, col=mycol[i], pch=plotchar[i]) 
		lines(c(1:28), dataAllMines[,i*3], col=mycol[i], pch=16, lty="solid",lwd=1.5) 
	}	
	# add a legend 
	legend(xrange[1], yrange[2], altNames, cex=0.8, col=mycol,
		lty="solid", title="Scenario")

dev.off()

outPlot <- paste0(outDir, "TotalBiodiversityLoss_by_mine_rail.png")
png(outPlot)
	#get range
	yrange <- range(dataAll$SumSpLoss)
	xrange <- range(1:28)
	#set up plot
	plot(xrange, yrange, type="n", ylab="Summed Biodiversity Loss", xlab="Mine ID")
	mycol<-c("red3", "orchid4", "slateblue", "black")
	#linetype<-c(1:4)
	#plotchar<-seq(15, 15+4, 1)
	
	#add lines
	for (i in 1:4){
		#points(currAll$TotalCostRail, currAll$SumSpLoss, col=mycol[i], pch=plotchar[i]) 
		lines(c(1:28), dataAllMines[,(i*3)-1], col=mycol[i], pch=16, lty="solid",lwd=1.5) 
	}	
	# add a legend 
	legend(xrange[1], yrange[2], altNames, cex=0.8, col=mycol,
		lty="solid", title="Scenario")

dev.off()

######################
#Cost vs Biodiversity
sumCosts <- colSums(dataAllMines[,c(3,6,9,12)])
sumBio <- colSums(dataAllMines[,c(2,5,8,11)])/100
mycol2<-c("red2", "orchid3", "slateblue1", "grey70")

outPlot <- paste0(outDir, "TotalCost_vs_biodiversity_rail.png")
png(outPlot)

	plot(range(sumCosts), range(sumBio), type="n", ylab="Biodiversity Loss (number of species)", xlab="Total Cost rail ($ million)")
	points(sumCosts, sumBio, pch=19, col=mycol)
	legend(range(sumCosts)[1], range(sumBio)[2], altNames, cex=0.8, col=mycol, pch=16, title="Scenario")
dev.off()

#########################
meanCosts <- colMeans(dataAllMines[,c(3,6,9,12)])
meanBio <- colMeans(dataAllMines[,c(2,5,8,11)])/100

outPlot <- paste0(outDir, "TotalCost_vs_biodiversity_mean_rail.png")
png(outPlot)

	plot(range(dataAll$TotalCostRail), range(dataAll$SumSpLoss), type="n", ylab="Biodiversity Loss (number of species)", xlab="Total Cost rail ($ million)")
	points(meanCosts, meanBio, pch=19, col=mycol)
	for (i in 1:4){
		points(dataAllMines[,i*3], dataAllMines[,(i*3)-1], pch=3, col=mycol2)
	}
	legend(range(dataAll$TotalCostRail)[1], range(dataAll$SumSpLoss)[2], altNames, cex=0.8, col=mycol, pch=19, title="Scenario")
dev.off()
##
outPlot <- paste0(outDir, "TotalCost_vs_biodiversity_mean_rail_v2.png")
png(outPlot)

	plot(meanCosts, meanBio, type="n", ylab="Biodiversity Loss (number of species)", xlab="Total Cost rail ($ million)")
	points(meanCosts, meanBio, pch=19, col=mycol)
	legend("topleft", altNames, cex=0.8, col=mycol, pch=19, title="Scenario")
dev.off()


#######################
#Plot length against cost & biodiversity

colList <- c("TotalCostRoad", "TotalCostRail", "SumSpLoss")
labelList <- c("Total Cost road ($ million)", "Total Cost rail ($ million)", "Biodiversity Loss (percent of a species)")
plotfun2<- function(i){
	outPlot <- paste0(outDir, colList[i], "_vs_length.png")
	png(outPlot)
	x<-dataAll[,c("Scenario", "Len", colList[i])]
	plot(range(dataAll$Len), range(x[,3]), type="n",  ylab=labelList[i], xlab="Infrastructure length (km)")
		#add lines
	for (y in 1:4){
		currAll <- subset(x, x$Scenario==y)
		#points(currAll$TotalCostRail, currAll$SumSpLoss, col=mycol[i], pch=plotchar[i]) 
		points(currAll$Len, currAll[,3], col=mycol[y], pch=16) 
	}
	legend("topleft", altNames, cex=0.8, col=mycol, pch=19, title="Scenario")
	dev.off()
}

sapply(1:3, plotfun2)

#################
#Linear model of biodiversity against length
scen1 <- subset(dataAll, dataAll$Scenario==1)
scen2 <- subset(dataAll, dataAll$Scenario==2)
scen3 <- subset(dataAll, dataAll$Scenario==3)
scen4 <- subset(dataAll, dataAll$Scenario==4)

len1<-scen1$Len
len2<-scen2$Len
len3<-scen3$Len
len4<-scen4$Len

mod1 <- lm(scen1$SumSpLoss~len1)
mod2 <- lm(scen2$SumSpLoss~len2)
mod3 <- lm(scen3$SumSpLoss~len3)
mod4 <- lm(scen4$SumSpLoss~len4)

x.values <- seq(0,640,1)

#95%confidence intervals on slope
ypred1 <- data.frame(predict(mod1, list(len1=x.values),interval = c("confidence"), level = 0.95,type="response"))
ypred2 <- data.frame(predict(mod2, list(len2=x.values),interval = c("confidence"), level = 0.95,type="response"))
ypred3 <- data.frame(predict(mod3, list(len3=x.values),interval = c("confidence"), level = 0.95,type="response"))
ypred4 <- data.frame(predict(mod4, list(len4=x.values),interval = c("confidence"), level = 0.95,type="response"))

mycol<-c("red3", "orchid4", "slateblue", "black")

outPath <- paste0(outDir, "SumSpLoss_vs_Length_lm.png")
png(outPath)
#Set up a blank plot
plot(range(dataAll$Len),range(dataAll$SumSpLoss), xlab="Infrastructure length (km)",ylab="Biodiversity loss (Percent of a species)", type="n")

#Add points
points(scen1$Len, scen1$SumSpLoss, col=mycol[1], pch=3) 
points(scen2$Len, scen2$SumSpLoss, col=mycol[2], pch=3) 
points(scen3$Len, scen3$SumSpLoss, col=mycol[3], pch=3) 
points(scen4$Len, scen4$SumSpLoss, col=mycol[4], pch=3) 

#Add lines
lines(x.values,ypred1$fit, lty='solid', col=mycol[1], lwd=1.2)
lines(x.values,ypred2$fit, lty='solid', col=mycol[2], lwd=1.2)
lines(x.values,ypred3$fit, lty='solid', col=mycol[3], lwd=1.2)
lines(x.values,ypred4$fit, lty='solid', col=mycol[4], lwd=1.2)

legend("topleft", altNames, cex=0.8, col=mycol, pch=3, lty='solid', title="Scenario")

dev.off()

summary(mod1)
summary(mod2)
summary(mod3)
summary(mod4)

######################
#Linear model of biodiversity against cost
len1<-scen1$TotalCostRail
len2<-scen2$TotalCostRail
len3<-scen3$TotalCostRail
len4<-scen4$TotalCostRail

mod1 <- lm(scen1$SumSpLoss~len1)
mod2 <- lm(scen2$SumSpLoss~len2)
mod3 <- lm(scen3$SumSpLoss~len3)
mod4 <- lm(scen4$SumSpLoss~len4)

x.values <- seq(0,330,1)

#95%confidence intervals on slope
ypred1 <- data.frame(predict(mod1, list(len1=x.values),interval = c("confidence"), level = 0.95,type="response"))
ypred2 <- data.frame(predict(mod2, list(len2=x.values),interval = c("confidence"), level = 0.95,type="response"))
ypred3 <- data.frame(predict(mod3, list(len3=x.values),interval = c("confidence"), level = 0.95,type="response"))
ypred4 <- data.frame(predict(mod4, list(len4=x.values),interval = c("confidence"), level = 0.95,type="response"))

mycol<-c("red3", "orchid4", "slateblue", "black")

outPath <- paste0(outDir, "SumSpLoss_vs_TotalCostRail_lm.png")
png(outPath)
#Set up a blank plot
plot(range(dataAll$TotalCostRail),range(dataAll$SumSpLoss), xlab="Total cost rail ($million)",ylab="Biodiversity loss (Percent of a species)", type="n")

#Add points
points(len1, scen1$SumSpLoss, col=mycol[1], pch=3) 
points(len2, scen2$SumSpLoss, col=mycol[2], pch=3) 
points(len3, scen3$SumSpLoss, col=mycol[3], pch=3) 
points(len4, scen4$SumSpLoss, col=mycol[4], pch=3) 

#Add lines
lines(x.values,ypred1$fit, lty='solid', col=mycol[1], lwd=1.2)
lines(x.values,ypred2$fit, lty='solid', col=mycol[2], lwd=1.2)
lines(x.values,ypred3$fit, lty='solid', col=mycol[3], lwd=1.2)
lines(x.values,ypred4$fit, lty='solid', col=mycol[4], lwd=1.2)

legend("topleft", altNames, cex=0.8, col=mycol, pch=3, lty='solid', title="Scenario")

dev.off()

summary(mod1)
summary(mod2)
summary(mod3)
summary(mod4)

######################
#Linear model of biodiversity against cost
len1<-scen1$TotalCostRoad
len2<-scen2$TotalCostRoad
len3<-scen3$TotalCostRoad
len4<-scen4$TotalCostRoad

mod1 <- lm(scen1$SumSpLoss~len1)
mod2 <- lm(scen2$SumSpLoss~len2)
mod3 <- lm(scen3$SumSpLoss~len3)
mod4 <- lm(scen4$SumSpLoss~len4)

x.values <- seq(0,550,1)

#95%confidence intervals on slope
ypred1 <- data.frame(predict(mod1, list(len1=x.values),interval = c("confidence"), level = 0.95,type="response"))
ypred2 <- data.frame(predict(mod2, list(len2=x.values),interval = c("confidence"), level = 0.95,type="response"))
ypred3 <- data.frame(predict(mod3, list(len3=x.values),interval = c("confidence"), level = 0.95,type="response"))
ypred4 <- data.frame(predict(mod4, list(len4=x.values),interval = c("confidence"), level = 0.95,type="response"))

mycol<-c("red3", "orchid4", "slateblue", "black")

outPath <- paste0(outDir, "SumSpLoss_vs_TotalCostRoad_lm.png")
png(outPath)
#Set up a blank plot
plot(range(dataAll$TotalCostRoad),range(dataAll$SumSpLoss), xlab="Total cost road ($million)",ylab="Biodiversity loss (Percent of a species)", type="n")

#Add points
points(len1, scen1$SumSpLoss, col=mycol[1], pch=3) 
points(len2, scen2$SumSpLoss, col=mycol[2], pch=3) 
points(len3, scen3$SumSpLoss, col=mycol[3], pch=3) 
points(len4, scen4$SumSpLoss, col=mycol[4], pch=3) 

#Add lines
lines(x.values,ypred1$fit, lty='solid', col=mycol[1], lwd=1.2)
lines(x.values,ypred2$fit, lty='solid', col=mycol[2], lwd=1.2)
lines(x.values,ypred3$fit, lty='solid', col=mycol[3], lwd=1.2)
lines(x.values,ypred4$fit, lty='solid', col=mycol[4], lwd=1.2)

legend("topleft", altNames, cex=0.8, col=mycol, pch=3, lty='solid', title="Scenario")

dev.off()

summary(mod1)
summary(mod2)
summary(mod3)
summary(mod4)

