inpDir1 <- "C:/Claire/GPEM_Postdoc/1_USG_Collaboration/Analysis/tables/final costs/1_high_finalcosts_141218.csv"
inpDir2 <- "C:/Claire/GPEM_Postdoc/1_USG_Collaboration/Analysis/tables/final costs/2_medium_finalcosts_141218.csv"
inpDir3 <- "C:/Claire/GPEM_Postdoc/1_USG_Collaboration/Analysis/tables/final costs/3_low_finalcosts_141218.csv"
inpDir4 <- "C:/Claire/GPEM_Postdoc/1_USG_Collaboration/Analysis/tables/final costs/4_none_finalcosts_141218.csv"
outDir <- "C:/Claire/GPEM_Postdoc/1_USG_Collaboration/Analysis/figures/dataplots/183Sp_diffuse_rail4m/"

#library(vioplot)

data1 <- read.csv(inpDir1, stringsAsFactors=FALSE)
data2 <- read.csv(inpDir2, stringsAsFactors=FALSE)
data3 <- read.csv(inpDir3, stringsAsFactors=FALSE)
data4 <- read.csv(inpDir4, stringsAsFactors=FALSE)

dataAll <- data.frame(rep(c(1:4), each=28), rbind(data1, data2, data3, data4))
names(dataAll)[1]<-"Scenario"
dataAll$RailCostHighM <- dataAll$RailCostHigh/1000000
dataAll$RailCostExtraHighM <- dataAll$RailCostExtraHigh/1000000
dataAll$RoadCostM <- dataAll$RoadCost/1000000
dataAll$TotalCostRoad <- rowSums(dataAll[,c("AgriLoss", "TransCost", "RoadCost")])/1000000
dataAll$TotalCostRail <- rowSums(dataAll[,c("AgriLoss", "TransCost", "RailCostExtraHigh")])/1000000
dataAll$SumSpLoss183SpDiffHomog <- dataAll$Area*0.2
altNames <- c("3PortShared", "8PortShared", "3PortUnshare", "8PortUnshare")

sapply(list(data1, data2, data3, data4), nrow)

mycols <- rainbow(4)

###################################
###################################
#SET UP VARIABLES
#biodiversity benefit from collaboration
bioBen3port <- (dataAll[dataAll$Scenario==3, "SumSpLoss183SpDiffuse"] - dataAll[dataAll$Scenario==1, "SumSpLoss183SpDiffuse"])/100
bioBen3portHomog <- (dataAll[dataAll$Scenario==3, "SumSpLoss183SpDiffHomog"] - dataAll[dataAll$Scenario==1, "SumSpLoss183SpDiffHomog"])/100
bioBen3portdirect <- (dataAll[dataAll$Scenario==3, "SumSpLoss183Sp"] - dataAll[dataAll$Scenario==1, "SumSpLoss183Sp"])/100
bioBen8port <- (dataAll[dataAll$Scenario==4, "SumSpLoss183SpDiffuse"] - dataAll[dataAll$Scenario==2, "SumSpLoss183SpDiffuse"])/100
#biodiversity benefit as % of total
bioBen3portPer <- bioBen3port/dataAll[dataAll$Scenario==3, "SumSpLoss183SpDiffuse"]*100*100
bioBen3portHomogPer <- bioBen3portHomog/dataAll[dataAll$Scenario==3, "SumSpLoss183SpDiffHomog"]*100*100
#financial benefit from collaboration
finBen3port <- (dataAll[dataAll$Scenario==3, "TotalCostRail"] - dataAll[dataAll$Scenario==1, "TotalCostRail"])/1000
finBen8port <- (dataAll[dataAll$Scenario==4, "TotalCostRail"] - dataAll[dataAll$Scenario==2, "TotalCostRail"])/1000
#financial benefit as % of total cost
finBen3portPer <- finBen3port/dataAll[dataAll$Scenario==3, "TotalCostRail"]*100*1000
finBen8portPer <- finBen8port/dataAll[dataAll$Scenario==4, "TotalCostRail"]*100*1000
#number of mines collaborating
numMine3port <- dataAll[dataAll$Scenario==1, "NumCollab"]
numMine3portF <- factor(numMine3port, levels=as.character(5:16))
numMine8port <- dataAll[dataAll$Scenario==2, "NumCollab"]
#normalised biodiversity impact
bioNorm3port3 <- dataAll[dataAll$Scenario==3, "SumSpLoss183SpDiffuse"]/dataAll[dataAll$Scenario==3, "Len"]
bioNorm3port1<- dataAll[dataAll$Scenario==1, "SumSpLoss183SpDiffuse"]/dataAll[dataAll$Scenario==1, "Len"]
bioNorm8port4 <- dataAll[dataAll$Scenario==4, "SumSpLoss183SpDiffuse"]/dataAll[dataAll$Scenario==4, "Len"]
bioNorm8port2 <- dataAll[dataAll$Scenario==2, "SumSpLoss183SpDiffuse"]/dataAll[dataAll$Scenario==2, "Len"]
finNorm3port <- finBen3port/dataAll[dataAll$Scenario==3, "Len"]

#######################
#FINAL PLOTS
#######################
#PANNELL PLOT (Then edited in Illustrator)
bioplotfun <- function(x,y,z,labx,laby){
	outPlot <- paste0(outDir, z)
	postscript(outPlot, width=10, height=10)
	#png(outPlot)
		#plot(range(x), range(y), type="n", ylab=laby, xlab=labx)
		plot(c(0,100), range(y), type="n", ylab=laby, xlab=labx)
		points(x, y, pch=16, col='black')
	dev.off()
}

xlabs <- c("Averted cost ($ billion)", "Averted cost (% of total)", "Mine ID", "Averted cost/length", "Length (km)")
ylabs <- c("Averted impact (species equivalents)", "Normalised impact (bio/length)", "Averted impact (% of total)", "Impact (species equivalents)")

bioplotfun(finBen3portPer, bioBen3port,"Benefit_Bioversity_vs_CostPercent_3port.png", xlabs[2], ylabs[1])
bioplotfun(finBen3portPer, bioBen3port,"Benefit_Bioversity_vs_CostPercent_3port.eps", xlabs[2], ylabs[1]) 

######################
#plot benefit against number of mines collaborating
ylabs <- c("Averted cost ($ billion)", "Averted cost (% of total)", "Averted impact (species equivalents)", "Averted impact (% of total)")

collabplotfun <- function(x,y,z,laby){
	outPlot <- paste0(outDir, z)
	png(outPlot)
	boxplot(y~x, ylab=laby, xlab="Number of collaboration partners")
	dev.off()
}
collabplotfun(numMine3portF, finBen3portPer, "Number_collab_vs_Cost_3port_boxplot.png", ylabs[2])
collabplotfun(numMine3portF, bioBen3port, "Number_collab_vs_Bio_3port_boxplot.png", ylabs[3])
collabplotfun(numMine3portF, bioBen3portPer, "Number_collab_vs_BioPer_3port_boxplot.png", ylabs[4])
collabplotfun(numMine3portF, bioBen3portHomogPer, "Number_collab_vs_BioHomogPer_3port_boxplot.png", ylabs[4])

###########################
#Length vs biodiversity impact
outPlot <- paste0(outDir, "BioversityHeteroHomog_vs_Length_3portunshared.png")
png(outPlot)
	plot(range(dataAll[dataAll$Scenario==3, "Len"]), range(dataAll[dataAll$Scenario==3, "SumSpLoss183SpDiffuse"]/100), type="n", ylab="Impact (species equivalents)", xlab="Length (km)")
	points(dataAll[dataAll$Scenario==3, "Len"], dataAll[dataAll$Scenario==3, "SumSpLoss183SpDiffuse"]/100, pch=16, col='black')
	points(dataAll[dataAll$Scenario==3, "Len"], dataAll[dataAll$Scenario==3, "SumSpLoss183SpDiffHomog"]/100, pch=16, col='grey70')
	legend('topleft', c("Heterogenous", "Homogenous"), cex=0.8, col=c("black", "grey70"), pch=16, bty='n')
dev.off()

#############################
#Cost vs impact (for incentives plot)
outPlot <- paste0(outDir, "Benefit_BioversityHeteroHomogPer_vs_CostPercent_3port.png")
png(outPlot)
	plot(c(20,100), range(bioBen3portPer), type="n", ylab="Averted impact (% of total)", xlab="Averted cost (% of total)")
	points(finBen3portPer, bioBen3portPer, pch=16, col="black")
	points(finBen3portPer, bioBen3portHomogPer, pch=16, col="grey70")
	legend('topleft', c("Heterogenous", "Homogenous"), cex=0.8, col=c("black", "grey70"), pch=16, bty='n')
dev.off()

outPlot <- paste0(outDir, "Benefit_BioversityHeteroHomog_vs_CostPercent_3port.png")
png(outPlot)
	plot(c(20,100), range(bioBen3port), type="n", ylab="Averted impact (species equivalents)", xlab="Averted cost (% of total)")
	points(finBen3portPer, bioBen3port, pch=16, col="black")
	points(finBen3portPer, bioBen3portHomog, pch=16, col="grey70")
	legend('topright', c("Heterogenous", "Homogenous"), cex=0.8, col=c("black", "grey70"), pch=16, bty='n')
dev.off()



#################################
#################################
#plot benefit against number of mines collaborating
#Add lines of best fit
collabplotfun <- function(x1,y1, y2,z,laby){
	x2<-x+0.1 #jitter x values for second plot
	outPlot <- paste0(outDir, z)
	print(summary(lm(y1~x)))
	print(summary(lm(y2~x)))
	postscript(outPlot, paper="special", horizontal=FALSE,width=9, height=10)
	#png(outPlot)
		plot(range(x), c(0,100), type="n", ylab=laby, xlab="Number of collaboration partners")
		points(x, y1, pch=16, col='black')
		abline(lm(y1~x), col='black')
		points(x2, y2, pch=16, col='grey70')
		abline(lm(y2~x), col='grey70')
		legend('topright', c("Cost", "Biodiversity"), cex=0.8, col=c("black", "grey70"), pch=16, bty='n')
	dev.off()
}

ylabs <- c("Averted cost ($ billion)", "Averted cost (% of total)", "Averted impact (species equivalents)", "Averted impact (% of total)")

collabplotfun(numMine3port, finBen3portPer, bioBen3portPer, "Number_collab_vs_CostandBio_3port.png", ylabs[2])
collabplotfun(numMine3port, finBen3portPer, bioBen3portPer, "Number_collab_vs_CostandBio_3port.eps", ylabs[2])

#Minimise sum of least squares
collabplotfunNLS <- function(x,y,z,laby){
	outPlot <- paste0(outDir, z)
	#some starting values
	a1 <- 0.5
	a2 <- 700
	a3 <- 1
	fit <- nls(y ~ 1/(a1 + a2*(exp(-x))), start=list(a1=a1, a2=a2))
	#draw the fit on the plot
	new <- data.frame(x=seq(min(x), len=200))
	png(outPlot)
	#postscript(outPlot)
		plot(range(x), range(y), type="n", ylab=laby, xlab="Number of collaboration partners")
		points(x, y, pch=16, col='black')
		## adding the actual model
		lines(new$x, predict(fit, newdata=new))  
	dev.off()
}

collabplotfunLM <- function(x,y,z,laby){
	outPlot <- paste0(outDir, z)
	#linear model
	mod1 <-lm(y~log2(x))
	print(summary(mod1))
	x.values <- seq(0,16, 0.01)
	#95%confidence intervals on slope
	ypred <- data.frame(predict(mod1, list(x=x.values),interval = c("confidence"),level = 0.95,type="response"))
	#Plot
	png(outPlot)
	#postscript(outPlot)
		plot(range(x), range(y), type="n", ylab=laby, xlab="Number of collaboration partners")
		points(x, y, pch=16, col='black')
		## adding the actual model
		lines(x.values,ypred$fit, lty='solid', col='black', lwd=1.2)  
		## Adding the 5 and 95% confidence intervals
		lines(x.values,ypred$lwr,lty=1, col='grey80')  
		lines(x.values,ypred$upr,lty=1, col='grey80')
	dev.off()
}

collabplotfunLM(numMine3port, finBen3port, "Number_collab_vs_Cost_3port_logfit.png", ylabs[1])
collabplotfunLM(numMine3port, finBen3port, "Number_collab_vs_Cost_3port_logfit.eps", ylabs[1])
collabplotfunNLS(numMine3port, finBen3port, "Number_collab_vs_Cost_3port_inverseexp.png", ylabs[1])
collabplotfunNLS(numMine3port, finBen3port, "Number_collab_vs_Cost_3port_inverseexp.eps", ylabs[1])




######################
#OTHER PLOTS
######################
#MAKE PLOTS
#plot bio benefit vs financial benefit (ie reduction in cost vs reduction in species impact)
#with each datapoint as the mine ID
#as both straight benefit and as percent benefit

bioplotfun <- function(x,y,z,labx,laby){
	outPlot <- paste0(outDir, z)
	postscript(outPlot, width=10, height=10)
	#png(outPlot)
		#plot(range(x), range(y), type="n", ylab=laby, xlab=labx)
		plot(c(0,100), range(y), type="n", ylab=laby, xlab=labx)
		#plot(c(20,100), c(0,0.55), type="n", ylab=laby, xlab=labx)
		#plot(range(x), range(y), type="n", ylab=laby, xlab=labx, ylim=c(0,100), xlim=c(0,100))
		points(x, y, pch=16, col='black')
		#points(finBen3portPer, bioBen3portdirect, pch=16, col='grey70')
		#points(x, y, pch=16, col= ifelse(y > 0.2, "green", ifelse(y <= 0.1,"black", "blue")))
		#text(x,y, label=as.character(1:28), col='black', cex=0.8)
		#legend('topright', c("Diffuse", "Direct"), cex=0.8, col=c("black", "grey70"), pch=16, title="Impacts")
		#abline(0,1)
	dev.off()
}

xlabs <- c("Averted cost ($ billion)", "Averted cost (% of total)", "Mine ID", "Averted cost/length", "Length (km)")
ylabs <- c("Averted impact (species equivalents)", "Normalised impact (bio/length)", "Averted impact (% of total)", "Impact (species equivalents)")

bioplotfun(finBen3port, bioBen3port,"Benefit_Bioversity_vs_Cost_3port.png", xlabs[1], ylabs[1])
bioplotfun(finBen3portPer, bioBen3port,"Benefit_BioversityDirect_vs_CostPercent_3port.png", xlabs[2], ylabs[1])
#bioplotfun(finBen8port, bioBen8port,"Benefit_Bioversity_vs_Cost_8port.png", xlabs[1], ylabs[1])
bioplotfun(finBen3portPer, bioBen3port,"Benefit_Bioversity_vs_CostPercent_3port.png", xlabs[2], ylabs[1])
bioplotfun(finBen3portPer, bioBen3port,"Benefit_Bioversity_vs_CostPercent_3port.eps", xlabs[2], ylabs[1]) 
#bioplotfun(finBen8portPer, bioBen8port,"Benefit_Bioversity_vs_CostPercent_8port.png", xlabs[2], ylabs[1])
bioplotfun(finBen3portPer, bioBen3portPer,"Benefit_BioversityPercent_vs_CostPercent_3port.png", xlabs[2], ylabs[3])
bioplotfun(dataAll[dataAll$Scenario==3, "Len"], dataAll[dataAll$Scenario==3, "SumSpLoss183SpDiffuse"]/100,"Bioversity_vs_Length_3portunshared.png", xlabs[5], ylabs[4])

#EVEN MORE PLOTS
bioplotfun(c(1:28), bioNorm3port1,"Normalised_Bioversity_by_Mine_3portScen1.png", xlabs[3], ylabs[2])
bioplotfun(c(1:28), bioNorm3port3,"Normalised_Bioversity_by_Mine_3portScen3.png", xlabs[3], ylabs[2])
#bioplotfun(c(1:28), bioNorm8port2,"Normalised_Bioversity_by_Mine_8portScen2.png", xlabs[3], ylabs[2])
#bioplotfun(c(1:28), bioNorm8port4,"Normalised_Bioversity_by_Mine_8portScen4.png", xlabs[3], ylabs[2])
#plot them all on one plot
#bioNormAll <- append(bioNorm3port1,bioNorm8port2,bioNorm3port3,bioNorm8port4)
mycols <- c("black", "grey70")  
bioNormAll <- append(bioNorm3port1,bioNorm3port3)  
bioplotfunall <- function(x,y,z,labx,laby){
	outPlot <- paste0(outDir, z)
	png(outPlot)
		plot(range(x), range(y), type="n", ylab=laby, xlab=labx)
		points(x, y[1:28], pch=16, col=mycols[1])
		points(x, y[29:56], pch=16, col=mycols[2])
		#points(x, y[57:84], pch=16, col=mycols[3])
		#points(x, y[85:112], pch=16, col=mycols[4])
		#text(x,y, label=as.character(1:28), col='black', cex=0.8)
		legend('topright', altNames[c(1,3)], cex=0.8, col=mycols, pch=16, title="Scenario")
	dev.off()
}
bioplotfunall(c(1:28), bioNormAll,"Normalised_Bioversity_by_Mine_All.png", xlabs[3], ylabs[2])
bioplotfunall(finBen3port, bioNormAll,"Normalised_Bioversity_by_Cost_All.png", xlabs[1], ylabs[2])
bioplotfunall(finBen3portPer, bioNormAll,"Normalised_Bioversity_by_CostPercent_All.png", xlabs[2], ylabs[2])
bioplotfunall(finNorm3port, bioNormAll,"Normalised_Bioversity_by_CostNorm_All.png", xlabs[4], ylabs[2])


#MAKE MORE PLOTS
#plot benefit against number of mines collaborating
collabplotfun <- function(x,y,z,laby){
	outPlot <- paste0(outDir, z)
	png(outPlot)
		plot(range(x), range(y), type="n", ylab=laby, xlab="Number of collaboration partners")
		points(x, y, pch=16, col='black')
		abline(lm(y~x))
		#text(x,y, label=as.character(1:28), col='black', cex=0.8)
	dev.off()
}

ylabs <- c("Averted cost ($ billion)", "Averted cost (% of total)", "Averted impact (species equivalents)", "Averted impact (% of total)")

collabplotfun(numMine3port, finBen3port, "Number_collab_vs_Cost_3port.png", ylabs[1])
#collabplotfun(numMine8port, finBen8port, "Number_collab_vs_Cost_8port.png", ylabs[1])
collabplotfun(numMine3port, finBen3portPer, "Number_collab_vs_CostPercent_3port.png", ylabs[2])
#collabplotfun(numMine8port, finBen8portPer, "Number_collab_vs_CostPercent_8port.png", ylabs[2])
collabplotfun(numMine3port, bioBen3port, "Number_collab_vs_BioBen_3port.png", ylabs[3])
collabplotfun(numMine3port, bioBen3portPer, "Number_collab_vs_BioBenPercent_3port.png", ylabs[4])
#collabplotfun(numMine8port, bioBen8port, "Number_collab_vs_BioBen_8port.png", ylabs[3])

##################
#Add lines of best fit
#Minimise sum of least squares

#some starting values
a1 <- 0.5
a2 <- 700
a3 <- 1
#draw the fit on the plot
#new <- data.frame(x=seq(min(x), len=200))
#lines(new$x, predict(fit, newdata=new))

collabplotfunLM <- function(x,y,z,laby){
	outPlot <- paste0(outDir, z)
	
	#linear model
	mod1 <-lm(y~log2(x))
	#mod1 <- nls(y ~ 1/(a1 + a2*(exp(-x))), start=list(a1=a1, a2=a2))
	summary(mod1)
	x.values <- seq(0,16, 0.01)
	#standard deviations
	#ypred <- predict(mod1, list(x=x.values),se.fit=TRUE)
	#95%confidence intervals on slope
	ypred <- data.frame(predict(mod1, list(x=x.values),interval = c("confidence"),level = 0.95,type="response"))
	#95% confidence intervals for mean
	#yCI <- confint(mod1, 'x', level=0.95)

	#Plot
	png(outPlot)
		plot(range(x), range(y), type="n", ylab=laby, xlab="Number of collaboration partners", box=FALSE)
		points(x, y, pch=16, col='black')
		## adding the actual model
		lines(x.values,ypred$fit, lty='solid', col='black', lwd=1.2)  
		## Adding the 5 and 95% confidence intervals
		lines(x.values,ypred$lwr,lty=1, col='grey80')  
		lines(x.values,ypred$upr,lty=1, col='grey80')
	dev.off()
}

collabplotfunLM(numMine3port, finBen3port, "Number_collab_vs_Cost_3port_logfit.png", ylabs[1])




#plot bio&financial benefit vs collab transaction cost (ie # mines sharing)



#############
#Plot biodiversity by scenario
# outPlot <- paste0(outDir, "Biodiversity_violinplot.png")
# png(outPlot)
# vioplot(data1$SumSpLoss, data2$SumSpLoss, data3$SumSpLoss, data4$SumSpLoss, names=altNames, col="darkseagreen1", border="darkseagreen2", rectCol="darkseagreen")
# dev.off()

#################
#Plot costs by scenario
Scenarios <- dataAll[,c("Scenario")]
Costs <- dataAll[,c("SumSpLoss183SpDiffuse", "AgriLoss", "TransCost", "RoadCostM", "RailCostHighM", "RailCostExtraHighM", "TotalCostRail", "TotalCostRoad", "Len")]
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
outPlot <- paste0(outDir, "Biodiversity_vs_TotalCost_road_diffuse.png")
png(outPlot)
	#get range
	xrange <- range(dataAll$TotalCostRoad)
	yrange <- range(dataAll$SumSpLoss183SpDiffuse)
	#set up plot 
	plot(xrange, yrange, type="n", xlab="Total cost road ($ million)", ylab="Summed Biodiversity Loss")
	mycol<-c("black", "grey70", "slateblue", "red4")
	#linetype<-c(1:4)
	#plotchar<-seq(15, 15+4, 1)
	
	#add lines
	for (i in 1:4){
		currAll <- subset(dataAll, dataAll$Scenario==i)
		currAll <- currAll[order(currAll$SumSpLoss183SpDiffuse),]
		#points(currAll$TotalCostRoad, currAll$SumSpLoss183SpDiffuse, col=mycol[i], pch=plotchar[i]) 
		points(currAll$TotalCostRoad, currAll$SumSpLoss183SpDiffuse, col=mycol[i], pch=16) 
	}	
	# add a legend 
	legend(xrange[1], yrange[2], altNames, cex=0.8, col=mycol,
		pch=16, title="Scenario")

dev.off()

#######################
#Plot cost by mine

#Get data in order
dataAll <- dataAll[order(dataAll$Mine),]
data4Mine <- dataAll[dataAll$Scenario==4, c("Mine", "SumSpLoss183SpDiffuse", "TotalCostRoad")]
currIndex <- order(data4Mine$TotalCostRoad)
data4Mine <- data4Mine[currIndex,]

data1Mine <- dataAll[dataAll$Scenario==1, c("Mine", "SumSpLoss183SpDiffuse", "TotalCostRoad")]
data1Mine <- data1Mine[currIndex,]
data2Mine <- dataAll[dataAll$Scenario==2, c("Mine", "SumSpLoss183SpDiffuse", "TotalCostRoad")]
data2Mine <- data2Mine[currIndex,]
data3Mine <- dataAll[dataAll$Scenario==3, c("Mine", "SumSpLoss183SpDiffuse", "TotalCostRoad")]
data3Mine <- data3Mine[currIndex,]

dataAllMines <- data.frame(data1Mine, data2Mine, data3Mine, data4Mine)

outPlot <- paste0(outDir, "TotalCost_by_mine_road_diffuse.png")
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
		#points(currAll$TotalCostRoad, currAll$SumSpLoss183SpDiffuse col=mycol[i], pch=plotchar[i]) 
		lines(c(1:28), dataAllMines[,i*3], col=mycol[i], pch=16, lty="solid",lwd=1.5) 
	}	
	# add a legend 
	legend(xrange[1], yrange[2], altNames, cex=0.8, col=mycol,
		lty="solid", title="Scenario")

dev.off()

outPlot <- paste0(outDir, "TotalBiodiversityLoss_by_mine_road.png")
png(outPlot)
	#get range
	yrange <- range(dataAll$SumSpLoss183SpDiffuse)
	xrange <- range(1:28)
	#set up plot
	plot(xrange, yrange, type="n", ylab="Summed Biodiversity Loss", xlab="Mine ID")
	mycol<-c("red3", "orchid4", "slateblue", "black")
	#linetype<-c(1:4)
	#plotchar<-seq(15, 15+4, 1)
	
	#add lines
	for (i in 1:4){
		#points(currAll$TotalCostRoad, currAll$SumSpLoss183SpDiffuse, col=mycol[i], pch=plotchar[i]) 
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

outPlot <- paste0(outDir, "TotalCost_vs_biodiversity_road_diffuse.png")
png(outPlot)

	plot(range(sumCosts), range(sumBio), type="n", ylab="Biodiversity Loss (number of species)", xlab="Total Cost road ($ million)")
	points(sumCosts, sumBio, pch=19, col=mycol)
	legend(range(sumCosts)[1], range(sumBio)[2], altNames, cex=0.8, col=mycol, pch=16, title="Scenario")
dev.off()

#########################
meanCosts <- colMeans(dataAllMines[,c(3,6,9,12)])
meanBio <- colMeans(dataAllMines[,c(2,5,8,11)])/100

outPlot <- paste0(outDir, "TotalCost_vs_biodiversity_mean_road_diffuse.png")
png(outPlot)

	plot(range(dataAll$TotalCostRoad), range(dataAll$SumSpLoss183SpDiffuse), type="n", ylab="Biodiversity Loss (number of species)", xlab="Total Cost road ($ million)")
	points(meanCosts, meanBio, pch=19, col=mycol)
	for (i in 1:4){
		points(dataAllMines[,i*3], dataAllMines[,(i*3)-1], pch=3, col=mycol2)
	}
	legend(range(dataAll$TotalCostRoad)[1], range(dataAll$SumSpLoss183SpDiffuse)[2], altNames, cex=0.8, col=mycol, pch=19, title="Scenario")
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
	yrange <- range(dataAll$SumSpLoss183SpDiffuse)
	#set up plot 
	plot(xrange, yrange, type="n", xlab="Total cost rail ($ million)", ylab="Summed Biodiversity Loss")
	mycol<-c("black", "grey70", "slateblue", "red4")
	#linetype<-c(1:4)
	#plotchar<-seq(15, 15+4, 1)
	
	#add lines
	for (i in 1:4){
		currAll <- subset(dataAll, dataAll$Scenario==i)
		currAll <- currAll[order(currAll$SumSpLoss183SpDiffuse),]
		#points(currAll$TotalCostRail, currAll$SumSpLoss183SpDiffuse, col=mycol[i], pch=plotchar[i]) 
		points(currAll$TotalCostRail, currAll$SumSpLoss183SpDiffuse, col=mycol[i], pch=16) 
	}	
	# add a legend 
	legend(xrange[1], yrange[2], altNames, cex=0.8, col=mycol,
		pch=16, title="Scenario")

dev.off()

#######################
#Plot cost by mine

#Get data in order
dataAll <- dataAll[order(dataAll$Mine),]
data4Mine <- dataAll[dataAll$Scenario==4, c("Mine", "SumSpLoss183SpDiffuse", "TotalCostRail")]
currIndex <- order(data4Mine$TotalCostRail)
data4Mine <- data4Mine[currIndex,]
data1Mine <- dataAll[dataAll$Scenario==1, c("Mine", "SumSpLoss183SpDiffuse", "TotalCostRail")]
data1Mine <- data1Mine[currIndex,]
data2Mine <- dataAll[dataAll$Scenario==2, c("Mine", "SumSpLoss183SpDiffuse", "TotalCostRail")]
data2Mine <- data2Mine[currIndex,]
data3Mine <- dataAll[dataAll$Scenario==3, c("Mine", "SumSpLoss183SpDiffuse", "TotalCostRail")]
data3Mine <- data3Mine[currIndex,]

dataAllMines <- data.frame(data1Mine, data2Mine, data3Mine, data4Mine)

outPlot <- paste0(outDir, "TotalCost_by_mine_rail_diffuse.png")
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
		#points(currAll$TotalCostRail, currAll$SumSpLoss183SpDiffuse, col=mycol[i], pch=plotchar[i]) 
		lines(c(1:28), dataAllMines[,i*3], col=mycol[i], pch=16, lty="solid",lwd=1.5) 
	}	
	# add a legend 
	legend(xrange[1], yrange[2], altNames, cex=0.8, col=mycol,
		lty="solid", title="Scenario")

dev.off()

outPlot <- paste0(outDir, "TotalBiodiversityLoss_by_mine_rail_diffuse.png")
png(outPlot)
	#get range
	yrange <- range(dataAll$SumSpLoss183SpDiffuse)
	xrange <- range(1:28)
	#set up plot
	plot(xrange, yrange, type="n", ylab="Summed Biodiversity Loss", xlab="Mine ID")
	mycol<-c("red3", "orchid4", "slateblue", "black")
	#linetype<-c(1:4)
	#plotchar<-seq(15, 15+4, 1)
	
	#add lines
	for (i in 1:4){
		#points(currAll$TotalCostRail, currAll$SumSpLoss183SpDiffuse, col=mycol[i], pch=plotchar[i]) 
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

outPlot <- paste0(outDir, "TotalCost_vs_biodiversity_rail_diffuse.png")
png(outPlot)

	plot(range(sumCosts), range(sumBio), type="n", ylab="Biodiversity Loss (number of species)", xlab="Total Cost rail ($ million)")
	points(sumCosts, sumBio, pch=19, col=mycol)
	legend(range(sumCosts)[1], range(sumBio)[2], altNames, cex=0.8, col=mycol, pch=16, title="Scenario")
dev.off()

#########################
meanCosts <- colMeans(dataAllMines[,c(3,6,9,12)])
meanBio <- colMeans(dataAllMines[,c(2,5,8,11)])/100

outPlot <- paste0(outDir, "TotalCost_vs_biodiversity_mean_rail_diffuse.png")
png(outPlot)

	plot(range(dataAll$TotalCostRail), range(dataAll$SumSpLoss183SpDiffuse), type="n", ylab="Biodiversity Loss (number of species)", xlab="Total Cost rail ($ million)")
	points(meanCosts, meanBio, pch=19, col=mycol)
	for (i in 1:4){
		points(dataAllMines[,i*3], dataAllMines[,(i*3)-1], pch=3, col=mycol2)
	}
	legend(range(dataAll$TotalCostRail)[1], range(dataAll$SumSpLoss183SpDiffuse)[2], altNames, cex=0.8, col=mycol, pch=19, title="Scenario")
dev.off()
##
outPlot <- paste0(outDir, "TotalCost_vs_biodiversity_mean_rail_diffuse_v2.png")
png(outPlot)

	plot(meanCosts, meanBio, type="n", ylab="Biodiversity Loss (number of species)", xlab="Total Cost rail ($ million)")
	points(meanCosts, meanBio, pch=19, col=mycol)
	legend("topleft", altNames, cex=0.8, col=mycol, pch=19, title="Scenario")
dev.off()


#######################
#Plot length against cost & biodiversity

colList <- c("TotalCostRoad", "TotalCostRail", "SumSpLoss183SpDiffuse")
labelList <- c("Total Cost road ($ million)", "Total Cost rail ($ million)", "Biodiversity Loss (percent of a species)")
plotfun2<- function(i){
	outPlot <- paste0(outDir, colList[i], "_vs_length_diffuse.png")
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

mod1 <- lm(scen1$SumSpLoss183SpDiffuse~len1)
mod2 <- lm(scen2$SumSpLoss183SpDiffuse~len2)
mod3 <- lm(scen3$SumSpLoss183SpDiffuse~len3)
mod4 <- lm(scen4$SumSpLoss183SpDiffuse~len4)

x.values <- seq(0,640,1)

#95%confidence intervals on slope
ypred1 <- data.frame(predict(mod1, list(len1=x.values),interval = c("confidence"), level = 0.95,type="response"))
ypred2 <- data.frame(predict(mod2, list(len2=x.values),interval = c("confidence"), level = 0.95,type="response"))
ypred3 <- data.frame(predict(mod3, list(len3=x.values),interval = c("confidence"), level = 0.95,type="response"))
ypred4 <- data.frame(predict(mod4, list(len4=x.values),interval = c("confidence"), level = 0.95,type="response"))

mycol<-c("red3", "orchid4", "slateblue", "black")

outPath <- paste0(outDir, "SumSpLossDiffuse_vs_Length_lm.png")
png(outPath)
#Set up a blank plot
plot(range(dataAll$Len),range(dataAll$SumSpLoss183SpDiffuse), xlab="Infrastructure length (km)",ylab="Biodiversity loss (Percent of a species)", type="n")

#Add points
points(scen1$Len, scen1$SumSpLoss183SpDiffuse, col=mycol[1], pch=3) 
points(scen2$Len, scen2$SumSpLoss183SpDiffuse, col=mycol[2], pch=3) 
points(scen3$Len, scen3$SumSpLoss183SpDiffuse, col=mycol[3], pch=3) 
points(scen4$Len, scen4$SumSpLoss183SpDiffuse, col=mycol[4], pch=3) 

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

mod1 <- lm(scen1$SumSpLoss183SpDiffuse~len1)
mod2 <- lm(scen2$SumSpLoss183SpDiffuse~len2)
mod3 <- lm(scen3$SumSpLoss183SpDiffuse~len3)
mod4 <- lm(scen4$SumSpLoss183SpDiffuse~len4)

x.values <- seq(0,330,1)

#95%confidence intervals on slope
ypred1 <- data.frame(predict(mod1, list(len1=x.values),interval = c("confidence"), level = 0.95,type="response"))
ypred2 <- data.frame(predict(mod2, list(len2=x.values),interval = c("confidence"), level = 0.95,type="response"))
ypred3 <- data.frame(predict(mod3, list(len3=x.values),interval = c("confidence"), level = 0.95,type="response"))
ypred4 <- data.frame(predict(mod4, list(len4=x.values),interval = c("confidence"), level = 0.95,type="response"))

mycol<-c("red3", "orchid4", "slateblue", "black")

outPath <- paste0(outDir, "SumSpLossDiffuse_vs_TotalCostRail_lm.png")
png(outPath)
#Set up a blank plot
plot(range(dataAll$TotalCostRail),range(dataAll$SumSpLoss183SpDiffuse), xlab="Total cost rail ($million)",ylab="Biodiversity loss (Percent of a species)", type="n")

#Add points
points(len1, scen1$SumSpLoss183SpDiffuse, col=mycol[1], pch=3) 
points(len2, scen2$SumSpLoss183SpDiffuse, col=mycol[2], pch=3) 
points(len3, scen3$SumSpLoss183SpDiffuse, col=mycol[3], pch=3) 
points(len4, scen4$SumSpLoss183SpDiffuse, col=mycol[4], pch=3) 

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

mod1 <- lm(scen1$SumSpLoss183SpDiffuse~len1)
mod2 <- lm(scen2$SumSpLoss183SpDiffuse~len2)
mod3 <- lm(scen3$SumSpLoss183SpDiffuse~len3)
mod4 <- lm(scen4$SumSpLoss183SpDiffuse~len4)

x.values <- seq(0,550,1)

#95%confidence intervals on slope
ypred1 <- data.frame(predict(mod1, list(len1=x.values),interval = c("confidence"), level = 0.95,type="response"))
ypred2 <- data.frame(predict(mod2, list(len2=x.values),interval = c("confidence"), level = 0.95,type="response"))
ypred3 <- data.frame(predict(mod3, list(len3=x.values),interval = c("confidence"), level = 0.95,type="response"))
ypred4 <- data.frame(predict(mod4, list(len4=x.values),interval = c("confidence"), level = 0.95,type="response"))

mycol<-c("red3", "orchid4", "slateblue", "black")

outPath <- paste0(outDir, "SumSpLossDiffuse_vs_TotalCostRoad_lm.png")
png(outPath)
#Set up a blank plot
plot(range(dataAll$TotalCostRoad),range(dataAll$SumSpLoss183SpDiffuse), xlab="Total cost road ($million)",ylab="Biodiversity loss (Percent of a species)", type="n")

#Add points
points(len1, scen1$SumSpLoss183SpDiffuse, col=mycol[1], pch=3) 
points(len2, scen2$SumSpLoss183SpDiffuse, col=mycol[2], pch=3) 
points(len3, scen3$SumSpLoss183SpDiffuse, col=mycol[3], pch=3) 
points(len4, scen4$SumSpLoss183SpDiffuse, col=mycol[4], pch=3) 

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

####################
##Barplots
dat<-rbind(finBen3port, bioBen3port)
dat<-dat[,order(finBen3port)]
barplot(dat)


###################
###################
