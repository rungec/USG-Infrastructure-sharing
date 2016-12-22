#Redo of figures following reviewer comments #Round 2
#NOTE I looked at changing how homogenous impacts are calculated but didn't end up using this in the analysis

scen1Dir <- "C:/Claire/GPEM_Postdoc/1_USG_Collaboration/Analysis/tables/final costs/1_high_finalcosts_161221.csv"
minegroupingsDir <- "C:/Claire/GPEM_Postdoc/1_USG_Collaboration/Analysis/tables/Mine_groupings.csv"
scen3Dir <- "C:/Claire/GPEM_Postdoc/1_USG_Collaboration/Analysis/tables/final costs/3_low_finalcosts_150420.csv"
scen1byportDir <- "C:/Claire/GPEM_Postdoc/1_USG_Collaboration/Analysis/tables/1_high_diffuse_biodiversity_byport.csv"
scen5Dir <- "C:/Claire/GPEM_Postdoc/1_USG_Collaboration/Analysis/tables/final costs/5_partial_finalcosts.csv"
scen7Dir <- "C:/Claire/GPEM_Postdoc/1_USG_Collaboration/Analysis/tables/final costs/7_lowimpact_shared_finalcosts.csv"
#inpRast <- "C:/Claire/GPEM_Postdoc/1_USG_Collaboration/Data/Species/thresholdmaps/183sp/summed_presence/Summed_presence_183Sp.tif"
#spInpDir <- "C:/Claire/GPEM_Postdoc/1_USG_Collaboration/Data/Species/Species spreadsheets/Species_list_taxa_impacts.csv"

outDirList <- c("C:/Claire/GPEM_Postdoc/1_USG_Collaboration/Analysis/tables/", "_diffuse_biodiversity_totals.csv")
plotOutDir <- "C:/Claire/GPEM_Postdoc/1_USG_Collaboration/Analysis/figures/dataplots/183Sp_diffuse_rail4m_FINALSCENARIOS/"

#library(vioplot)
library(ggplot2)
library(gridExtra)

scen1data <- read.csv(scen1Dir, stringsAsFactors=FALSE)
scen3data <- read.csv(scen3Dir, stringsAsFactors=FALSE)[,1:22]#split off empty last column
dataPorts <- read.csv(scen1byportDir, stringsAsFactors=FALSE)
groupings <- read.csv(minegroupingsDir, stringsAsFactors=FALSE)

#Calculate homogenous impacts
avSpLoss <- sum(scen3data$SumSpLoss183SpDiffuse)/sum(scen3data$Area)#average species loss per area in scenario 3
#Alternate:
# r <- raster(inpRast)
# spAreadata <- read.csv(spInpDir, stringsAsFactors=FALSE)
# avSpLoss <- sum(spAreadata$Total_Area/(ncell(r)*0.25))/183 #Species area divided by area of study region = proportion of species habitat lost if all region developed #average species loss per km^2
#gave up here - this got a bit complicated because of the diffuse impacts - impact value in a cell depends on what happends to neighbours


dataAll <- data.frame(rep(c(1,3), each=28), rbind(scen1data,scen3data))
names(dataAll)[1]<-"Scenario"
dataAll$RailCostHighM <- dataAll$RailCostHigh/1000000
dataAll$RailCostExtraHighM <- dataAll$RailCostExtraHigh/1000000
dataAll$RoadCostM <- dataAll$RoadCost/1000000
dataAll$TotalCostRoad <- rowSums(dataAll[,c("AgriLoss", "TransCost", "RoadCost")])/1000000
dataAll$TotalCostRail <- rowSums(dataAll[,c("AgriLoss", "TransCost", "RailCostExtraHigh")])/1000000
dataAll$SumSpLoss183SpDiffHomog <- dataAll$Area*avSpLoss
altNames <- c("3PortShared", "3PortUnshare")

sapply(list(scen1data, scen3data), nrow)

mycols <- rainbow(4)

#top 5 high impact mines = Kingsgate, Eyre Iron, Mungana, Minotaur, Illuka
#big 5 high value mines = Bhp Billiton, Arrium, Illuka, Iron Road, OzMinerals 

###################################
###################################
#SET UP VARIABLES
#biodiversity benefit from collaboration
bioBen3port <- (dataAll[dataAll$Scenario==3, "SumSpLoss183SpDiffuse"] - dataAll[dataAll$Scenario==1, "SumSpLoss183SpDiffuse"])/100
bioBen3portHomog <- (dataAll[dataAll$Scenario==3, "SumSpLoss183SpDiffHomog"] - dataAll[dataAll$Scenario==1, "SumSpLoss183SpDiffHomog"])/100
bioBen3portdirect <- (dataAll[dataAll$Scenario==3, "SumSpLoss183Sp"] - dataAll[dataAll$Scenario==1, "SumSpLoss183Sp"])/100
#biodiversity benefit as % of total
bioBen3portPer <- bioBen3port/dataAll[dataAll$Scenario==3, "SumSpLoss183SpDiffuse"]*100*100
bioBen3portHomogPer <- bioBen3portHomog/dataAll[dataAll$Scenario==3, "SumSpLoss183SpDiffHomog"]*100*100
#financial benefit from collaboration
finBen3port <- (dataAll[dataAll$Scenario==3, "TotalCostRail"] - dataAll[dataAll$Scenario==1, "TotalCostRail"])/1000
#financial benefit as % of total cost
finBen3portPer <- finBen3port/dataAll[dataAll$Scenario==3, "TotalCostRail"]*100*1000
#number of mines collaborating
numMine3port <- dataAll[dataAll$Scenario==1, "NumCollab"]
numMine3portF <- factor(numMine3port, levels=as.character(5:16))
#normalised biodiversity impact
bioNorm3port3 <- dataAll[dataAll$Scenario==3, "SumSpLoss183SpDiffuse"]/dataAll[dataAll$Scenario==3, "Len"]
bioNorm3port1<- dataAll[dataAll$Scenario==1, "SumSpLoss183SpDiffuse"]/dataAll[dataAll$Scenario==1, "Len"]
finNorm3port <- finBen3port/dataAll[dataAll$Scenario==3, "Len"]


DFben <- data.frame(dataAll$Mine[1:28], dataAll$Len[1:28],bioBen3port,bioBen3portHomog, bioBen3portdirect, bioBen3portPer, bioBen3portHomogPer, finBen3port, finBen3portPer, numMine3port, numMine3portF, bioNorm3port3, bioNorm3port1, finNorm3port, groupings$Group_1, groupings$Group_2, groupings$Group_3)
names(DFben)[1:2] <- c("Mine", "Len")
write.csv(DFben, paste0(outDirList[1], "Data_for_plots_161221.csv"), row.names=FALSE)


#######################
#statistical tests whether means are different between scenarios - by mine
#statistical test whether means are different between independent and shared scenarios
tbio13 <- t.test(dataAll[dataAll$Scenario==3, "SumSpLoss183SpDiffuse"], dataAll[dataAll$Scenario==1, "SumSpLoss183SpDiffuse"], paired=TRUE)
tbio13
tfin13 <- t.test(dataAll[dataAll$Scenario==3, "TotalCostRail"], dataAll[dataAll$Scenario==1, "TotalCostRail"], paired=TRUE)
tfin13

#test whether means are different between independent and low impact scenarios
lowimpact <- dataAll[dataAll$Scenario==3, c("Mine", "SumSpLoss183SpDiffuse", "TotalCostRail")]
lowimpact[lowimpact$Mine %in% c("Eyre Iron","Iluka", "Kingsgate","Minotaur", "Mungana"), c("SumSpLoss183SpDiffuse","TotalCostRail")] <-0

tbio16 <- t.test(dataAll[dataAll$Scenario==3, "SumSpLoss183SpDiffuse"], lowimpact$SumSpLoss183SpDiffuse, paired=TRUE)
tbio16
tfin16 <- t.test(dataAll[dataAll$Scenario==3, "TotalCostRail"], lowimpact$TotalCostRail, paired=TRUE)
tfin16

#test whether means are different between independent and staged sharing scenarios
scen5data <- read.csv(scen5Dir, header=TRUE)

tbio15 <- t.test(dataAll[dataAll$Scenario==3, "SumSpLoss183SpDiffuse"], scen5data$SumSpLoss183spDiffuse, paired=TRUE)
tbio15
tfin15 <- t.test(dataAll[dataAll$Scenario==3, "TotalCostRail"], (scen5data$RailCostExtraHigh+scen5data$AgriLoss+scen5data$TransCost)/1000000, paired=TRUE)
tfin15

#######################
#statistical tests whether means are different between scenarios - by species
#Calculate impacts by species
spData1 <- read.csv(paste0(outDirList[1], "1_high", outDirList[2]), header=TRUE)
spData3 <- read.csv(paste0(outDirList[1], "3_low", outDirList[2]), header=TRUE)
spData5 <- read.csv(paste0(outDirList[1], "5_partial", outDirList[2]), header=TRUE)
spData6 <- read.csv(paste0(outDirList[1], "6_lowimpact", outDirList[2]), header=TRUE)
spData7 <- read.csv(paste0(outDirList[1], "7_lowimpact_shared", outDirList[2]), header=TRUE)


impactBySpeciesS1 <- matrix(colSums(spData1[,6:737]), ncol=4, byrow=TRUE)
impactBySpeciesS3 <- matrix(colSums(spData3[,6:737]), ncol=4, byrow=TRUE)
impactBySpeciesS5 <- matrix(colSums(spData5[,14:745]), ncol=4, byrow=TRUE)
impactBySpeciesS6 <- matrix(colSums(spData6[,6:737]), ncol=4, byrow=TRUE)
impactBySpeciesS7 <- matrix(colSums(spData7[,6:737]), ncol=4, byrow=TRUE)

#total number of species impacted
length(which(rowSums(impactBySpeciesS1)!=0))
length(which(rowSums(impactBySpeciesS3)!=0))
length(which(rowSums(impactBySpeciesS5)!=0))
length(which(rowSums(impactBySpeciesS6)!=0))
length(which(rowSums(impactBySpeciesS7)!=0))

#Mean habitat loss per species (% of total habitat)
mean(rowSums(impactBySpeciesS1))
mean(rowSums(impactBySpeciesS3))
mean(rowSums(impactBySpeciesS5))
mean(rowSums(impactBySpeciesS6))
mean(rowSums(impactBySpeciesS7))

#For all 183 species
#t test whether means are different independent vs shared
tsp13 <- t.test(rowSums(impactBySpeciesS3), rowSums(impactBySpeciesS1), paired=TRUE)
tsp13
#t test whether means are different independent vs lowimpact
tsp16 <- t.test(rowSums(impactBySpeciesS3), rowSums(impactBySpeciesS6), paired=TRUE)
tsp16
#t test whether means are different independent vs staged shared
tsp15 <- t.test(rowSums(impactBySpeciesS3), rowSums(impactBySpeciesS5), paired=TRUE)
tsp15
#t test whether means are different independent vs staged shared
tsp17 <- t.test(rowSums(impactBySpeciesS3), rowSums(impactBySpeciesS7), paired=TRUE)
tsp17

#mean habitat loss for species impacted
mean(rowSums(impactBySpeciesS1)[which(rowSums(impactBySpeciesS1)!=0)])
sd(rowSums(impactBySpeciesS1)[which(rowSums(impactBySpeciesS1)!=0)])
mean(rowSums(impactBySpeciesS3)[which(rowSums(impactBySpeciesS3)!=0)])
sd(rowSums(impactBySpeciesS3)[which(rowSums(impactBySpeciesS3)!=0)])


#######################
#FINAL PLOTS
#######################

# #Pannell plot
# p1 <- ggplot(DFben, aes(x=finBen3portPer, y=bioBen3port))+
	# geom_blank() +
	# theme_bw(17) + #remove grey #remove grids
	# theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
	# #scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) #make start at zero
	# coord_cartesian(xlim=c(0,100), ylim=c(0,0.60001))+ #set x and y limits
	# #theme(axis.ticks = element_blank(), axis.text = element_blank())+#hide tixk marks and numbers
	# labs(x="Private net benefit", y="Public net benefit")+
	# theme(axis.title.x = element_text(vjust=-0.5),axis.title.y = element_text(vjust=1))+ #move xylabels away from graph
	# geom_rect(xmin=0, ymin=0.1, xmax=10, ymax=0.6, fill="lightskyblue")+
	# geom_rect(xmin=10, ymin=0.1, xmax=25, ymax=0.6, fill="lightskyblue1")+
	# geom_rect(xmin=0, ymin=0, xmax=100, ymax=0.1, fill="grey70")+
	# geom_polygon(aes(x=c(25, 100, 100, 25), y=c(0.1, 0.1, 0.3,0.1)), fill="grey90") +
	# geom_line(aes(x=c(24.99,25), y=c(0.6, 0.1)), linetype="dashed", size=1)+
	# geom_line(aes(x=c(25,100), y=c(0.1, 0.3)), linetype="dashed", size=1)+
	# geom_line(aes(x=c(10,10), y=c(0.6, 0.1)), linetype="solid", size=1)+
	# geom_line(aes(x=c(10,100), y=c(0.1, 0.1)), linetype="solid", size=1)+
	# geom_segment(aes(x=10,xend=25,y=0.45,yend=0.45),arrow=arrow(length=unit(0.2,"cm")),show_guide=F, size=1)+ #add arrow
	# geom_segment(aes(x=80,xend=80,y=0.1,yend=0.24),arrow=arrow(length=unit(0.2,"cm")),show_guide=F, size=1)+ #add arrow
	# annotate("text", label="POSITIVE INCENTIVES", x=5, y=0.3, angle=90, size=6)+
	# annotate("text", label="NO ACTION", x=50, y=0.05, size=6)+
	# annotate("text", label="EXTENSION", x=70, y=0.5, size=6)+
	# annotate("text", label="Transaction \n cost (private)", x=37, y=0.45, angle=0, size=5)+
	# annotate("text", label="Transaction \n cost (public)", x=90, y=0.18, angle=0, size=5)

# outPath <- paste0(plotOutDir, "USG_collab_Pannell_fig.png")
	# ggsave(filename=outPath)
	
#######################
#Public vs private incentive plot
#######################

#p2 <- ggplot(DFben, aes(x=finBen3portPer, y=bioBen3port, color=groupings.Group_3, shape=groupings.Group_3)) +
p2 <- ggplot(DFben, aes(x=finBen3portPer, y=bioBen3port, shape=groupings.Group_3)) +
	geom_blank() +
	#theme_bw(17) + #remove grey #remove grids
	theme_classic(17) + #remove grey #remove grids
	theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line.x = element_line(color="black"), axis.line.y = element_line(color="black"))+
	#scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) #make start at zero
	coord_cartesian(xlim=c(15,100), ylim=c(0,0.60001))+ #set x and y limits
	labs(x="Averted capital cost (% of total)", y="Averted biodiversity impact")+
	theme(axis.title.x = element_text(vjust=-0.6),axis.title.y = element_text(vjust=1))+ #move xylabels away from graph
	#geom_rect(xmin=0, ymin=0.1, xmax=25, ymax=0.6, fill="lightskyblue1")+
	#geom_rect(xmin=0, ymin=0, xmax=100, ymax=0.1, fill="grey70")+
	#annotate("text", label="POSITIVE INCENTIVES", x=20, y=0.3, angle=90, size=6)+
	#annotate("text", label="NO ACTION", x=30, y=0.05, size=6)+
	#annotate("text", label="EXTENSION", x=70, y=0.5, size=6)+
	geom_abline(slope=-0.003464, intercept=0.354103, colour="grey70", linetype='dashed')+
	#geom_smooth(method="lm", se=FALSE, colour="grey70", linetype='dashed')+
	geom_point(size=4, colour='black', show.legend = FALSE)+
	scale_shape_manual(values=c(4,2,3,1,0))
	#scale_color_manual(values=c("darkgoldenrod1", "cyan4","darkblue", "cyan2", "darkgray"))

# outPath <- paste0(plotOutDir, "USG_collab_Pannell_withData_fig.png")
#outPath <- paste0(plotOutDir, "USG_collab_PublicvsPrivate_fig.png")
outPath <- paste0(plotOutDir, "USG_collab_PublicvsPrivate_fig_withline.png")
	ggsave(filename=outPath)
#outPath <- paste0(plotOutDir, "USG_collab_PublicvsPrivate_fig.pdf")
outPath <- paste0(plotOutDir, "USG_collab_PublicvsPrivate_fig_withline.pdf")
	ggsave(filename=outPath)
dev.off()
	
#[stats for trendline]
fit <- lm(bioBen3port~finBen3portPer)
summary(fit) #not significant
fit2 <- lm(bioBen3port[-9]~finBen3portPer[-9])
summary(fit2)
outPath <- paste0(plotOutDir, "USG_collab_PublicvsPrivate_fig_withline_fit.pdf")
pdf(file=outPath)
	par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
	plot(fit)
	dev.off()
outPath <- paste0(plotOutDir, "USG_collab_PublicvsPrivate_fig_withline_fit2.pdf")
pdf(file=outPath)
	par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
	plot(fit2)
	dev.off()
	
# library(nlme)
# fit2 <- lme(bioBen3port~finBen3portPer, random=~1|DFben$groupings.Group_3) #glmm with line as random factor
# summary(fit2) #couldnt get this to work

# #######################
# #2-part pannell plot
# #######################
# outPath <- paste0(plotOutDir, "USG_collab_Pannell_2part_fig.png") 
# png(filename=outPath, width=960, height=480)
# grid.arrange(p1+annotate("text", label="a", size=7, x=95, y=0.57, fontface="bold"), p2+annotate("text", label="b", size=7, x=95, y=0.57, fontface="bold"), ncol=2)
# dev.off()



################
#Model & plot of relationship between biodiversity & length
################

DFHetero <- data.frame(rep(dataAll[dataAll$Scenario==3, "Len"], 2), append(dataAll[dataAll$Scenario==3, "SumSpLoss183SpDiffHomog"]/100,dataAll[dataAll$Scenario==3, "SumSpLoss183SpDiffuse"]/100), rep(c("Homog", "Hetero"), each=28))
names(DFHetero)<- c("Len", "Impact", "HetorHom")
#add points for the shared scenario with 3 ports (lines)
# DFHetero <- rbind(DFHetero, data.frame(	
		# Len = unlist(rep(dataPorts[dataPorts$X=="Length_km",c("Bonython", "Hardy", "Myponie")], 2)), 
		# Impact = unlist(c( dataPorts[dataPorts$X=="SumSpTotal", c("Bonython", "Hardy", "Myponie")]/183, dataPorts[dataPorts$X=="Area_km2", c("Bonython", "Hardy", "Myponie")]*avSpLoss/100)),
		# HetorHom = rep(c("Hetero", "Homog"), each=3)
		# ))
		#subHetero <- subset(DFHetero, DFHetero$HetorHom=="Hetero")[c(1:28, 30, 31),] #remove outlier at 1880km
		
DFHetero <- rbind(DFHetero, data.frame(	
		Len = rep(DF5portlinks$Len, 2),
		Impact = c(DF5portlinks$HeteroSharedImpact, DF5portlinks$HomogSharedImpact),
		HetorHom = rep(c("Hetero", "Homog"), each=5)
		))	
subHetero <- subset(DFHetero, DFHetero$HetorHom=="Hetero")[c(1:29, 31:33),] #remove outlier at 1589km

mod1 <- lm(subHetero$Impact~0+subHetero$Len) #set intercept to zero
summary(mod1)
mod1b <- lm(DFHetero$Impact~0+DFHetero$Len) #all datapoints
summary(mod1b)

outPath <- paste0(plotOutDir, "USG_collab_biodiv_vs_length_modelfit_excluding1589km.pdf")
pdf(file=outPath)
	par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
	plot(mod1)
	dev.off()
outPath <- paste0(plotOutDir, "USG_collab_biodiv_vs_length_modelfit_alldata.pdf")
pdf(file=outPath)
	par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
	plot(mod1b)
	dev.off()

modpred <- cbind(subHetero,predict(mod1,interval="confidence"), Scenario=rep(c("Independent", "Shared"), times=c(28, 4)))

p <- ggplot(modpred, aes(Len, Impact, shape=Scenario, colour=HetorHom)) +
	geom_ribbon(aes(ymin=lwr,ymax=upr),fill = "gray80", colour="gray80")+
	geom_abline(slope=coef(mod1), intercept=0, colour="black") +
	geom_point(color="black", size=4) +#add points
	#scale_color_manual(name="Species\nDistribution", breaks=c("Hetero", "Homog"),labels=c("Heterogeneous", "Homogeneous"), values=c("black", "lightblue"))+ #change point colours and legend titles
	#scale_colour_manual(name="Distribution of biodiversity", breaks=c("Homog", "Hetero"), labels=c("Homogeneous", "Real data"), values=c("black", "grey70"))+ #change point colours and legend titles
	#scale_shape_manual(name="Distribution of biodiversity", breaks=c("Homog", "Hetero"), labels=c("Homogeneous", "Real data"), values=c(19, 17))+ #change point shapes and make it a single legend
	theme_classic(17) + #get rid of grey bkg and gridlines
	theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
	coord_cartesian(xlim=c(0,780), ylim=c(0,0.75))+ #set x and y limits
	labs(x="Infrastructure length (km)", y="Biodiversity impact")+
	theme(axis.title.x = element_text(vjust=-0.6),axis.title.y = element_text(vjust=1), axis.line.x = element_line(color="black"), axis.line.y = element_line(color="black"))+
	theme(legend.position="none")#gets rid of legend
	#theme(legend.title=element_blank())#get rid of legend title
	#theme(legend.position=c(.25, .85), legend.title = element_text(size=14, face="bold"), legend.key = element_blank())
	#theme(legend.background = element_rect(colour="grey70", size=.5, linetype="solid"))+#add box
	
	
outPath <- paste0(plotOutDir, "USG_collab_Hetero_Homog_vs_Length_bothscenarios_fig_5lines_v2.png")
	ggsave(filename=outPath)
outPath <- paste0(plotOutDir, "USG_collab_Hetero_Homog_vs_Length_bothscenarios_fig_5lines_v2.pdf")
	ggsave(filename=outPath)

		
#######################
#Summary & plot of scenario impacts by Port grouping
#######################

#with points added for the 5 shared mine-port links	
scen6data <- dataAll[dataAll$Scenario==3,]
scen6data[scen6data$Mine %in% c("Eyre Iron", "Iluka", "Minotaur", "Mungana", "Kingsgate"),"SumSpLoss183SpDiffuse"] <- 0
scen5 <- read.csv(scen5Dir, header=TRUE)
scen5data <- scen5[order(scen5$Mine),]
scen7 <- read.csv(scen7Dir, header=TRUE)
blankrows <- data.frame(c("Eyre Iron", "Iluka", "Minotaur", "Mungana", "Kingsgate"), 0,0,0,0,0,0,0,0,0,0,0,0)
names(blankrows) <- names(scen7)
scen7 <- rbind(scen7,blankrows) 
scen7data <- scen7[order(scen7$Mine),]


DF5portlinks <- data.frame(aggregate(DFben$Len, by=list(DFben$groupings.Group_3), FUN=sum), 
				HeteroSharedImpact=aggregate(dataAll[dataAll$Scenario==1, "SumSpLoss183SpDiffuse"]/100,  by=list(DFben$groupings.Group_3), FUN=sum)$x, 
				HomogImpact=aggregate(dataAll[dataAll$Scenario==1, "SumSpLoss183SpDiffHomog"]/100,  by=list(DFben$groupings.Group_3), FUN=sum)$x, 
				HeteroUnsharedImpact=aggregate(dataAll[dataAll$Scenario==3, "SumSpLoss183SpDiffuse"]/100,  by=list(DFben$groupings.Group_3), FUN=sum)$x,
				LowImpact = aggregate(scen6data[, "SumSpLoss183SpDiffuse"]/100,  by=list(DFben$groupings.Group_3), FUN=sum)$x,
				LowImpactShared = aggregate(scen7data[, "SumSpLoss183spDiffuse"]/100,  by=list(DFben$groupings.Group_3), FUN=sum)$x,
				RestrictedAccess = aggregate(scen5data[, "SumSpLoss183spDiffuse"]/100,  by=list(DFben$groupings.Group_3), FUN=sum)$x
			)
				
names(DF5portlinks)[1:2] <- c("Group_3", "Len")
write.csv(DF5portlinks, "C:/Claire/GPEM_Postdoc/1_USG_Collaboration/Analysis/tables/1_high_diffuse_biodiversity_by5mineportlinks.csv", row.names=FALSE)		
	
#######################
#Plot of benefit of collaborating by 5 mine-port links
#######################
#totalimpact <- sum(DF5portlinks$HeteroUnsharedImpact)	
plotDf <- with(DF5portlinks, data.frame(Group3=rep(Group_3, 5), PercContrib=c(HeteroSharedImpact, HeteroUnsharedImpact, LowImpact, LowImpactShared, RestrictedAccess), Scen=rep(c("Shared", "Independent", "LowImpact", "LowImpactShared", "RestrictedAccess"), each=5)))
plotDf$ScenOrdered <- factor(plotDf$Scen, levels=c("Independent", "RestrictedAccess", "LowImpact", "LowImpactShared", "Shared"))

#Plot for supp info
p <- ggplot(plotDf, aes(x=Group3, y=PercContrib, fill=ScenOrdered))+
		geom_bar(stat='identity', position='dodge')+
		scale_fill_grey(name="Scenario", 
                         breaks=c("Independent", "RestrictedAccess", "LowImpact", "LowImpactShared", "Shared"),
                         labels=c("Independent", "Restricted access", "Low impact independent", "Low Impact shared", "Shared"), start=0.8, end=0.2)+
		theme_classic(17) + #get rid of grey bkg and gridlines
		theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
	#coord_cartesian(xlim=c(0,780), ylim=c(0,0.75))+ #set x and y limits
		labs(x="Mine-port link region", y="Biodiversity impact")+
		theme(axis.title.x = element_text(vjust=-0.6),axis.title.y = element_text(vjust=1), axis.line.x = element_line(color="black"), axis.line.y = element_line(color="black"))+#move xylabels away from graph
		theme(legend.position="right", legend.text=element_text(size=14))+#use 'none' to get rid of legend
		theme(legend.title=element_blank())#get rid of legend title
	
outPath <- paste0(plotOutDir, "USG_collab_Shared_vs_Unshared_5mineportlinks_fig_suppinfo.png")
	ggsave(filename=outPath)
outPath <- paste0(plotOutDir, "UUSG_collab_Shared_vs_Unshared_5mineportlinks_fig_suppinfo.pdf")
	ggsave(filename=outPath)	

#Plot for manuscript
plotDfSub <- plotDf[plotDf$Scen %in% c("Independent", "RestrictedAccess", "LowImpact", "Shared"),]
p <- ggplot(plotDfSub, aes(x=Group3, y=PercContrib, fill=ScenOrdered))+
		geom_bar(stat='identity', position='dodge')+
		scale_fill_grey(name="Scenario", 
                         breaks=c("Independent", "RestrictedAccess", "LowImpact", "LowImpactShared", "Shared"),
                         labels=c("Independent", "Restricted access", "Low impact", "LowImpactShared", "Shared"), start=0.8, end=0.2)+
		theme_classic(17) + #get rid of grey bkg and gridlines
		theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
	#coord_cartesian(xlim=c(0,780), ylim=c(0,0.75))+ #set x and y limits
		labs(x="Mine-port link region", y="Biodiversity impact")+
		theme(axis.title.x = element_text(vjust=-0.6),axis.title.y = element_text(vjust=1), axis.line.x = element_line(color="black"), axis.line.y = element_line(color="black"))+#move xylabels away from graph
		theme(legend.position="bottom", legend.text=element_text(size=14))+#use 'none' to get rid of legend
		theme(legend.title=element_blank())#get rid of legend title
	
outPath <- paste0(plotOutDir, "USG_collab_Shared_vs_Unshared_5mineportlinks_fig.png")
	ggsave(filename=outPath)
outPath <- paste0(plotOutDir, "UUSG_collab_Shared_vs_Unshared_5mineportlinks_fig.pdf")
	ggsave(filename=outPath)	
	
#######################
#Plot of collaboration benefit per number of partners
#######################

# meanlines <- data.frame(aggregate(data.frame(DFben$bioBen3portPer,DFben$finBen3portPer), by=list(DFben$numMine3portF), FUN=mean))
# names(meanlines)=c("numMine3portF", "Meanbio", "Meanfin")

# p1 <- ggplot(DFben, aes(x=numMine3portF, y=bioBen3portPer)) +
	# geom_point(size=3.5)+
	# geom_errorbar(data=meanlines, aes(y=Meanbio, ymin=Meanbio, ymax=Meanbio), width=0.8, size=1)+
	# theme_bw(17) + #the number here changes the size of all the text in the graph
	# theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
	# #coord_cartesian(ylim=c(30,100))+
	# labs(x="Number of companies collaborating", y="Averted biodiversity impact (% of total)")+
	# theme(axis.title.x = element_text(vjust=-0.5),axis.title.y = element_text(vjust=0.6))+	#move xylabels away from graph
	# annotate("text", x = 0.65, y = 99, label = "a", face="bold", size=8)
	
# p2 <- ggplot(DFben, aes(x=numMine3portF, y=finBen3portPer)) +
	# geom_point(size=3.5)+
	# geom_errorbar(data=meanlines, aes(y=Meanfin, ymin=Meanfin, ymax=Meanfin), width=0.8, size=1)+
	# theme_bw(17) + #the number here changes the size of all the text in the graph
	# theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
	# #coord_cartesian(ylim=c(20,90))+
	# labs(x="Number of companies collaborating", y="Averted capital cost (% of total)")+
	# theme(axis.title.x = element_text(vjust=-0.5),axis.title.y = element_text(vjust=1))+	#move xylabels away from graph
	# annotate("text", x = 0.65, y = 99, label = "b", face="bold", size=8)

# outPath <- paste0(plotOutDir, "USG_collab_NumCollab_fig.png") 
# png(filename=outPath, width=960, height=480)
# grid.arrange(p1, p2, ncol=2)
# dev.off()


#######################
#Plot comparing shared with unshared
#######################

#set up data
DFShareUn <- data.frame(scen1data$SumSpLoss183SpDiffuse/100, scen3data$SumSpLoss183SpDiffuse/100, dataAll[dataAll$Scenario==1, "TotalCostRail"], dataAll[dataAll$Scenario==3, "TotalCostRail"])
names(DFShareUn) <- c("ShareSpLoss","UnshareSpLoss", "Sharecost", "Unsharecost")

pB <- ggplot(DFShareUn)+
	geom_segment(aes(x=DFShareUn$"Sharecost", y=DFShareUn$"ShareSpLoss", xend=DFShareUn$"Unsharecost", yend=DFShareUn$"UnshareSpLoss"), size=0.7, colour="grey30")+
	geom_point(aes(x=DFShareUn$"Sharecost", y=DFShareUn$"ShareSpLoss", colour="ShareSpLoss"), size=3)+
	geom_point(aes(x=DFShareUn$"Unsharecost", y=DFShareUn$"UnshareSpLoss", colour="UnshareSpLoss"), size=3)+
	# geom_point(aes(x=DFShareUn$"Sharecost", y=DFShareUn$"ShareSpLoss"), colour='black', size=3)+
	# geom_point(aes(x=DFShareUn$"Unsharecost", y=DFShareUn$"UnshareSpLoss"), colour="grey70", size=3)+
	labs(x="Capital cost (million $)", y="Biodiversity impact")+
	theme_classic(17) + #get rid of grey bkg and gridlines
	#theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
	theme(axis.title.x = element_text(vjust=-0.6),axis.title.y = element_text(vjust=1), axis.line.x = element_line(color="black"), axis.line.y = element_line(color="black"))+	#move xylabels away from graph
	#edit legend
	scale_fill_discrete(breaks=c("UnshareSpLoss", "ShareSpLoss"))+
	#scale_color_discrete(name="Scenario", labels=c("Independent", "Shared"),breaks=c("UnshareSpLoss", "ShareSpLoss"))+
	scale_color_manual(name="Scenario", labels=c("Independent", "Shared"),breaks=c("UnshareSpLoss", "ShareSpLoss"), values=c('black', 'grey70'))+
	theme(legend.position=c(.2, .85), legend.title = element_text(size=14, face="bold"))#legend position
	#theme(legend.background = element_rect(colour="grey70", size=.5, linetype="solid"))#add box
	#theme(legend.key = element_blank())#remove boxes from around legend items

outPath <- paste0(plotOutDir, "USG_collab_Shared_vs_Unshared_fig.png")
	ggsave(filename=outPath)
outPath <- paste0(plotOutDir, "USG_collab_Shared_vs_Unshared_fig.pdf")
	ggsave(filename=outPath)
	
	
#######################
#Plot comparing shared with unshared - agricultural data
#######################	
#set up data
DFShareUnAg <- data.frame(dataAll[dataAll$Scenario==1, "AgriLoss"]/1000000, dataAll[dataAll$Scenario==3, "AgriLoss"]/1000000, dataAll[dataAll$Scenario==1, "TotalCostRail"], dataAll[dataAll$Scenario==3, "TotalCostRail"])
names(DFShareUnAg) <- c("ShareAgLoss","UnshareAgLoss", "Sharecost", "Unsharecost")

pA <- ggplot(DFShareUnAg)+
	geom_segment(aes(x=DFShareUnAg$"Sharecost", y=DFShareUnAg$"ShareAgLoss", xend=DFShareUnAg$"Unsharecost", yend=DFShareUnAg$"UnshareAgLoss"), size=0.7, colour="grey30")+
	geom_point(aes(x=DFShareUnAg$"Sharecost", y=DFShareUnAg$"ShareAgLoss", colour="ShareAgLoss"), size=3)+
	geom_point(aes(x=DFShareUnAg$"Unsharecost", y=DFShareUnAg$"UnshareAgLoss", colour="UnshareAgLoss"), size=3)+
	labs(x="Capital cost (million $)", y="Agricultural impact (million $)")+
	theme_classic(17) + #get rid of grey bkg and gridlines
	#theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
	theme(axis.title.x = element_text(vjust=-0.6),axis.title.y = element_text(vjust=1), axis.line.x = element_line(color="black"), axis.line.y = element_line(color="black"))+	#move xylabels away from graph
	#edit legend
	scale_fill_discrete(breaks=c("UnshareAgLoss", "ShareAgLoss"))+
	scale_color_discrete(name="Scenario", labels=c("Independent", "Shared"),breaks=c("UnshareAgLoss", "ShareAgLoss"))+
	#theme(legend.position=c(.2, .85))#legend position
	#theme(legend.background = element_rect(colour="grey70", size=.5, linetype="solid"))#add box
	#theme(legend.key = element_blank())#remove boxes from around legend items
	theme(legend.position="none")

outPath <- paste0(plotOutDir, "USG_collab_Shared_vs_Unshared_AgriLoss_fig.png")
	ggsave(filename=outPath)
outPath <- paste0(plotOutDir, "USG_collab_Shared_vs_Unshared_AgriLoss_fig.pdf")
	ggsave(filename=outPath)
	
#######################
#Plot violin plot of biodiversity impact of each scenario
#######################	

bioL <- list("3_low_diffuse_biodiversity_total_byspecies_areas.csv", "1_high_diffuse_biodiversity_total_byspecies_areas.csv")

bioLDf <- lapply(bioL, function(x) {
	currBio <- read.csv(paste0(outDirList[1], x))
	currBio <- rowSums(matrix(colSums(currBio[,6:ncol(currBio)]), ncol=4, byrow=TRUE))#sum the impacts across the 4 impact classes (0m, 125m, 500m, 750m) for each species
		})
bioDF <- data.frame(Scenario=rep(c("Independent", "Shared"), each=183), BioAreaLost=unlist(bioLDf)*100)#convert km to 1000*ha

pV <- ggplot(data=bioDF, aes(factor(Scenario), BioAreaLost))+
		#geom_violin(colour="grey70", fill="grey70")+
		geom_boxplot(colour="black", fill="grey70")+
		geom_jitter(height=0, width=0.5, colour="black",)+
		theme_classic(17)+
		theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
		#coord_trans(y="log10")+
		scale_y_log10(labels=scales::comma) + 
		labs(x="", y="Area habitat loss (ha)")+
		#labs(x="", y=expression(paste("Area habitat loss (", 10^3, " ha)")))+
		theme(axis.title.x = element_text(vjust=-0.6),axis.title.y = element_text(vjust=1)) #move xylabels away from graph
	
outPath <- paste0(plotOutDir, "USG_collab_Shared_vs_Unshared_HabitatAreaLoss.png")
	ggsave(filename=outPath)		

#loss as % of total habitat
bioL2 <- list("3_low_diffuse_biodiversity_totals.csv", "1_high_diffuse_biodiversity_totals.csv")
bioLDf2 <- lapply(bioL2, function(x) {
	currBio <- read.csv(paste0(outDirList[1], x))
	currBio <- rowSums(matrix(colSums(currBio[,6:ncol(currBio)]), ncol=4, byrow=TRUE))#sum the impacts across the 4 impact classes (0m, 125m, 500m, 750m) for each species
		})
bioDF2 <- data.frame(Scenario=rep(c("Independent", "Shared"), each=183), BioAreaLost=unlist(bioLDf2))

summary(bioDF2[bioDF2$Scenario=="Independent", "BioAreaLost"])
summary(bioDF2[bioDF2$Scenario=="Shared", "BioAreaLost"])

bioDF3 <- subset(bioDF2, BioAreaLost!=0)
summary(bioDF3[bioDF3$Scenario=="Independent", "BioAreaLost"])
summary(bioDF3[bioDF3$Scenario=="Shared", "BioAreaLost"])

