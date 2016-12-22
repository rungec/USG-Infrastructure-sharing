inpDir1 <- "Z:/ayesha/USG_Collaboration/Analysis/tables/final costs/1_high_finalcosts_150420.csv"
inpDir2 <- "Z:/ayesha/USG_Collaboration/Analysis/tables/Mine_groupings.csv"
inpDir3 <- "Z:/ayesha/USG_Collaboration/Analysis/tables/final costs/3_low_finalcosts_150420.csv"
outDir <- "Z:/ayesha/USG_Collaboration/Analysis/figures/dataplots/183Sp_diffuse_rail4m_FINALSCENARIOS/"

#library(vioplot)
library(ggplot2)
library(gridExtra)

data1 <- read.csv(inpDir1, stringsAsFactors=FALSE)
data3 <- read.csv(inpDir3, stringsAsFactors=FALSE)[,1:22]#split off empty last column
groupings <- read.csv(inpDir2, stringsAsFactors=FALSE)

avSpLoss <- sum(data3$SumSpLoss183SpDiffuse)/sum(data3$Area)#average species loss per area in scenario 3

dataAll <- data.frame(rep(c(1,3), each=28), rbind(data1,data3))
names(dataAll)[1]<-"Scenario"
dataAll$RailCostHighM <- dataAll$RailCostHigh/1000000
dataAll$RailCostExtraHighM <- dataAll$RailCostExtraHigh/1000000
dataAll$RoadCostM <- dataAll$RoadCost/1000000
dataAll$TotalCostRoad <- rowSums(dataAll[,c("AgriLoss", "TransCost", "RoadCost")])/1000000
dataAll$TotalCostRail <- rowSums(dataAll[,c("AgriLoss", "TransCost", "RailCostExtraHigh")])/1000000
dataAll$SumSpLoss183SpDiffHomog <- dataAll$Area*avSpLoss
altNames <- c("3PortShared", "3PortUnshare")

sapply(list(data1, data3), nrow)

mycols <- rainbow(4)

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

# outPath <- paste0(outDir, "USG_collab_Pannell_fig.png")
	# ggsave(filename=outPath)
	
#######################
#Public vs private incentive plot
#######################

p2 <- ggplot(DFben, aes(x=finBen3portPer, y=bioBen3port, color=groupings.Group_3)) +
	geom_blank() +
	theme_bw(17) + #remove grey #remove grids
	theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
	#scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) #make start at zero
	coord_cartesian(xlim=c(15,100), ylim=c(0,0.60001))+ #set x and y limits
	labs(x="Averted capital cost (% of total)", y="Averted biodiversity impact")+
	theme(axis.title.x = element_text(vjust=-0.6),axis.title.y = element_text(vjust=1))+ #move xylabels away from graph
	#geom_rect(xmin=0, ymin=0.1, xmax=25, ymax=0.6, fill="lightskyblue1")+
	#geom_rect(xmin=0, ymin=0, xmax=100, ymax=0.1, fill="grey70")+
	#annotate("text", label="POSITIVE INCENTIVES", x=20, y=0.3, angle=90, size=6)+
	#annotate("text", label="NO ACTION", x=30, y=0.05, size=6)+
	#annotate("text", label="EXTENSION", x=70, y=0.5, size=6)+
	geom_point(shape=16, size=4, show_guide = FALSE)+
	#scale_shape_manual(values=c(17,25,15,22,16))
	scale_color_manual(values=c("darkgoldenrod1", "cyan4","darkblue", "cyan2", "darkgray"))

# outPath <- paste0(outDir, "USG_collab_Pannell_withData_fig.png")
outPath <- paste0(outDir, "USG_collab_PublicvsPrivate_fig.png")
	ggsave(filename=outPath)
outPath <- paste0(outDir, "USG_collab_PublicvsPrivate_fig.pdf")
	ggsave(filename=outPath)

# #######################
# #2-part pannell plot
# #######################
# outPath <- paste0(outDir, "USG_collab_Pannell_2part_fig.png") 
# png(filename=outPath, width=960, height=480)
# grid.arrange(p1+annotate("text", label="a", size=7, x=95, y=0.57, fontface="bold"), p2+annotate("text", label="b", size=7, x=95, y=0.57, fontface="bold"), ncol=2)
# dev.off()

	
#######################
#Plot of heterogeneity
#######################

DFHetero <- data.frame(rep(dataAll[dataAll$Scenario==3, "Len"], 2), append(dataAll[dataAll$Scenario==3, "SumSpLoss183SpDiffHomog"]/100,dataAll[dataAll$Scenario==3, "SumSpLoss183SpDiffuse"]/100), rep(c("Homog", "Hetero"), each=28))
names(DFHetero)<- c("Len", "Impact", "HetorHom")

p <- ggplot(DFHetero, aes(Len, Impact, shape=HetorHom, colour=HetorHom)) +
	geom_point(aes(color=HetorHom), size=4) +#add points
	#scale_color_manual(name="Species\nDistribution", breaks=c("Hetero", "Homog"),labels=c("Heterogeneous", "Homogeneous"), values=c("black", "lightblue"))+ #change point colours and legend titles
	scale_colour_manual(name="Distribution of biodiversity", breaks=c("Homog", "Hetero"), labels=c("Homogeneous", "Real data"), values=c("black", "grey70"))+ #change point colours and legend titles
	scale_shape_manual(name="Distribution of biodiversity", breaks=c("Homog", "Hetero"), labels=c("Homogeneous", "Real data"), values=c(19, 17))+ #change point shapes and make it a single legend
	theme_bw(17) + #get rid of grey bkg and gridlines
	theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
	coord_cartesian(xlim=c(0,650), ylim=c(0,0.75))+ #set x and y limits
	labs(x="Infrastructure length (km)", y="Biodiversity impact")+
	theme(axis.title.x = element_text(vjust=-0.6),axis.title.y = element_text(vjust=1))+	#move xylabels away from graph
	#theme(legend.title=element_blank())#get rid of legend title
	theme(legend.position=c(.25, .9))+#legend position
	#theme(legend.background = element_rect(colour="grey70", size=.5, linetype="solid"))+#add box
	theme(legend.key = element_blank())
	
outPath <- paste0(outDir, "USG_collab_Hetero_Homog_vs_Length_fig.png")
	ggsave(filename=outPath)
outPath <- paste0(outDir, "USG_collab_Hetero_Homog_vs_Length_fig.pdf")
	ggsave(filename=outPath)

#######################
#Plot of collaboration benefit per number of partners
#######################

meanlines <- data.frame(aggregate(data.frame(DFben$bioBen3portPer,DFben$finBen3portPer), by=list(DFben$numMine3portF), FUN=mean))
names(meanlines)=c("numMine3portF", "Meanbio", "Meanfin")

p1 <- ggplot(DFben, aes(x=numMine3portF, y=bioBen3portPer)) +
	geom_point(size=3.5)+
	geom_errorbar(data=meanlines, aes(y=Meanbio, ymin=Meanbio, ymax=Meanbio), width=0.8, size=1)+
	theme_bw(17) + #the number here changes the size of all the text in the graph
	theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
	#coord_cartesian(ylim=c(30,100))+
	labs(x="Number of companies collaborating", y="Averted biodiversity impact (% of total)")+
	theme(axis.title.x = element_text(vjust=-0.5),axis.title.y = element_text(vjust=0.6))+	#move xylabels away from graph
	annotate("text", x = 0.65, y = 99, label = "a", face="bold", size=8)
	
p2 <- ggplot(DFben, aes(x=numMine3portF, y=finBen3portPer)) +
	geom_point(size=3.5)+
	geom_errorbar(data=meanlines, aes(y=Meanfin, ymin=Meanfin, ymax=Meanfin), width=0.8, size=1)+
	theme_bw(17) + #the number here changes the size of all the text in the graph
	theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
	#coord_cartesian(ylim=c(20,90))+
	labs(x="Number of companies collaborating", y="Averted capital cost (% of total)")+
	theme(axis.title.x = element_text(vjust=-0.5),axis.title.y = element_text(vjust=1))+	#move xylabels away from graph
	annotate("text", x = 0.65, y = 99, label = "b", face="bold", size=8)

outPath <- paste0(outDir, "USG_collab_NumCollab_fig.png") 
png(filename=outPath, width=960, height=480)
grid.arrange(p1, p2, ncol=2)
dev.off()


#######################
#Plot comparing shared with unshared
#######################

#set up data
DFShareUn <- data.frame(data1$SumSpLoss183SpDiffuse/100, data3$SumSpLoss183SpDiffuse/100, dataAll[dataAll$Scenario==1, "TotalCostRail"], dataAll[dataAll$Scenario==3, "TotalCostRail"])
names(DFShareUn) <- c("ShareSpLoss","UnshareSpLoss", "Sharecost", "Unsharecost")

pB <- ggplot(DFShareUn)+
	geom_segment(aes(x=DFShareUn$"Sharecost", y=DFShareUn$"ShareSpLoss", xend=DFShareUn$"Unsharecost", yend=DFShareUn$"UnshareSpLoss"), size=0.7, colour="grey30")+
	geom_point(aes(x=DFShareUn$"Sharecost", y=DFShareUn$"ShareSpLoss", colour="ShareSpLoss"), size=3)+
	geom_point(aes(x=DFShareUn$"Unsharecost", y=DFShareUn$"UnshareSpLoss", colour="UnshareSpLoss"), size=3)+
	labs(x="Capital cost (million $)", y="Biodiversity impact")+
	theme_classic(17) + #get rid of grey bkg and gridlines
	#theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
	theme(axis.title.x = element_text(vjust=-0.6),axis.title.y = element_text(vjust=1))+	#move xylabels away from graph
	#edit legend
	scale_fill_discrete(breaks=c("UnshareSpLoss", "ShareSpLoss"))+
	scale_color_discrete(name="Scenario", labels=c("Independent", "Shared"),breaks=c("UnshareSpLoss", "ShareSpLoss"))+
	theme(legend.position=c(.2, .85))#legend position
	#theme(legend.background = element_rect(colour="grey70", size=.5, linetype="solid"))#add box
	#theme(legend.key = element_blank())#remove boxes from around legend items

outPath <- paste0(outDir, "USG_collab_Shared_vs_Unshared_fig.png")
	ggsave(filename=outPath)
outPath <- paste0(outDir, "USG_collab_Shared_vs_Unshared_fig.pdf")
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
	theme(axis.title.x = element_text(vjust=-0.6),axis.title.y = element_text(vjust=1))+	#move xylabels away from graph
	#edit legend
	scale_fill_discrete(breaks=c("UnshareAgLoss", "ShareAgLoss"))+
	scale_color_discrete(name="Scenario", labels=c("Independent", "Shared"),breaks=c("UnshareAgLoss", "ShareAgLoss"))+
	#theme(legend.position=c(.2, .85))#legend position
	#theme(legend.background = element_rect(colour="grey70", size=.5, linetype="solid"))#add box
	#theme(legend.key = element_blank())#remove boxes from around legend items
	theme(legend.position="none")

outPath <- paste0(outDir, "USG_collab_Shared_vs_Unshared_AgriLoss_fig.png")
	ggsave(filename=outPath)
outPath <- paste0(outDir, "USG_collab_Shared_vs_Unshared_AgriLoss_fig.pdf")
	ggsave(filename=outPath)