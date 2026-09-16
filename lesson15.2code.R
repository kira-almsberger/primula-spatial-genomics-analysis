setwd("/Users/kalmsberger/Desktop/primula/")

#install packages
remotes::install_github('royfrancis/pophelper')
install.packages("grid")
install.packages("gridExtra")
#load packages
library(grid)
library(gridExtra)
library(pophelper)
library(RColorBrewer)

#Importing data from a modified CLUMPP outfile
K2_Qlist <- readQ(files = "results/clumpp/K2_modified.outfile.txt") #this stores the output to a list
K3_Qlist <- readQ(files = "results/clumpp/K3_modified.outfile.txt")
K6_Qlist <- readQ(files = "results/clumpp/K6_modified.outfile.txt")
K7_Qlist <- readQ(files = "results/clumpp/K7_modified.outfile.txt")

#access data in list (multiple options)
head(K2_Qlist$K2_modified.outfile.txt) #use the name of the item in the list

head(K2_Qlist[[1]]) #use the double-bracket notation to select the first item in the list

head(K2_Qlist[[1]]$Cluster1) #add the column that we want to look at from the item we've selected

#combine four lists into a single list
qlist <- joinQ(K2_Qlist, K3_Qlist, K6_Qlist, K7_Qlist)

head(qlist$K3_modified.outfile.txt) #use the name of the list

head(qlist[[3]]) #use the double-bracket notation - item 3 (K6) in qlist

head(qlist[[4]]$Cluster7) #access a column within a list - item 4 cluster 7

#Names and labels 

tab_pops <- read.csv("PCs.csv", header = TRUE) #import the file
sample.id <- tab_pops$sample.id #store this as a vector

rownames(qlist[[1]]) <- sample.id #add the sample.id vector as the row names for the first data frame
head(qlist[[1]]) #check to see that the names have been added

rownames(qlist[[2]]) <- sample.id
rownames(qlist[[3]]) <- sample.id
rownames(qlist[[4]]) <- sample.id

#import population designations file 
pops <- read.delim("results/clumpp/metadata.txt", header=T,stringsAsFactors=F)

head(pops)

#basic plots 
basic_plot <- plotQ(
  qlist[1], #graph the first dataframe in the qlist, our K2 outfile
  returnplot=T, exportplot=F, #show plot, do not export it. 
  #If you want to export it, you need to provide a path and information 
  #on exporting such as the following line
  #outputfilename="plotq_test",imgtype="png", exportpath=getwd(), #export
  basesize=11,linesize=0.8,pointsize=3,panelratio=c(5,1) #specifying font and plot size info
)

grid.arrange(basic_plot$plot[[1]])

#adding population and region labels
p1 <- plotQ(qlist[1],returnplot=T,exportplot=F,
            grplab=pops[c(1)], #choose dataframe from which to draw label 
            #info such as population. Right now it is taking info from the first column, pop. 
            #To subset by another column, or by both columns, remove "[c(1)]" or change 
            #number 1 to number of the column you want to pull the labels from
            selgrp="pop", subsetgrp=c("SP","GF","TR","AP","HB","SG","NP"), ordergrp=T, #apply labels
            #subsetgrp sets the order in which labels will be listed.
            splab=c("K=2"), #removes the file name that was displayed on the
            #righthand side, and replaces it with a simple "K=2" label.
            grplabsize=3.5, basesize=11, linesize=0.8,pointsize=3,panelratio=c(5,1)) #size info

grid.arrange(p1$plot[[1]])

#adds MN and CA region label 
p2 <- plotQ(qlist[1],returnplot=T,exportplot=F,
            grplab=pops[c(1,2)], #now shows 2 of the columns in this data frame:
            #the populations and MN/CA location
            selgrp="pop", subsetgrp=c("SP","GF","TR","AP","HB","SG","NP"), ordergrp=T, 
            splab=c("K=2"),
            grplabsize=3.5, basesize=11, linesize=0.8,pointsize=3,panelratio=c(5,1)) #size info

grid.arrange(p2$plot[[1]])

#adding individual labels and subsetting populations 
p3 <- plotQ(qlist[1],returnplot=T,exportplot=F,
            grplab=pops[c(1,2)],
            subsetgrp=c("HB", "SG"), selgrp="pop", ordergrp=T, #Only listing pops 
            #you want to show in the subsetgrp argument
            showindlab=T, useindlab=T, #these two functions work together to show individual 
            #label info. 
            splab=c("K=2"),
            grplabsize=3.5,basesize=11,linesize=0.8,pointsize=3,panelratio=c(5,1))

grid.arrange(p3$plot[[1]])

#creating multiple plots
p4 <- plotQ(qlist[c(1:4)], imgoutput="join", #we are using all parts of the qlist 
            # to show the different levels of K, 
            #which are K2, K3, K6, and K7. imgoutput="join" combines 
            #them into a single image stacked on top of each other
            returnplot=T,exportplot=F,
            grplab=pops[c(1,2)],subsetgrp=c("SP","GF","TR","AP","HB","SG","NP"), selgrp="pop", ordergrp=T,
            splab=c("K=2","K=3","K=6", "K=7"),
            grplabsize=3.5,basesize=11,linesize=0.8,pointsize=3,panelratio=c(5,1))

grid.arrange(p4$plot[[1]],ncol=1) #ncol=1 will stack the plots in a single column

#customizing colors
brewer.pal(n = 7, name = "Spectral") #recall this function from a previous lesson to list the Spectral hex codes
"#D53E4F" "#FC8D59" "#FEE08B" "#FFFFBF" "#E6F598" "#99D594" "#3288BD"

p5 <- plotQ(qlist[c(4)], returnplot=T,exportplot=F,basesize=11,
            grplab=pops[c(1)], subsetgrp=c("SP","GF","TR","AP","HB","SG","NP"), selgrp="pop", ordergrp=T,
            clustercol=c("#D53E4F", "#FC8D59", "#FEE08B", "#FFFFBF", "#E6F598", "#99D594", "#3288BD"), #assign color
            showlegend=T, #show legend
            legendkeysize=10,legendtextsize=10,legendmargin=c(2,2,2,0),legendrow=1,
            splab=c("K=7"),
            grplabsize=3.5,linesize=0.8,pointsize=3,panelratio=c(5,1))

grid.arrange(p5$plot[[1]],ncol=1)

p6 <- plotQ(qlist[c(4)], returnplot=T,exportplot=F,basesize=11,
            grplab=pops[c(1)], subsetgrp=c("SP","GF","TR","AP","HB","SG","NP"), selgrp="pop", ordergrp=T,
            clustercol=c("#FFFFBF", "#FEE08B", "#99D594", "#FC8D59",  "#3288BD", "#E6F598", "#D53E4F"), #assign color
            splab=c("K=7"),
            grplabsize=3.5,linesize=0.8,pointsize=3,panelratio=c(5,1))

grid.arrange(p6$plot[[1]],ncol=1)

#sorting by Q-value
p7 <- plotQ(qlist[c(1:2)], imgoutput="join", 
            returnplot=T,exportplot=F,basesize=11,
            grplab=pops[c(1)], subsetgrp=c("SP","GF","TR","AP","HB","SG","NP"),
            selgrp="pop", ordergrp=T,
            sortind="Cluster1", sharedindlab=F, #sort by cluster
            splab=c("K=2", "K=3"), 
            grplabsize=3.5,linesize=0.8,pointsize=3,panelratio=c(5,1))

p8 <- plotQ(qlist[c(1:2)], imgoutput="join", 
            returnplot=T,exportplot=F,basesize=11,
            grplab=pops[c(3)], #change to using the numbered pop labels
            selgrp="pop_n", ordergrp=T,
            sortind="Cluster1", sharedindlab=F, #sort by cluster
            splab=c("K=2", "K=3"),
            grplabsize=3.5,linesize=0.8,pointsize=3,panelratio=c(5,1))

grid.arrange(p7$plot[[1]],p8$plot[[1]],ncol=2)
grid.arrange(p8$plot[[1]])
