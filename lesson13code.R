setwd("/Users/kalmsberger/Desktop/primula/")

#install packages
install.packages("vcfR")
install.packages("reshape2")
#load packages
library(ggplot2)
library(RColorBrewer)
library(reshape2)
library(vcfR)

tab_pops <- read.csv("PCs.csv", header = TRUE)
tab_pops$pop <- factor(tab_pops$pop, levels=c("SP", "GF", "TR", "AP", "HB", "SG", "NP"))
pop <- tab_pops$pop #now just making a vector with all the population designations in it

vcf <- read.vcfR("data/geno80ind30max130.recode.vcf")

myDiff <- genetic_diff(vcf, pops = pop, method = "nei")

colnames(myDiff)
dim(myDiff)

head(myDiff[,c(1:9)])

#na.rm means 'remove missing values'
(round(colMeans(myDiff[,c(3:9)], na.rm = TRUE), digits = 3))

write.csv(myDiff, file = "results/Hs.csv")

dpf <- melt(myDiff[,c(3:9)], variable.name = ("Population"), value.name = 'Hs', na.rm=TRUE) #reshape data table to long format for graphing

dim(myDiff)
  3172*7
  22204
dim(dpf)
  22200
sum(is.na(myDiff[,c(3,9)]))
  [1] 4

levels(dpf$Population)


##VIOLIN PLOT

ggplot(dpf, aes(x=Population, y=Hs, fill=Population)) + 
  geom_violin() +
  theme_classic() +
  scale_fill_brewer(palette = "Spectral") +     #fill using the Spectral colors
  theme(axis.title = element_text(size = 16, face= "bold"), #change size of axis titles
        axis.text = element_text(size = 12, face = "bold"), #change size of axis tick labels
        legend.position = "none") +   #remove legend
  labs(
    y = expression(bold(H[S])) #turn the "S" in "Hs" into a subscript, and keep the text bold
  ) +
  stat_summary(fun = median, geom="point", shape=23, size=4, fill="black") +
  scale_x_discrete(labels=c("Hs_SP" = "SP", "Hs_GF" = "GF",
                              "Hs_TR" = "TR","Hs_AP" = "AP","Hs_HB" = "HB","Hs_SG" = "SG","Hs_NP" = "NP"))

##DENSITY PLOT

ggplot(dpf, aes(x=Hs, color=Population, fill=Population)) +
  geom_density(alpha = 0.25) + #alpha makes the fill transparent
  theme_classic() +
  scale_fill_brewer(palette = "Spectral", 
                    labels = c("SP", "GF", "TR", "AP", "HB", "SG", "NP")) +
  scale_color_brewer(palette = "Spectral", 
                     labels = c("SP", "GF", "TR", "AP", "HB", "SG", "NP")) +
  theme(
    axis.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 12, face = "bold"),
    legend.title = element_blank(), #remove legend title
    legend.position = c(.8,.8), #move the legend inside the plot using x,y coordinates
    legend.text = element_text(size = 12) #increase legend text size
  ) +
  labs(
    y = "Site density",
    x = expression(bold(H[S]))
  )

##TASK 4/Practicum 13

tab_pops <- read.csv("PCs.csv", header = TRUE)
tab_pops$pop <- factor(tab_pops$pop, levels=c("SP", "GF", "TR", "AP", "HB", "SG", "NP"))
pop <- tab_pops$pop #now just making a vector with all the population designations in it

vcf <- read.vcfR("data/geno80ind30max130.recode.vcf")

myDiff <- genetic_diff(vcf, pops = pop, method = "nei")

dpf <- melt(myDiff[,c(3,9)], variable.name = ("Population"), value.name = 'Hs', na.rm=TRUE) #reshape data table to long format for graphing

ggplot(dpf, aes(x=Hs, color=Population, fill=Population)) +
  geom_density(alpha = 0.25) + #alpha makes the fill transparent
  theme_classic() +
  scale_color_manual(values=c("#D53E4F", "#3288BD"), 
                     labels = c("SP", "NP")) +
  scale_fill_manual(values=c("#D53E4F", "#3288BD"), 
                     labels = c("SP", "NP")) +
  theme(
    axis.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 12, face = "bold"),
    legend.title = element_blank(), #remove legend title
    legend.position = c(.8,.8), #move the legend inside the plot using x,y coordinates
    legend.text = element_text(size = 12) #increase legend text size
  ) +
  labs(
    y = "Site density",
    x = expression(bold(H[S]))
  )
