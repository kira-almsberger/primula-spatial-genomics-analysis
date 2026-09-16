setwd("/Users/kalmsberger/Desktop/primula/")

library("ggplot2")
library("RColorBrewer")

###Importing csv files and saving as vectors

percent.temp <- read.csv("PC_percent.csv", header = TRUE)
tab_pops <- read.csv("PCs.csv", header = TRUE)

pc.percent <- percent.temp$x

#change pop values to a factor; factor specifies an order for the variables***
tab_pops$pop <- factor(tab_pops$pop, levels=c("SP", "GF", "TR", "AP", "HB", "SG", "NP"))

#check levels 
tab_pops$pop

##Building the plot 

#includes data we're plotting and what type of plot 
ggplot(tab_pops, aes(x=EV1, y=EV2, color = pop)) + 
  geom_point()

#adds classic theme and changes point size and transparency
ggplot(tab_pops, aes(x=EV1, y=EV2, color = pop)) +
  theme_classic() +
  geom_point(size = 5, alpha = 0.75)

#changes the axis titles, tick marks, removes legend title 
ggplot(tab_pops, aes(x=EV1, y=EV2, color = pop)) +
  theme_classic() +
  geom_point(size = 5, alpha = 0.75) +
  theme(
    axis.text = element_text(face = "bold",size = 14), #changes the tick mark font
    axis.title = element_text(face = "bold", size = 16), #changes the axis title font
    legend.title = element_blank() #takes away the legend title
  )

#to view R brewer color palettes 
display.brewer.all()

#changes colors where cool colors = north and warm colors = south using a palette of colors from R brewer 
ggplot(tab_pops, aes(x=EV1, y=EV2, color = pop)) +
  theme_classic() +
  geom_point(size = 5, alpha = 0.75) +
  theme(
    axis.text = element_text(face = "bold",size = 14),
    axis.title = element_text(face = "bold", size = 16),
    legend.title = element_blank()
  ) +
  scale_color_brewer(palette = "Spectral") #use this palette to color by population

#display 7 sevens in palette 
display.brewer.pal(n = 7, name = "Spectral")

#gives hex code values of colors in palette 
brewer.pal(n = 7, name = "Spectral")

[1] "#D53E4F" "#FC8D59" "#FEE08B" "#FFFFBF" "#E6F598" "#99D594"
[7] "#3288BD"

#add percent variation to axis titles 
PC1.per <- round(pc.percent[1], 2) #take the first element, round it to 2, and store it
PC2.per <- round(pc.percent[2], 2) #take the second element, round it to 2, and store it

ggplot(tab_pops, aes(x=EV1, y=EV2, color = pop)) +
  theme_classic() +
  geom_point(size = 5, alpha = 0.75) +
  theme(
    axis.text = element_text(face = "bold",size = 14),
    axis.title = element_text(face = "bold", size = 16),
    legend.title = element_blank()
  ) +
  scale_color_brewer(palette = "Spectral") +
  labs(
    x = paste("PC1 - ", PC1.per, "%", sep = ""), #add x-axis label with a percentage
    y = paste("PC2 - ", PC2.per, "%", sep = "") #add y-axis label with a percentage
  )

### TASK 2 ###

setwd("/Users/kalmsberger/Desktop/primula/")

library("ggplot2")
library("RColorBrewer")

###Importing csv files and saving as vectors

percent.temp <- read.csv("PC_percent_MNonly.csv", header = TRUE)
tab_pops <- read.csv("PCs_MNonly.csv", header = TRUE)

pc.percent <- percent.temp$x

#change pop values to a factor; factor specifies an order for the variables***
tab_pops$pop <- factor(tab_pops$pop, levels=c("SP", "GF", "TR", "AP", "HB"))

#add percent variation to axis titles 
PC1.per <- round(pc.percent[1], 2) #take the first element, round it to 2, and store it
PC2.per <- round(pc.percent[2], 2) #take the second element, round it to 2, and store it

#building the plot 
ggplot(tab_pops, aes(x=EV1, y=EV2, color = pop)) +
  theme_classic() +
  geom_point(size = 5, alpha = 0.75) +
  theme(
    axis.text = element_text(face = "bold",size = 14),
    axis.title = element_text(face = "bold", size = 16),
    legend.title = element_blank()
  ) +
  scale_color_manual(values=c("#D53E4F", "#FC8D59", "#FEE08B", "#FFFFBF", "#E6F598"), #use the first five hexcodes from the list you generated earlier
                     labels=c("SP", "GF", "TR", "AP", "HB"), #names of the MN populations in the order corresponding to the hexcodes
                     name="") +
  labs(
    x = paste("PC1 - ", PC1.per, "%", sep = ""), #add x-axis label with a percentage
    y = paste("PC2 - ", PC2.per, "%", sep = "") #add y-axis label with a percentage
  )
