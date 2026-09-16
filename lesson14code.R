setwd("/Users/kalmsberger/Desktop/primula/")

#load packages
library(ggplot2)
library(RColorBrewer)
library(reshape2)
library(ggpubr)
library(tidyverse)
library(ggpmisc)
library(ggrepel)
library(ggspatial)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)
library(maps)
library(remotes)

#importing Hs.csv file
myDiff <- read.csv("results/Hs.csv", header = TRUE)
#note we have an extra column of numbers at the beginning of the file; let's remove it by retaining only columns 2-22 as "myDiff"
myDiff <- myDiff[,c(2:22)]

#melt myDiff
dpf <- melt(myDiff[,c(3:9)], variable.name = ("Population"), value.name = 'Hs', na.rm=TRUE)

#import latlong.csv file
latlong <- read.csv("data/LatLong.csv", header = TRUE)

#summary of the mean HS values, but in wide format which isn't ideal 
#na.rm means 'remove missing values'
(round(colMeans(myDiff[,c(3:9)], na.rm = TRUE), digits = 3))

by_pop <- group_by(dpf, Population) #group by population and save as a new variable
levels(by_pop$Population) #check that the populations have levels

summary(by_pop$Hs) #summary of Hs of all populations 
summarise(by_pop, meanHs = mean(Hs, na.rm = TRUE)) 

meanHs <- summarise(by_pop, meanHs = mean(Hs, na.rm = TRUE)) #displays table of mean Hs values for each pop and save as new variable
head(meanHs)

medianHs <- summarise(by_pop, medianHs = median(Hs, na.rm = TRUE))#displays table of median Hs values for each pop and save as new variable
head(medianHs)

#extract last two characters 
Pop <- substr(meanHs$Population, 4, 5)

#combine several vectors into onto data frame
Hs_sum <- data.frame(Pop = Pop,  # the population names
                     Population = meanHs$Population, # the Hs_population names for reference
                     meanHs = meanHs$meanHs,
                     medianHs = medianHs$medianHs)

dim(Hs_sum)

#combine Hs_sum and latlong data frames 
Pop_info <- merge(Hs_sum, latlong, by="Pop")
Pop_info

#add levels 
Pop_info$Pop <- factor(Pop_info$Pop, levels = c("SP", "GF", "TR", "AP", "HB", "SG", "NP"))

#saves new dataframe as csv file
write.csv(Pop_info, "results/Pop_info.csv")

##GRAPHING
#ggscatter
ggscatter(Pop_info, x = "Latitude", y= "meanHs",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson")

#ggplot & adding geom_smooth
ggplot(Pop_info, aes(x=Latitude, y=meanHs)) + 
  geom_smooth(method = "lm", color = "black", fill = "grey95", alpha = 0.5) + 
  #put the geom_smooth first in the plot, so that it goes UNDER the points
  theme_classic() +
  geom_point(aes(color = Pop), size = 6) +
  scale_color_brewer(palette = "Spectral") +
  theme(
    axis.text = element_text(face = "bold",size = 14),
    axis.title = element_text(face = "bold", size = 16),
    legend.title = element_blank(), #remove legend title
    legend.text = element_text(size = 12) #increase legend text size 
  ) +
  labs(
    y = expression(bold(H[S])) 
  )

##TASK 1##
ggplot(Pop_info, aes(x=Latitude, y=meanHs)) + 
  geom_smooth(method = "lm", color = "black", fill = "grey95", alpha = 0.5) + 
  #put the geom_smooth first in the plot, so that it goes UNDER the points
  theme_classic() +
  geom_point(aes(color = Pop), size = 6) +
  scale_color_brewer(palette = "Spectral") +
  theme(
    axis.text = element_text(face = "bold",size = 14),
    axis.title = element_text(face = "bold", size = 16),
    legend.title = element_blank(), #remove legend title
    legend.text = element_text(size = 12) #increase legend text size 
  ) +
  labs(
    y = expression(bold(H[S])) 
  ) +
  ggpmisc::stat_correlation(use_label("R", "P"))
  
###MAPPING

#Mapping basics 
world <- ne_countries(scale = "medium", returnclass = "sf")
colnames(world)

ggplot(world, aes(geometry = geometry)) +
  theme_bw() +
  geom_sf()

ggplot(world, aes(geometry = geometry)) +
  theme_bw() +
  geom_sf(color = "grey", #map outline
          fill = "lavender") #map fill 

#zoom in on region using lat and longitude with coor_sf
ggplot(world, aes(geometry = geometry)) +
  theme_bw() +
  geom_sf() +
  coord_sf(xlim = c(-102.15, -74.12), #longitude
           ylim = c(7.65, 33.97), #latitude
           expand = FALSE)

#add scale bar and arrow with “annotation_scale” and “annotation_north_arrow”
ggplot(world, aes(geometry = geometry)) +
  theme_bw() +
  geom_sf() +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-102.15, -74.12), 
           ylim = c(7.65, 33.97), 
           expand = FALSE)

##Mapping primula 
ggplot(world, aes(geometry = geometry)) +
  theme_bw() +
  geom_sf() +
  coord_sf(xlim = c(-92.5, -85), #longitude
           ylim = c(44.5, 49), #latitude
           expand = FALSE) #this didn't show Lake Superior/Canada reference very well; switch to ne_states 

#stores outlines of different states
ne_states <- ne_states(country = c("United States of America", "Canada"), returnclass = "sf")

#basic plot of ne_states & remove grid lines from lake superior 
ggplot(ne_states, aes(geometry = geometry)) +
  geom_sf(color = "black", 
          fill = "gray90") +
  coord_sf(xlim = c(-92.5, -85), 
           ylim = c(44.5, 49), 
           expand = FALSE) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")

#map longitude and latitude for each population from "Pop-info"; using two different data sets on one map 
ggplot() +
  geom_sf(data = ne_states,
          mapping = aes(geometry = geometry),          
          color = "black", 
          fill = "gray90") +
  coord_sf(xlim = c(-92.5, -85), 
           ylim = c(44.5, 49.5), 
           expand = FALSE) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none") +
  geom_point(data = Pop_info, mapping = aes(x = Longitude, y = Latitude), size = 4)

#adding labels with “geom_label_repel” above the data points with “nudge_y”
ggplot() +
  geom_sf(data = ne_states,
          mapping = aes(geometry = geometry),          
          color = "black", 
          fill = "gray90") +
  coord_sf(xlim = c(-92.5, -85), 
           ylim = c(44.5, 49.5), 
           expand = FALSE) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none") +
  geom_point(data = Pop_info, mapping = aes(x = Longitude, y = Latitude), size = 4) +
  geom_label_repel(data = Pop_info, aes(x = Longitude, y = Latitude, label= Pop),
                   nudge_y = 0.5,
                   segment.colour = 'grey50')

##TASK 2## dont forget ne_states
ggplot() +
  geom_sf(data = ne_states,
          mapping = aes(geometry = geometry),          
          color = "black", 
          fill = "gray90") +
  coord_sf(xlim = c(-92.5, -85), 
           ylim = c(44.5, 49.5), 
           expand = FALSE) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none") +
  geom_point(data = Pop_info, mapping = aes(x = Longitude, y = Latitude, color = Pop), size = 4) +
  scale_color_brewer(palette = "Spectral") +
  geom_label_repel(data = Pop_info, aes(x = Longitude, y = Latitude, label= Pop),
                   nudge_y = 0.5,
                   segment.colour = 'grey50') +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
  