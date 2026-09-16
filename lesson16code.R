setwd("/Users/kalmsberger/Desktop/primula/")

#install packages
install.packages("grid")
install.packages("gridExtra")
install.packages("lattice")
#load packages
library(ggplot2)
library(RColorBrewer)
library(ggrepel)
library(ggspatial)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)
library(maps)
library(gridExtra)
library(grid)
library(lattice)
#library(viridis) **may need to install later

#created colored rectangles (grobs)
rec1 <- rectGrob(gp=gpar(fill="red"))
rec2 <- rectGrob(gp=gpar(fill="orange"))
rec3 <- rectGrob(gp=gpar(fill="yellow"))
rec4 <- rectGrob(gp=gpar(fill="green"))
rec5 <- rectGrob(gp=gpar(fill="blue"))
rec6 <- rectGrob(gp=gpar(fill="violet"))
rec7 <- rectGrob(gp=gpar(fill="grey"))

#use 4 rectangles, put into 2 columns
grid.arrange(rec1, rec2, rec3, rec4, ncol=2)

#use 6 rectangles, put into 2 rows
grid.arrange(rec1, rec2, rec3, rec4, rec5, rec6, nrow=2)

#specify a layout in form of data frame 
lay <- rbind (c(1,1),
              c(2,3))
lay

#use lay dataframe to create plot - rec7 = 1 rec5 = 2 rec6 = 3
grid.arrange(rec7, rec5, rec6, layout_matrix = lay)

#don't need a plot in every available cell 
lay <- rbind (c(1,1),
              c(2,NA))
lay
grid.arrange(rec7, rec5, layout_matrix = lay) #bottom right is empty

#matrix of 7 cells in portrait orientation 
lay <- rbind(c(1,2),
             c(3,4),
             c(5,6),
             c(7,7))
lay
grid.arrange(rec1, rec2, rec3, rec4, rec5, rec6, rec7, layout_matrix = lay)

##TASK 1 Adding maps to plot##

#MAP
#import the Pop_info data and set the levels for the populations
Pop_info <- read.csv("results/Pop_info.csv", header = TRUE)
Pop_info$Pop <- factor(Pop_info$Pop, levels = c("SP", "GF", "TR", "AP", "HB", "SG", "NP"))

#generate the "ne_states" data frame
ne_states <- ne_states(country = c("United States of America", "Canada"), returnclass = "sf")

Map <- ggplot() +
  geom_sf(data = ne_states,
          mapping = aes(geometry = geometry),          
          color = "black", 
          fill = "gray90") +
  coord_sf(xlim = c(-95, -84), 
           ylim = c(44.5, 50), 
           expand = FALSE) +
  theme_bw() +
  theme(axis.title = element_text(face = "bold", size = 14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none") +
  geom_point(data = Pop_info, mapping = aes(x = Longitude, y = Latitude, color = Pop), size = 4) +
  scale_color_brewer(palette = "Spectral") +
  geom_label_repel(data = Pop_info, aes(x = Longitude, y = Latitude, label= Pop),
                   nudge_y = 0.5,
                   segment.colour = 'grey50') +
  annotation_scale(location = "bl", width_hint = 0.5)

Map

#replace rec1 with Map to import our Map into layout 
grid.arrange(Map, rec2, rec3, rec4, rec5, rec6, rec7, layout_matrix = lay)

