#Setting up for the lesson

getwd()
setwd("/Users/kalmsberger/Desktop/primula/")
getwd()
list.files("results")

ind_depth <- read.delim("results/depth_stats/geno80ind30.idepth", header = TRUE, stringsAsFactors = FALSE)
colnames(ind_depth)
dim(ind_depth)
nrow(ind_depth)
ncol(ind_depth)

mean(ind_depth$MEAN_DEPTH)
median(ind_depth$MEAN_DEPTH)


#Plotting the data 

ggplot(data = ind_depth)
#gives empty space for a plot

ggplot(data = ind_depth,
       mapping = aes(x = MEAN_DEPTH))
#adding the mapping argument - which gives the variable 

ggplot(data = ind_depth,
       mapping = aes(x = MEAN_DEPTH)) +
  geom_histogram()
#adding the geom function - which tells which shape/type of plot

ggplot(data = ind_depth,
       mapping = aes(x = MEAN_DEPTH)) +
  geom_histogram(binwidth = 5)
#specifying the bin width 

ggplot(ind_depth, aes(x = MEAN_DEPTH)) +
  geom_histogram(binwidth = 5)
#the exact same as the above command but simplified command 

#this adds a vertical line to show the mean 
ggplot(ind_depth, aes(x = MEAN_DEPTH)) +
  geom_histogram(binwidth = 5) +
  geom_vline(aes(xintercept = mean(MEAN_DEPTH)), #adding the vertical line
             color = "red", linetype = "dashed", linewidth = 0.5) #line color, width, etc.

#this makes the graph look nicer and cleaner - but has the whole graph code 
ggplot(ind_depth, aes(x = MEAN_DEPTH)) +
  theme_classic() + #changes the background
  geom_histogram(binwidth = 5) +
  geom_vline(aes(xintercept = mean(MEAN_DEPTH)),
             color = "red", linetype = "dashed", linewidth = 0.5) +
  labs(
    title = "Mean depth of coverage per sample",
    subtitle = "Vertical line indicates overall mean",
    x = "Mean depth of coverage",
    y = "# of samples"
  )


#Locus depth
loc_depth <- read.delim("results/depth_stats/geno80ind30.ldepth.mean", header = TRUE, stringsAsFactors = FALSE)
colnames(loc_depth)
dim(loc_depth)
mean(loc_depth$MEAN_DEPTH)
#37.71177 - more influenced by extreme values
median(loc_depth$MEAN_DEPTH)
#24.6742 - not heavily influence by extreme values 
#this bigger difference between mean and median indicates extreme values are present 

ggplot(loc_depth, aes(x = MEAN_DEPTH)) +
  geom_histogram(binwidth = 3)
#creates histogram of data 

quantile(loc_depth$MEAN_DEPTH, prob = c(0, .25, .50, .75, 1))
#looking at the percentages of data in a quantile
quantile(loc_depth$MEAN_DEPTH, prob = c(.90, .95, .99, 1))
#looking at the distribution of the top 90% of our data 

my_loc_depth <- loc_depth[loc_depth$MEAN_DEPTH < 130,]
#only keep rows where mean depth is below 130 
#130 is chosen from the 99% of our data is 128 mean depth
nrow(loc_depth) - nrow(my_loc_depth)
mean(loc_depth$MEAN_DEPTH)
#37.71177
mean(my_loc_depth$MEAN_DEPTH)
#28.2711
#the new data set mean is sigficantly lower and closer to the median
median(my_loc_depth$MEAN_DEPTH)
#24.54145

ggplot(my_loc_depth, aes(x = MEAN_DEPTH)) +
  geom_histogram(binwidth = 1)
#creates new histogram without extreme values

ggplot(my_loc_depth, aes(x = MEAN_DEPTH)) +
  theme_classic() + #changes the background
  geom_histogram(binwidth = 1) +
  geom_vline(aes(xintercept = mean(MEAN_DEPTH)),
             color = "orange", linetype = "dashed", linewidth = 0.5) +
  labs(
    title = "Mean depth of coverage per site",
    subtitle = "Vertical line indicates overall mean",
    x = "Mean depth of coverage",
    y = "# of samples"
  )
#customize graphs with vertical line, theme, titles, and labels 

#task 4 creating a new vcf file 

ind_depth <- read.delim("results/depth_stats/geno80ind30max130.idepth", header = TRUE, stringsAsFactors = FALSE)

loc_depth <- read.delim("results/depth_stats/geno80ind30max130.ldepth.mean", header = TRUE, stringsAsFactors = FALSE)

mean(ind_depth$MEAN_DEPTH)
median(ind_depth$MEAN_DEPTH)

mean(loc_depth$MEAN_DEPTH)
median(loc_depth$MEAN_DEPTH)

#graph for ind_depth
ggplot(ind_depth, aes(x = MEAN_DEPTH)) +
  theme_classic() + #changes the background
  geom_histogram(binwidth = 5) +
  geom_vline(aes(xintercept = mean(MEAN_DEPTH)),
             color = "pink", linetype = "dashed", linewidth = 0.5) +
  labs(
    title = "Mean depth of coverage per sample",
    subtitle = "Vertical line indicates overall mean",
    x = "Mean depth of coverage",
    y = "# of samples"
  )

#graph for depth per site 
ggplot(loc_depth, aes(x = MEAN_DEPTH)) +
  theme_classic() + #changes the background
  geom_histogram(binwidth = 1) +
  geom_vline(aes(xintercept = mean(MEAN_DEPTH)),
             color = "blue", linetype = "dashed", linewidth = 0.5) +
  labs(
    title = "Mean depth of coverage per site",
    subtitle = "Vertical line indicates overall mean",
    x = "Mean depth of coverage",
    y = "# of samples"
  )
