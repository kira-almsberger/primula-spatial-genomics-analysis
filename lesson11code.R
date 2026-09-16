setwd("/Users/kalmsberger/Desktop/primula/")

library("SNPRelate")

##loading and checking the data from primula 

#First save the path to the vcf as a variable named "vcf_import.fn"
vcf_import.fn <- "data/geno80ind30max130.recode.vcf"

#Now convert the vcf to a GDS file, which is the file type used by SNPRelate
snpgdsVCF2GDS(vcf_import.fn, "vcf", method = "biallelic.only", ignore.chr.prefix = "Scaffold-", verbose = TRUE)

#Double-check the file you just imported to make sure it has the expected number of individuals
snpgdsSummary("vcf")

#Now store the GDS file as an object for manipulation 
genofile <- snpgdsOpen("vcf")

#Double-check that the genofile you just created has the expected number of individuals
snpgdsSummary(genofile)

##Running a PCA

#create principal components to reduce amount of variables 
pca <- snpgdsPCA(genofile)

#check the sample names
pca$sample.id

#use the substr command to extract the first two characters of each sample name and saving it as a vector; this separates each sample to just it's location
pop <- substr(pca$sample.id, 1, 2)

#save the populations to a csv to use in future analyses
write.csv(pop, "pop.csv", row.names = FALSE)

#extract the variation explained from the PCA multiply it by 100 to get a percentage
pc.percent <- pca$varprop*100

#look at it using the command "head"
head(pc.percent)

#now we're looking at it again, but rounding everything to one significant digit
head(round(pc.percent, 1))

#save the PC percentages to a csv to use in future analyses
write.csv(pc.percent, "PC_percent.csv", row.names = FALSE)

head(pca$eigenvect)

##Plotting a PCA

#combine sample names and PCs (eigenvectors) into a data frame
tab <- data.frame(sample.id = pca$sample.id,  # the sample names labeled as sample.id
                  EV1 = pca$eigenvect[,1],    # the first eigenvect labeled as EV1
                  EV2 = pca$eigenvect[,2],    # the second eigenvect labeled as EV2
                  stringsAsFactors = FALSE)

#look at it to check it out
head(tab)

#check the number of columns and rows
dim(tab)

#makes a basic plot of the first 2 PCs
#values are arbitrary, they just show a difference among values
plot(tab$EV1, tab$EV2, xlab="PC1", ylab="PC2")

#visually check to make sure everything matches up between the 'tab' data frame and the populations using the "cbind" command that combines columns from different sources
head(cbind(tab, pop))

#match up with the sample IDs, make a new data frame where you include the population codes as a new column
tab_pops <- cbind(tab, pop)

#change pop values to a factor; factor specifies an order for the variables***
tab_pops$pop <- factor(tab_pops$pop, levels=c("SP", "GF", "TR", "AP", "HB", "SG", "NP"))
#we'll check to make sure the levels are present by viewing the values
tab_pops$pop

#saving the PCs to a csv to use in future analyses
write.csv(tab_pops, "PCs.csv", row.names = FALSE)

#plot it using the basic plot function but adding colors for each population
plot(tab_pops$EV1, tab_pops$EV2, col=as.integer(tab_pops$pop), xlab="PC 1", ylab="PC 2")

#add a legend
#you may need to change the position of the legend to "topright", "bottomleft", or "bottomright" depending on the location of the points in your plot
legend("topleft", legend=levels(tab_pops$pop), pch="o", col=1:nlevels(tab_pops$pop))

snpgdsClose(genofile)

###TASK 2 - making MN populations only PCA

setwd("/Users/kalmsberger/Desktop/primula/")

library("SNPRelate")

##loading and checking the data from primula 

#First save the path to the vcf as a variable named "vcf_import.fn"
vcf_import.fn <- "data/geno80ind30max130_MNonly.recode.vcf"

#Now convert the vcf to a GDS file, which is the file type used by SNPRelate
snpgdsVCF2GDS(vcf_import.fn, "vcf", method = "biallelic.only", ignore.chr.prefix = "Scaffold-", verbose = TRUE)

#Now store the GDS file as an object for manipulation 
genofile <- snpgdsOpen("vcf")

##Running a PCA

#create principal components to reduce amount of variables 
pca <- snpgdsPCA(genofile)

#use the substr command to extract the first two characters of each sample name and saving it as a vector; this separates each sample to just it's location
pop <- substr(pca$sample.id, 1, 2)

#save the populations to a csv to use in future analyses
write.csv(pop, "pop_MNonly.csv", row.names = FALSE)

#extract the variation explained from the PCA multiply it by 100 to get a percentage
pc.percent <- pca$varprop*100

#now we're looking at it again, but rounding everything to one significant digit
head(round(pc.percent, 1))

#save the PC percentages to a csv to use in future analyses
write.csv(pc.percent, "PC_percent_MNonly.csv", row.names = FALSE)

##Plotting a PCA

#combine sample names and PCs (eigenvectors) into a data frame
tab <- data.frame(sample.id = pca$sample.id,  # the sample names labeled as sample.id
                  EV1 = pca$eigenvect[,1],    # the first eigenvect labeled as EV1
                  EV2 = pca$eigenvect[,2],    # the second eigenvect labeled as EV2
                  stringsAsFactors = FALSE)

#makes a basic plot of the first 2 PCs
#values are arbitrary, they just show a difference among values
plot(tab$EV1, tab$EV2, xlab="PC1", ylab="PC2")

#match up with the sample IDs, make a new data frame where you include the population codes as a new column
tab_pops <- cbind(tab, pop)

#change pop values to a factor; factor specifies an order for the variables***
tab_pops$pop <- factor(tab_pops$pop, levels=c("SP", "GF", "TR", "AP", "HB"))
#we'll check to make sure the levels are present by viewing the values
tab_pops$pop

#saving the PCs to a csv to use in future analyses
write.csv(tab_pops, "PCs_MNonly.csv", row.names = FALSE)

#plot it using the basic plot function but adding colors for each population
plot(tab_pops$EV1, tab_pops$EV2, col=as.integer(tab_pops$pop), xlab="PC 1", ylab="PC 2")

#add a legend
#you may need to change the position of the legend to "topright", "bottomleft", or "bottomright" depending on the location of the points in your plot
legend("topright", legend=levels(tab_pops$pop), pch="o", col=1:nlevels(tab_pops$pop))
