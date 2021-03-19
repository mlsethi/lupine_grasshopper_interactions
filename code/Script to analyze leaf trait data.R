## Author: Meera Lee Sethi
## Date: 3/19/21
## Script to analyze lupine leaf trait data 

## Sets uo workspace
setwd("~/Google Drive/Research/Plant-Insect Herbivore Interactions/github/lupine_grasshopper_interactions") 
library(factoextra)

#Reads in Leaf Mass per Area (LMA) and Carbon:Nitrogen ratio (CN) data
LMA <- read.csv(file="./data/Lupine_LMA.csv")
CN <- read.csv(file="./data/Lupine_CN.csv")

#Runs linear models to test for the relationships between elevation and LMA and elevation and CN
summary(lm(LMA$log.LMA~LMA$Elevation))
summary(lm(CN$C.N.ratio~CN$Elevation))


#Reads in lupine alkaloid data
alkaloids <- read.csv(file="./data/Lupine_alkaloids.csv", header=TRUE)
alkaloids$Transect <- as.factor(alkaloids$Transect)

#Runs linear models to test for the relationships between elevation and Lupanine and elevation and Sparteine
summary(lm(alkaloids$RT.13.31_Lupanine~alkaloids$Elevation))
summary(lm(alkaloids$sparteine..normalized..conc.adjusted~alkaloids$Elevation))

#Runs PCAs to explore whether alkaloid composition clusters by site

pca.PA <- prcomp(alkaloids[alkaloids$Transect=="PA",][,5:9], scale.=T)
summary(pca.PA)

pca.GB <- prcomp(alkaloids[alkaloids$Transect=="GB",][, c(5:8)], scale.=T)
summary(pca.GB)

PCA_PA <- fviz_pca_ind(pca.PA, geom.ind = "point", pointshape = 21,
             pointsize = 2,
             fill.ind =as.factor(alkaloids$Elevation[alkaloids$Transect=="PA"]),
             col.ind = "black",
             palette = "jco",
             addEllipses = TRUE,
             label = "var",
             col.var = "black",
             repel = TRUE,
             legend.title = "Elevation") +
  ggtitle("South transect") +
  theme(plot.title = element_text(hjust = 0.5))

PCA_PA

PCA_GB <- fviz_pca_ind(pca.GB, geom.ind = "point", pointshape = 21,
             pointsize = 2,
             fill.ind =as.factor(alkaloids$Elevation[alkaloids$Transect=="GB"]),
             col.ind = "black",
             palette = "jco",
             addEllipses = TRUE,
             label = "var",
             col.var = "black",
             repel = TRUE,
             legend.title = "Elevation") +
  ggtitle("East transect") +
  theme(plot.title = element_text(hjust = 0.5))
PCA_GB 




