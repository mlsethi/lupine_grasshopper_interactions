## Author: Meera Lee Sethi
## Date: 9/8/21
## Script to analyze grasshopper feeding trials  

#Sets up workspace
setwd("~/Google Drive/Research/Plant-Insect Herbivore Interactions/github/lupine_grasshopper_interactions") 
library(sjPlot)
library(sjstats)

#Reads in data
hoppers_raw <- as.data.frame(read.csv("./data/All feeding trials.csv", header=TRUE))

#Removes NAs
hoppers <- hoppers_raw[complete.cases(hoppers_raw[,6]),]

#Makes sure variables are treated as the correct classes with the correct levels
hoppers$Total<- as.numeric(as.character(hoppers$Total))
hoppers$Grasshopper.source <- factor(hoppers$Grasshopper.source, levels=c("Low", "High"))
hoppers$Lupine.source <- factor(hoppers$Lupine.source, levels=c("Low", "High"))
hoppers$Temp <- as.factor(hoppers$Temp)

# Fits three-factor ANOVA
fit <- aov(Total ~ Temp*Grasshopper.source*Lupine.source, data=hoppers)
summary(fit)
anova(fit)

