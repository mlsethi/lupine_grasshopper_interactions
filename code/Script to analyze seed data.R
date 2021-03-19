## Author: Meera Lee Sethi
## Date: 3/19/21
## Script to analyze seed predation data 
##

## Sets directory and reads in data:
setwd("~/Google Drive/Research/Plant-Insect Herbivore Interactions/github/lupine_grasshopper_interactions") 
seeds <- read.csv(file="./data/seeds.csv", header=TRUE)
seedsPA <- subset(seeds, Transect=="PA")
seedsGB <- subset(seeds, Transect=="GB")

# Runs linear model to test for relationship/s between elevation, transect, year, and damaged seeds
summary(lm(seeds$Fraction_heavily_damaged~seeds$Elevation +seeds$Transect + seeds$Year))

#Calculates some simple descriptive statistics for the paper
sum(seeds$Fraction_damaged>0)/1279
sum(seedsPA$Fraction_damaged>0)/678
sum(seedsGB$Fraction_damaged>0)/601
mean(seedsPA$Fraction_heavily_damaged[seedsPA$Fraction_heavily_damaged>0])
mean(seedsGB$Fraction_heavily_damaged[seedsGB$Fraction_heavily_damaged>0])

