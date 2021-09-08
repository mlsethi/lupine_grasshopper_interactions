## Author: Meera Lee Sethi
## Date: 9/8/21
## Script to analyze Photosynq data 


setwd("~/Google Drive/Research/Plant-Insect Herbivore Interactions/github/lupine_grasshopper_interactions") 
library(tidyverse)
library(lubridate)
library(ggplot2)
library(car)

# Reads in photosynQ data
photosynq <- read.csv(file="./data/PhotosynQ_data.csv", header=TRUE)

######
# Wrangles data:

#Separates timestamp into date and time, translates time of day into a number (0 to 24)
photosynq$time<- mdy_hm(photosynq$time)
photosynq <- photosynq %>% separate(time, c('Date', 'Time'), sep=" ", remove = TRUE)
photosynq$Time <- strptime(photosynq$Time, "%H:%M:%S")
photosynq$Time <- hour(photosynq$Time)

#separates elevation and plot, makes transect and elevation the correct object types
photosynq <- photosynq %>% separate(Site, c('Transect', 'Elev'), sep="-", remove = TRUE)
photosynq$Transect <- as.factor(photosynq$Transect)
photosynq$Elev <- as.numeric(photosynq$Elev)

# makes damage an ordered factor
photosynq$Damage <- factor(photosynq$Damage, ordered=TRUE, levels=c("None/Low (0-5%)", "Medium (5-25%)", "High (25-100%)"), labels=c("Low", "Med", "High"))
levels(photosynq$Damage) <- list(No="Low", Yes=c("Med", "High"))

#Selects relevant columns and renames for clarity
photosynq <- photosynq[,c(1:8, 58, 59, 0, 61, 75, 76, 77, 79, 80)]

photosynq <- photosynq %>%
  rename(
    PAR = Light.Intensity..PAR.,
    Chlorophyll = Relative.Chlorophyll,
    Leaf_temp_diff = Leaf.Temp.Differential,
    Temp = Ambient.Temperature,
    Humidity = Ambient.Humidity
  )

######
######

#Tests for a relationship between herbivory damage and relative chlorophyll levels
t.test(Chlorophyll~Damage, data=photosynq)

# Transforms PAR to make it linear
photosynq$sqrtPAR <- sqrt(photosynq$PAR)

#Tests whether the slopes of PAR vs. PhiNO are significantly different in damaged vs. undamaged leaves 

Anova(lm(PhiNO ~ sqrtPAR * Damage, data=photosynq, contrasts=list(Damage=contr.sum)), type=3)


