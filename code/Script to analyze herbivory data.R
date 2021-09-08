## Author: Meera Lee Sethi
## Date: 4/9/21
## Script to analyze herbivory data 

## Sets up workspace:
setwd("~/Google Drive/Research/Plant-Insect Herbivore Interactions/github/lupine_grasshopper_interactions") 
library(tidyverse)
library(data.table)
library(nlme)

#Reads in Lupine herbivory data
LUPARC <- read.csv("./data/LupineHerbivory.csv", header=TRUE)

# Makes sure categorical predictors are factors
LUPARC$Year <-as.factor(LUPARC$Year)
LUPARC$Transect <-as.factor(LUPARC$Transect)
LUPARC$Plot.ID <-as.factor(LUPARC$Plot.ID)

#Scales and centers numerical predictors
LUPARC$Elev<- scale(LUPARC$Elev)
LUPARC$timesincesdd <- scale(LUPARC$DOY-LUPARC$SDDDOY)
LUPARC$DOY <- scale(LUPARC$DOY)
LUPARC$SDDDOY <- scale(LUPARC$SDDDOY)
LUPARC$Cover <- scale(LUPARC$Cover)

# Calculates  herbivory observed across individual lupine "clumps" per visit to each plot
LUPARC_byvisit <- LUPARC %>% dplyr::group_by(Year, Transect, Elev, Plot.ID, DOY, timesincesdd, Cover, SDDDOY) %>% dplyr::summarize(meandamage=mean(apparentleafdamage), meddamage=median(apparentleafdamage), numobs=n())

LUPARC_byvisit <- as.data.frame(na.omit(LUPARC_byvisit)) #Removes NAs

#####

#Model structure

herbfit <- lme(meddamage ~Elev + Year + SDDDOY + timesincesdd + Elev:Transect + Elev:Year + Elev:SDDDOY, data=LUPARC_byvisit, method="ML", random=~1|Plot.ID, control = lmeControl(opt = 'optim'))


#Tests for homoscedasticity and normality
plot.lme(herbfit)
qqnorm(herbfit)

#Since we know there is heteroscedasticity, refit the model with a modified variance structure that assumes the residuals vary by each level of elevation

fit_varIdent <- lme(meddamage ~Elev + Year + SDDDOY + timesincesdd + Elev:Transect + Elev:Year + Elev:SDDDOY, data=LUPARC_byvisit, method="ML", random=~1|Plot.ID, control = lmeControl(opt = 'optim'), weights=varIdent(form=~1|Elev))

fit_varExp <- lme(meddamage ~Elev + Year + SDDDOY + timesincesdd + Elev:Transect + Elev:Year + Elev:SDDDOY, data=LUPARC_byvisit, method="ML", random=~1|Plot.ID, control = lmeControl(opt = 'optim'), weights= varExp(form = ~as.vector(Elev)))

fit_varPow <- lme(meddamage ~Elev + Year + SDDDOY + timesincesdd + Elev:Transect + Elev:Year + Elev:SDDDOY, data=LUPARC_byvisit, method="ML", random=~1|Plot.ID, control = lmeControl(opt = 'optim'), weights=varPower(form=~as.vector(Elev)))

anova(fitmax2, fit_varIdent, fit_varExp, fit_varPow) #the best variance structure is the one that allows for differences in variance by elevation

plot.lme(fit_varIdent) #checks that heteroscedasticity has been lessened
summary(fit_varIdent)

tab_model(fit_varIdent, digits=3, show.ci = F, show.se=T)
