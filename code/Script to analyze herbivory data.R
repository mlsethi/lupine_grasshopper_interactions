## Author: Meera Lee Sethi
## Date: 3/19/21
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

#Model selection:

# Creates full ("global") model including all possible predictors and any interactions we think might be relevant

fitmax <- lme(meddamage ~ Elev + Transect + Year + SDDDOY + timesincesdd + Cover+ Elev:Transect + Elev:Year + Elev:SDDDOY + Elev:timesincesdd + Elev:Cover + Transect:Cover + Transect:Year, data=LUPARC_byvisit, method="ML", random=~1|Plot.ID, control = lmeControl(opt = 'optim'))

fitmax2 <- lme(meddamage ~ Elev + Transect + Year + SDDDOY + timesincesdd + Cover+ Elev:Transect + Elev:Year + Elev:SDDDOY + Elev:timesincesdd + Elev:Cover + Transect:Cover, data=LUPARC_byvisit, method="ML", random=~1|Plot.ID, control = lmeControl(opt = 'optim')) #drops Transect:Year interaction

anova(fitmax, fitmax2) # Transect:Year interaction does not contribute to the model fit, so we will drop it.

fitmax3 <- lme(meddamage ~ Elev + Transect + Year + SDDDOY + timesincesdd + Cover+ Elev:Transect + Elev:Year + Elev:SDDDOY + Elev:timesincesdd + Elev:Cover, data=LUPARC_byvisit, method="ML", random=~1|Plot.ID, control = lmeControl(opt = 'optim')) #Drops Transect:Cover interaction

anova(fitmax2, fitmax3) #Transect:Cover interaction does not contribute to the model fit, so we will drop it.

fitmax4 <- lme(meddamage ~ Elev + Transect + Year + SDDDOY + timesincesdd + Cover+ Elev:Transect + Elev:Year + Elev:SDDDOY + Elev:timesincesdd, data=LUPARC_byvisit, method="ML", random=~1|Plot.ID, control = lmeControl(opt = 'optim')) # Drops Elev:Cover interaction

anova(fitmax3, fitmax4) #Elev:Cover interaction does not contribute to the model fit, so we will drop it.

fitmax5 <- lme(meddamage ~ Elev + Transect + Year + SDDDOY + timesincesdd + Cover+ Elev:Transect + Elev:Year + Elev:SDDDOY,  data=LUPARC_byvisit, method="ML", random=~1|Plot.ID, control = lmeControl(opt = 'optim'))  #Drops Elev:timesincesdd

anova(fitmax4, fitmax5) #Elev:timesincesdd interaction does contribute to the model fit, so we will keep it.

fitmax6 <- lme(meddamage ~ Elev + Transect + Year + SDDDOY + timesincesdd + Cover+ Elev:Transect + Elev:Year + Elev:timesincesdd, data=LUPARC_byvisit, method="ML", random=~1|Plot.ID, control = lmeControl(opt = 'optim')) #Drops Elev:SDDDOY interaction

anova(fitmax4, fitmax6) #Elev:SDDDOY interaction does not contribute to the model fit, so we will drop it.

fitmax7 <- lme(meddamage ~ Elev + Transect + Year + SDDDOY + timesincesdd + Cover+ Elev:Transect + Elev:timesincesdd, data=LUPARC_byvisit, method="ML", random=~1|Plot.ID, control = lmeControl(opt = 'optim')) #Drops Elev:Year interaction

anova(fitmax6, fitmax7) #Elev:Year interaction does contribute to the model fit, so we will keep it.

fitmax8 <- lme(meddamage ~ Elev + Transect + Year + SDDDOY + timesincesdd + Cover + Elev:Year + Elev:timesincesdd, data=LUPARC_byvisit, method="ML", random=~1|Plot.ID, control = lmeControl(opt = 'optim')) #Drops Elev:Transect interaction 

anova(fitmax6, fitmax8) #Elev:Transect interaction does contribute to the model fit, so we will keep it.

fitmax9 <- lme(meddamage ~ Elev + Transect + Year + SDDDOY + Elev:Transect + Elev:Year + Elev:timesincesdd, data=LUPARC_byvisit, method="ML", random=~1|Plot.ID, control = lmeControl(opt = 'optim')) #Drops timesincesdd

anova(fitmax6, fitmax9) #timesincesdd does contribute to the model fit, so we will keep it.

fitmax10 <- lme(meddamage ~ Elev + Transect + Year + timesincesdd + Cover+ Elev:Transect + Elev:Year + Elev:timesincesdd, data=LUPARC_byvisit, method="ML", random=~1|Plot.ID, control = lmeControl(opt = 'optim'))

anova(fitmax6, fitmax10) #SDDDOY does contribute to the model fit, so we will keep it.

fitmax11 <- lme(meddamage ~ Elev + Transect + SDDDOY + timesincesdd + Cover+ Elev:Transect + Elev:Year + Elev:timesincesdd, data=LUPARC_byvisit, method="ML", random=~1|Plot.ID, control = lmeControl(opt = 'optim')) #Drops Year

anova(fitmax6, fitmax11) #Year does contribute to the model fit, so we will keep it.

fitmax12 <- lme(meddamage ~ Elev + Year + SDDDOY + timesincesdd + Cover+ Elev:Transect + Elev:Year + Elev:timesincesdd, data=LUPARC_byvisit, method="ML", random=~1|Plot.ID, control = lmeControl(opt = 'optim')) #Drops Transect

anova(fitmax6, fitmax12) #Transect does not contribute to the model fit, so we will drop it.

#Tests for homoscedasticity and normality
plot.lme(fitmax12)
qqnorm(fitmax12)

#Since we know there is heteroscedasticity, refit the model with a modified variance structure that assumes the residuals vary by each level of elevation

fit_varIdent <- lme(meddamage ~Elev + Year + SDDDOY + timesincesdd + Cover+ Elev:Transect + Elev:Year + Elev:timesincesdd, data=LUPARC_byvisit, method="ML", random=~1|Plot.ID, control = lmeControl(opt = 'optim'), weights=varIdent(form=~1|Elev))

fit_varExp <- lme(meddamage ~Elev + Year + SDDDOY + timesincesdd + Cover+ Elev:Transect + Elev:Year + Elev:timesincesdd, data=LUPARC_byvisit, method="ML", random=~1|Plot.ID, control = lmeControl(opt = 'optim'), weights= varExp(form = ~as.vector(Elev)))

fit_varPow <- lme(meddamage ~Elev + Year + SDDDOY + timesincesdd + Cover+ Elev:Transect + Elev:Year + Elev:timesincesdd, data=LUPARC_byvisit, method="ML", random=~1|Plot.ID, control = lmeControl(opt = 'optim'), weights=varPower(form=~as.vector(Elev)))

anova(fitmax2, fit_varIdent, fit_varExp, fit_varPow) #the best variance structure is constant

plot.lme(fit_varIdent) #checks that heteroscedasticity has been lessened
tab_model(fit_varIdent, show.se = T, show.stat = T, digits = 3) #looks at model estimates
