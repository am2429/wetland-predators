#Import the Data File in CSV Format
PooledYearly <- read.csv("InitialFinalPooled2.csv", header=TRUE)

attach(PooledYearly)

#Visualize data
PooledYearly
summary(PooledYearly)

#Identify Categorical Variables
Trtmnt.f <- factor(PooledYearly$Trtmnt)
Site.f <- factor(PooledYearly$Site)
Block.f <- factor(PooledYearly$Block)
Plot.f <- factor(PooledYearly$Plot)
Year.f <- factor(PooledYearly$Year)

library(car)
library(lme4)
library(lmerTest)
library(multcomp)

#Mixed Effects Model for Burrow Site as Fixed Effect.
BurrowModel <- glmer(BurrowPos ~ Trtmnt.f + Year.f + Trtmnt.f*Year.f + Trtmnt.f*Site.f + Site.f/(1|Block.f), na.action = na.omit, family = Gamma(link = "log"), control=glmerControl(check.conv.singular = .makeCC(action = "ignore",  tol = 1e-4)), data = PooledYearly) 
summary(BurrowModel) 
vcov(BurrowModel)
summary(glht(BurrowModel, linfct=mcp(Site.f="Tukey")))

#Mixed Effects Model for Organic Matter. Site as Fixed Effect.
OMModel2 <- glmer(OMPos ~ Trtmnt.f + Year.f + Trtmnt.f*Year.f + Trtmnt.f*Site.f + Site.f/(1|Block.f), family = Gamma(link = "log"), na.action = na.omit, control=glmerControl(check.conv.singular = .makeCC(action = "ignore",  tol = 1e-4)), data = PooledYearly) 
summary(OMModel2)
summary(glht(OMModel2, linfct=mcp(Site.f="Tukey")))

#Mixed Effects Model for Biomass. Site as Fixed Effect. (Significance at Hamm)
BiomassModel2 <- lmer(Biomass_F ~ Trtmnt.f + Year.f + Trtmnt.f*Year.f + Trtmnt.f*Site.f + Site.f/(1|Block.f), data = PooledYearly) 
summary(BiomassModel2)
summary(glht(BiomassModel2, linfct=mcp(Site.f="Tukey")))

#Mixed Effects Model for Mineralization.. Site as Fixed Effect. (Significance at Hamm)
MineralModel2 <- glmer(MineralAbs ~ Trtmnt.f + Trtmnt.f*Site.f + Site.f/(1|Block.f), family = Gamma(link = "log"), na.action = na.omit, control=glmerControl(check.conv.singular = .makeCC(action = "ignore",  tol = 1e-4)), data = PooledYearly) 
summary(MineralModel2)
summary(glht(MineralModel2, linfct=mcp(Site.f="Tukey")))

#Mixed Effects Model for Nitrogen. Site as Fixed Effect.
NitroModel <- glmer(NitrogenPos ~ Trtmnt.f + Trtmnt.f*Site.f + Site.f/(1|Block.f), family = Gamma(link = "log"), na.action = na.omit, control=glmerControl(check.conv.singular = .makeCC(action = "ignore",  tol = 1e-4)), data = PooledYearly) 
summary(NitroModel)
summary(glht(NitroModel, linfct=mcp(Site.f="Tukey")))

### Analysis of Variance for Variables by Site ###

nitro <- aov(Nitrogen ~ Site.f, data=PooledYearly)
summary(nitro)
TukeyHSD(nitro)
 
mineral <- aov(Mineral ~ Site.f, data=PooledYearly)
summary(mineral)
TukeyHSD(mineral)

om <- aov(OM ~ Site.f, data=PooledYearly)
summary(om)
TukeyHSD(om)

bio <- aov(Biomass_F ~ Year.f, data=PooledYearly)
summary(bio)
TukeyHSD(bio)

### Path Analyses ###

library(lavaan)
library(OpenMx)
library(tidyverse)
library(knitr)
library(kableExtra)
library(GGally)
library(semPlot)

#Construct model
Model <-'Biomass ~ Predator + Burrow + Nitrogen + Mineral
Nitrogen ~ Predator + Burrow + OM
Mineral ~ Predator + Burrow + OM
OM ~ Predator + Burrow + Biomass
Burrow ~ Predator'

#Fit the model 
ModelFit <- cfa(Model, missing = "ML", data = PooledYearly)

#View results
summary(ModelFit, fit.measures = TRUE, standardized=T,rsquare=T)

#Build path diagram
semPaths(ModelFit, 'std', layout='tree')

