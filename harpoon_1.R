# Call me Ishmael 2/14/2021

library(tidyverse)
library(piecewiseSEM)
library(lme4)
library(car)
library(MASS)



pot_sum = read.csv("../data/pot_sum.csv", sep = ",", header = T) %>% 
  mutate(id = 1:n()) %>% 
  dplyr::filter(!is.na(year))

View(pot_sum)
str(pot_sum)

geo_gut <- pot_sum %>% 
  dplyr::select(1:2,id,3:11)

nab_gut <- pot_sum %>% 
  dplyr::select(1:2,id,12:22)

head(geo_gut)
head(nab_gut)

geo_tall <- geo_gut %>% 
  pivot_longer(cols = 5:6, names_to = "test.sp",values_ )

# SEMs investigating predation effects on prey SEPERATELY
aph_data <- pot_sum %>% 
  dplyr::select(-6:-11,-15:-20, -26:-28) %>% 
  dplyr::filter(AphidAbundance <= 5000)
head(aph_data)
#  First shot is 100% all factors model
mean(aph_data$AphidAbundance)
var( aph_data$AphidAbundance)
hist(log(aph_data$AphidAbundance))

# abiotic responses
nitrate_glm  <- glm.nb(Nitrate ~ manage, data = aph_data)
orgmatter_glm<- glm(OrgMatter  ~ manage, data = aph_data)
pH_glm       <- glm(pH         ~ manage, data = aph_data)

# biotic responses
tot_abun_glm   <- glm.nb(TotAbund ~ manage + Nitrate + OrgMatter + pH, data = aph_data)
tot_richness_glm<-glm(TotRich ~ manage + Nitrate + OrgMatter + pH, data = aph_data)
summary(tot_abun_glm)
summary(tot_richness_glm)

geo_pred_glm <- glm(cbind(GeoAphYes,GeoAphNo) ~ pH + Nitrate + OrgMatter + TotAbund + TotRich, family = "binomial", data = aph_data)
summary(geo_pred_glm)
nab_pred_glm <- glm(cbind(NabAphidYes, NabAphidNo) ~ pH + Nitrate + OrgMatter + TotAbund + TotRich, family = "binomial", data = aph_data)
summary(nab_pred_glm)

# Aphid reponse
aph_abun_glm <- glm(log(AphidAbundance) ~ cbind(GeoAphYes,GeoAphNo) + cbind(NabAphidYes, NabAphidNo) + 
                    pH + Nitrate + OrgMatter + TotAbund + TotRich,
                    data = aph_data)
