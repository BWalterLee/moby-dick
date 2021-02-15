# Call me Ishmael 2/14/2021

library(tidyverse)
library(piecewiseSEM)
library(lme4)
library(car)
library(MASS)
library(corrplot)



pot_sum = read.csv("../data/pot_sum.csv", sep = ",", header = T) %>% 
  mutate(id = 1:n(), Organic = if_else(manage == "Organic",1,0)) %>% 
  dplyr::filter(!is.na(year))

pot_cor_sub = cor(pot_sum %>% 
  dplyr::select(Organic, Nitrate:TotSimp, -Soilclass))

corrplot(pot_cor_sub, method = "circle")

View(pot_sum)
str(pot_sum)

geo_gut <- pot_sum %>% 
  dplyr::select(1:2,id,3:11)

nab_gut <- pot_sum %>% 
  dplyr::select(1:2,id,12:22)

head(geo_gut)
head(nab_gut)


# SEMs investigating predation effects on prey SEPERATELY
aph_data <- pot_sum %>% 
  dplyr::select(-6:-11,-15:-20, -26:-28) %>% 
  dplyr::filter(AphidAbundance <= 5000) %>% 
  dplyr::mutate(geo.prop.con = (GeoAphYes/NumberGeoTested),
                nab.prop.con = (NabAphidYes/NumberNabTested),
                log.aphid.abundance = ifelse(AphidAbundance == 0, 0, log(AphidAbundance)))
head(aph_data)
#  First shot is 100% all factors model
mean(aph_data$AphidAbundance)
var( aph_data$AphidAbundance)
hist(log(aph_data$AphidAbundance))

# abiotic responses
nitrate_glm  <- glm.nb(Nitrate ~ Organic, data = aph_data)
orgmatter_glm<- glm(OrgMatter  ~ Organic, data = aph_data)
pH_glm       <- glm(pH         ~ Organic, data = aph_data)

# biotic responses
tot_abun_glm   <- glm.nb(TotAbund ~ Organic + Nitrate + OrgMatter + pH, data = aph_data)
tot_richness_glm<-glm(TotRich ~ Organic + Nitrate + OrgMatter + pH, data = aph_data)

geo_pred_glm <- glm(geo.prop.con  ~ pH + Nitrate + OrgMatter + TotAbund + TotRich, 
                    family = "binomial", weights = NumberGeoTested,data = aph_data)

nab_pred_glm <- glm(nab.prop.con ~ pH + Nitrate + OrgMatter + TotAbund + TotRich, 
                    family = "binomial", weights = NumberNabTested,data = aph_data)

# Aphid response
aph_abun_glm <- glm(log.aphid.abundance ~ geo.prop.con + nab.prop.con + 
                    pH + Nitrate + OrgMatter + TotAbund + TotRich,
                    data = aph_data)
summary(aph_abun_glm)

# First Run\
# Nitrate, Organic Matter, and pH all super correlated, so revised model to include only Org matter. 
# Could change to focus on alternate questions of interest
sem.1 <- psem(nitrate_glm,
           orgmatter_glm,
                  pH_glm,
            tot_abun_glm,
        tot_richness_glm,
            geo_pred_glm,
            nab_pred_glm,
            aph_abun_glm, OrgMatter %~~% Nitrate, pH %~~% Nitrate, pH %~~%OrgMatter)
summary(sem.1)
# Nitrate, Organic Matter, and pH all super correlated, so revised model to include only Org matter. 
# Could change to focus on alternate questions of interest

#
orgmatter_glm2<- glm(OrgMatter  ~ Organic, data = aph_data)

tot_abun_glm2   <- glm.nb(TotAbund ~ Organic + OrgMatter, data = aph_data)
tot_richness_glm2<-glm(TotRich ~ Organic +OrgMatter, data = aph_data)

geo_pred_glm2 <- glm(geo.prop.con ~  OrgMatter + TotAbund + TotRich, 
                     family = "binomial", weights = NumberGeoTested, data = aph_data)
nab_pred_glm2 <- glm(nab.prop.con ~ OrgMatter + TotAbund + TotRich, 
                     family = "binomial", weights = NumberNabTested, data = aph_data)

aph_abun_glm2 <- glm(log.aphid.abundance ~ Organic + geo.prop.con + nab.prop.con + 
                       OrgMatter + TotAbund + TotRich,
                    data = aph_data)
sem.2 <- psem(   orgmatter_glm2,
                  tot_abun_glm2,
              tot_richness_glm2,
                  geo_pred_glm2,
                  nab_pred_glm2,
                  aph_abun_glm2, TotRich %~~% TotAbund, nab.prop.con %~~% geo.prop.con)
summary(sem.2)

# Updates based on p <.5 removal from path and p <.1 addition from dsep
orgmatter_glm2.1<- glm(OrgMatter  ~ Organic, data = aph_data)

tot_abun_glm2.1   <- glm.nb(TotAbund ~ Organic , data = aph_data)
tot_richness_glm2.1<-glm(TotRich ~ Organic +OrgMatter, data = aph_data)

geo_pred_glm2.1 <- glm(geo.prop.con ~  Organic + OrgMatter + TotAbund, 
                     family = "binomial", weights = NumberGeoTested, data = aph_data)
nab_pred_glm2.1 <- glm(nab.prop.con ~ Organic + OrgMatter , 
                     family = "binomial", weights = NumberNabTested, data = aph_data)

aph_abun_glm2.1 <- glm(log.aphid.abundance ~ Organic + nab.prop.con + 
                      OrgMatter + TotAbund + TotRich,
                     data = aph_data)

sem.2.1 <- psem(orgmatter_glm2.1,
                 tot_abun_glm2.1,
             tot_richness_glm2.1,
                 geo_pred_glm2.1,
                 nab_pred_glm2.1,
                 aph_abun_glm2.1,TotRich %~~% TotAbund, nab.prop.con %~~% geo.prop.con)
summary(sem.2.1, direction = c("TotAbund <- OrgMatter"))

