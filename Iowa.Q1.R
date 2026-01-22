###############################################################################
# Iowa Project - Q1 Reading Ability and Frequency
# 
# Our first question related to the longitudinal associations between Reading
# Fluency and Reading Frequency (both Shared book reading, SBR, and Independent
# reading, I)
# 
# Created: 2026-01-22
# Author: Serje Robidoux
# CHANGELOG:

# 0.0 Setup ####
include(lavaan)

# 1.0 Read Data ####

source("Iowa.Data.R")

## need to add "Interest" at some point.
q1Data = iowaData %>% select(participant, Grade, Fi, Fsbr, GORT_Fluency)

## 1.1 Reshape to be "wide" ####

q1SEMData = q1Data %>% 
  filter(Grade<=6) %>% 
  rename(RF = GORT_Fluency)  %>% 
  # Note that the gort is currently on a 100 +/-15 scale, so I'm going to
  # standardise those
  mutate(RF = (RF-100)/15) %>% 
  pivot_wider(id_cols=participant, values_from=c(Fi, Fsbr, RF), names_from=Grade)

# 2.0 Fit the Model ####
## 2.1 Data Availability ####
# I only want participants who provide all variables at least once, and at least
# one variable twice.
# That is they have at least 1 value for each of independent reading, shared 
# book reading and reading fluency, and at least one of them is measured twice.

# note rowMins/rowMaxes are custom functions
# rowMins = function(x, ...) { apply(x, 1, min, ...) }
# rowMaxes = function(x, ...) { apply(x, 1, max, ...) }

q1SEMAnalysis = q1SEMData  %>% 
  mutate(Fi_vals=rowSums(!is.na(select(., starts_with("Fi"))))
         , Fsbr_vals=rowSums(!is.na(select(., starts_with("Fsbr"))))
         , RF_vals=rowSums(!is.na(select(., starts_with("RF"))))
         ) %>% 
  mutate(all_vars = rowMins(select(., ends_with("vals")))>0
         ,two_years = rowMaxes(select(.,ends_with("vals")))>1
         ) %>% 
  filter(all_vars) %>%
  filter(two_years) %>%
  select(-ends_with("_vars"), -two_years)

# Costs me 21 participants - 4 because there is at least one variable that they
# do not have data for at all, and an additional 17 because they didn't provide
# data in at least two years.

## 2.2 Model ####

q1SEMModel = '
# Cross-lagged paths
RF_2 ~ Fi_1+Fsbr_1
RF_3 ~ Fi_1+Fsbr_2
RF_4 ~ Fi_1+Fsbr_3
RF_5 ~ Fi_1+Fsbr_4
RF_6 ~ Fi_1+Fsbr_5

Fi_2 ~ RF_1
Fi_3 ~ RF_2
Fi_4 ~ RF_3
Fi_5 ~ RF_4
Fi_6 ~ RF_5

Fsbr_2 ~ RF_1
Fsbr_3 ~ RF_2
Fsbr_4 ~ RF_3
Fsbr_5 ~ RF_4
Fsbr_6 ~ RF_5

# Auto-regressive paths (Stability)
RF_2 ~ RF_1
RF_3 ~ RF_2
RF_4 ~ RF_3
RF_5 ~ RF_4
RF_6 ~ RF_5

Fi_2 ~ Fi_1
Fi_3 ~ Fi_2
Fi_4 ~ Fi_3
Fi_5 ~ Fi_4
Fi_6 ~ Fi_5

Fsbr_2 ~ Fsbr_1
Fsbr_3 ~ Fsbr_2
Fsbr_4 ~ Fsbr_3
Fsbr_5 ~ Fsbr_4
Fsbr_6 ~ Fsbr_5

# Reciprocal effects
Fi_1 ~~ RF_1 + Fsbr_1
Fsbr_1 ~~ RF_1

Fi_2 ~~ RF_2 + Fsbr_2
Fsbr_2 ~~ RF_2

Fi_3 ~~ RF_3 + Fsbr_3
Fsbr_3 ~~ RF_3

Fi_4 ~~ RF_4 + Fsbr_4
Fsbr_4 ~~ RF_4

Fi_5 ~~ RF_5 + Fsbr_5
Fsbr_5 ~~ RF_5

Fi_6 ~~ RF_6 + Fsbr_6
Fsbr_6 ~~ RF_6
'

## 2.3 Fitting ####

q1SEM = cfa(q1SEMModel, q1SEMAnalysis, missing="fiml", orthogonal=T)

# Seems to fit ok. Need to look into the "coverage" problem.