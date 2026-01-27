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
include(mice)
include(semTools)

# Randomly sample a seed
set.seed(-893456647)
# Right now, it seems like results do vary a lot between runs. Which I don't love.

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
RF_3 ~ Fi_2+Fsbr_2
RF_4 ~ Fi_3+Fsbr_3
RF_5 ~ Fi_4+Fsbr_4
RF_6 ~ Fi_5+Fsbr_5

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

# Warning message:
#   lavaan->lav_data_full():  
#   due to missing values, some pairwise combinations have zero coverage; the corresponding covariances are not identified; use lavInspect(fit, "coverage") to 
# investigate. 

# Ok, so of course if there is no "complete" data for a pair of variables, you
# can't estimate the covariance for that pair (it is assumed to be zero) -
# which can cause an issue for FIML which wants to adjust the covariances for
# missing data. This happens a fair amount here because the study is not long 
# enough for kids in gr1 to have data in gr 5 or 6, or for kids in gr2 to have 
# data in gr6.

# I think maybe I will try multiple imputation as an alternative just to see how
# much that might matter. Imputation might not be smart enough to do this either
# actually. I guess it depends on if it just uses whatever data it can, or if it
# does listwise deletion as well.

## 2.4 Multiple Imputation ####

q1SEMmice = mice(q1SEMAnalysis %>% select(-ends_with("vals")), m=10)
q1SEM.mi = cfa.mi(q1SEMModel, q1SEMmice, orthogonal=T)
summary(q1SEM.mi)

## 2.5 Fits ####
fitmeasures(q1SEM.mi, fit.measures = c("rmsea", "srmr", "tli", "cfi", "agfi"))
fitmeasures(q1SEM, fit.measures = c("rmsea", "srmr", "tli", "cfi", "agfi"))

# 4 out of 5 seem to prefer the MI approach:
# srmr, rmsea, tli, cfi.
# Only agfi prefers the FIML model.

# 3.0 Output results ####

merge(parameterEstimates(q1SEM)
      , parameterEstimates.mi(q1SEM.mi)
      , by=c("lhs", "op", "rhs"), suffixes=c(".fiml", ".mi")) %>%
  write.csv(file="Iowa.q1.results.csv")

# From an NHST perspective, the two approaches only differ on two cross-lagged
# paths - 
# gr1 Independent Reading predicts gr2 Fluency for the MI model, but not
# the FIML model. CIs: [-.3, -.025] MI vs [-.228, .034] FIML
# gr3 Independent REading predicts gr4 FLuency for the FIML model, but not MI
# CIs [-.03, .20] MI vs [.011, .158] FIML

# While they disagree on significance, the CI's overlap considerably so this is
# more about the sensitivity of p-values to analytic choices than it is about
# anything more fundamentally structural.
# 
# There are three other reciprocal effects where the NHST is inconsistent. Again
# the estimates and CI's are not insanely out of keeping with each other.
