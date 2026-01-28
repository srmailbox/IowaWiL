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
# 2026-01-28: Massive overhaul to split the analysis into gr1-3, and gr4-6 to
#   overcome an issue with missingness in the covariance structures.

# 0.0 Setup ####
include(lavaan)
include(mice)
include(semTools)

# Randomly sample a seed
set.seed(-893456647)
# Right now, it seems like results do vary a lot between runs. Which I don't love.

# 1.0 Read Data ####

source("Iowa.Data.R")

q1Data = iowaData %>% select(participant, Grade, Fi, Fsbr, GORT_Fluency) %>% 
  filter(Grade < 7)

## 1.1 Add the earliest grade, and number of years of participation ###

q1Data = q1Data %>% 
  group_by(participant) %>% 
  mutate(
    startGrade = min(Grade)
    , nYears=n()
  ) %>% ungroup() %>% 
  mutate(
    incl13 = Grade %in% 1:3 #startGrade<3 & nYears>1
    , incl46 = Grade %in% 4:6 #startGrade>2 & nYears>1
  )

## 1.3 Split Data into grs 1-3 and 4-6 and reshape ####

q1.gr13.SEMData = q1Data %>% 
  filter(incl13) %>% 
  rename(RF = GORT_Fluency)  %>% 
  # Note that the gort is currently on a 100 +/-15 scale, so I'm going to
  # standardise those
  mutate(RF = (RF-100)/15) %>% 
  pivot_wider(id_cols=participant, values_from=c(Fi, Fsbr, RF), names_from=Grade)

q1.gr46.SEMData = q1Data %>% 
  filter(incl46) %>% 
  rename(RF = GORT_Fluency)  %>% 
  # Note that the gort is currently on a 100 +/-15 scale, so I'm going to
  # standardise those
  mutate(RF = (RF-100)/15) %>% 
  pivot_wider(id_cols=participant, values_from=c(Fi, Fsbr, RF), names_from=Grade)

q1.gr13.SEMAnalysis = q1.gr13.SEMData  %>% 
  mutate(Fi_vals=rowSums(!is.na(select(., starts_with("Fi"))))
         , Fsbr_vals=rowSums(!is.na(select(., starts_with("Fsbr"))))
         , RF_vals=rowSums(!is.na(select(., starts_with("RF"))))
         ) %>% 
  mutate(all_vars = rowMins(select(., ends_with("vals")))>0
         ,two_years = rowMaxes(select(.,ends_with("vals")))>1
         ) %>% 
  filter(all_vars) %>%
  # filter(two_years) %>% select(-two_years) %>% 
  select(-ends_with("_vars"))

# Requiring that each participant provides each var at least once Costs me 19 
# participants - 13 because they do not have SBR data, and 6 because they do not
# have GORT FLuency data. (1 because they provide no data at all)
# 
# After excluding them, there is still around 38% missingness across the dataset
# RF_2   RF_3   Fi_2   Fi_3 Fsbr_2 Fsbr_3   Fi_1 Fsbr_1   RF_1 
# 0.337  0.075  0.356  0.075  0.356  0.075  0.742  0.742  0.648 

q1.gr46.SEMAnalysis = q1.gr46.SEMData  %>% 
  mutate(Fi_vals=rowSums(!is.na(select(., starts_with("Fi"))))
         , Fsbr_vals=rowSums(!is.na(select(., starts_with("Fsbr"))))
         , RF_vals=rowSums(!is.na(select(., starts_with("RF"))))
  ) %>% 
  mutate(all_vars = rowMins(select(., ends_with("vals")))>0
         ,two_years = rowMaxes(select(.,ends_with("vals")))>1
  ) %>% 
  filter(all_vars) %>%
  # filter(two_years) %>% select(-two_years) %>% 
  select(-ends_with("_vars"))

# Requiring that each participant provides each var at least once costs me 14 
# participants - 13 because they do not have GORT Fluencydata, and 1 because 
# they do not have SBR data.
# 
# Overall, less missingness here although it's still 31%
#   RF_5   RF_6   Fi_5   Fi_6 Fsbr_5 Fsbr_6   Fi_4 Fsbr_4   RF_4 
# 0.167  0.445  0.339  0.661  0.339  0.661  0.061  0.061  0.016 

## 2.2 Models ####

q1.gr13.SEMModel = '
# Cross-lagged paths
RF_2 ~ Fi_1+Fsbr_1
RF_3 ~ Fi_2+Fsbr_2

Fi_2 ~ RF_1
Fi_3 ~ RF_2

Fsbr_2 ~ RF_1
Fsbr_3 ~ RF_2

# Auto-regressive paths (Stability)
RF_2 ~ RF_1
RF_3 ~ RF_2

Fi_2 ~ Fi_1
Fi_3 ~ Fi_2

Fsbr_2 ~ Fsbr_1
Fsbr_3 ~ Fsbr_2

# Reciprocal effects
Fi_1 ~~ RF_1 + Fsbr_1
Fsbr_1 ~~ RF_1

Fi_2 ~~ RF_2 + Fsbr_2
Fsbr_2 ~~ RF_2

Fi_3 ~~ RF_3 + Fsbr_3
Fsbr_3 ~~ RF_3
'

q1.gr46.SEMModel = '
# Cross-lagged paths
# RF_2 ~ Fi_1+Fsbr_1
# RF_3 ~ Fi_2+Fsbr_2
# RF_4 ~ Fi_3+Fsbr_3
RF_5 ~ Fi_4+Fsbr_4
RF_6 ~ Fi_5+Fsbr_5

# Fi_2 ~ RF_1
# Fi_3 ~ RF_2
# Fi_4 ~ RF_3
Fi_5 ~ RF_4
Fi_6 ~ RF_5

# Fsbr_2 ~ RF_1
# Fsbr_3 ~ RF_2
# Fsbr_4 ~ RF_3
Fsbr_5 ~ RF_4
Fsbr_6 ~ RF_5

# Auto-regressive paths (Stability)
# RF_2 ~ RF_1
# RF_3 ~ RF_2
# RF_4 ~ RF_3
RF_5 ~ RF_4
RF_6 ~ RF_5

# Fi_2 ~ Fi_1
# Fi_3 ~ Fi_2
# Fi_4 ~ Fi_3
Fi_5 ~ Fi_4
Fi_6 ~ Fi_5

# Fsbr_2 ~ Fsbr_1
# Fsbr_3 ~ Fsbr_2
# Fsbr_4 ~ Fsbr_3
Fsbr_5 ~ Fsbr_4
Fsbr_6 ~ Fsbr_5

# Reciprocal effects
# Fi_1 ~~ RF_1 + Fsbr_1
# Fsbr_1 ~~ RF_1
# 
# Fi_2 ~~ RF_2 + Fsbr_2
# Fsbr_2 ~~ RF_2
# 
# Fi_3 ~~ RF_3 + Fsbr_3
# Fsbr_3 ~~ RF_3

Fi_4 ~~ RF_4 + Fsbr_4
Fsbr_4 ~~ RF_4

Fi_5 ~~ RF_5 + Fsbr_5
Fsbr_5 ~~ RF_5

Fi_6 ~~ RF_6 + Fsbr_6
Fsbr_6 ~~ RF_6
'

## 2.3 Fitting ####

q1.gr13.SEM = cfa(q1.gr13.SEMModel, q1.gr13.SEMAnalysis, missing="fiml", orthogonal=T)

q1.gr46.SEM = cfa(q1.gr46.SEMModel, q1.gr46.SEMAnalysis, missing="fiml", orthogonal=T)


## 2.5 Fits ####
rbind(gr13=fitmeasures(q1.gr13.SEM, fit.measures = c("rmsea", "srmr", "tli", "cfi", "agfi"))
, gr46=fitmeasures(q1.gr46.SEM, fit.measures = c("rmsea", "srmr", "tli", "cfi", "agfi")))

# Not terrible, but not great either:
#      rmsea  srmr   tli   cfi  agfi
# gr13 0.082 0.074 0.887 0.959 0.720
# gr46 0.090 0.062 0.904 0.965 0.785
# 
# SRMR is good, RMSEA marginal, TLI borderline, CFI good, AGFI weak

# 3.0 Output results ####

merge(parameterEstimates(q1.gr13.SEM)
      , parameterEstimates(q1.gr46.SEM) %>% 
        mutate(
          rhs=
            factor(
              rhs
              , levels = paste(gl(3,3,labels=c("RF", "Fi", "Fsbr")), 4:6, sep="_")
              , labels=paste(gl(3,3,labels=c("RF", "Fi", "Fsbr")), 1:3, sep="_")
            )
          , lhs=
            factor(
              lhs
              , levels = paste(gl(3,3,labels=c("RF", "Fi", "Fsbr")), 4:6, sep="_")
              , labels=paste(gl(3,3,labels=c("RF", "Fi", "Fsbr")), 1:3, sep="_")
            )
        )
      , by=c("lhs", "op", "rhs"), suffixes=c(".gr13", ".gr46")) %>%
  write.csv(file="Iowa.q1.results.csv")



# 4.0 EXPLORATORY ####



## 4.1 Parsimonious models ####

# Let's try some parsimonious models, where we only keep effects that were 
# "marginal" or significant.

### 4.1.1 Gr 1-3 ####

q1.gr13.SEMModel.prsmny =
  '
# Cross-lagged paths
#RF_2 ~ Fi_1+Fsbr_1
#RF_3 ~ Fi_2+Fsbr_2

Fi_2 ~ RF_1
Fi_3 ~ RF_2

#Fsbr_2 ~ RF_1
#Fsbr_3 ~ RF_2

# Auto-regressive paths (Stability)
RF_2 ~ RF_1
RF_3 ~ RF_2

Fi_2 ~ Fi_1
Fi_3 ~ Fi_2

Fsbr_2 ~ Fsbr_1
Fsbr_3 ~ Fsbr_2

# Reciprocal effects
Fi_1 ~~ RF_1 + Fsbr_1
Fsbr_1 ~~ RF_1

Fi_2 ~~ RF_2 + Fsbr_2
#Fsbr_2 ~~ RF_2

Fi_3 ~~Fsbr_3 # + RF_3
# Fsbr_3 ~~ RF_3
'

#### 4.1.1.1 Gr 1-3 Fit Parsimonious ####

q1.gr13.SEM.prsmny = cfa(q1.gr13.SEMModel.prsmny, q1.gr13.SEMAnalysis, orthogonal=T, missing="fiml")