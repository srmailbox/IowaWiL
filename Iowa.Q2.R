###############################################################################
# Iowa Project - Q2 Reading Value/Interest and Frequency
# 
# Our second question related to the longitudinal associations between Value for
# and interest in Reading, and Reading Frequency (both Shared book reading, SBR, 
# and Independent reading, I)
# 
# Created: 2026-01-30
# Author: Serje Robidoux
# CHANGELOG:

# 0.0 Setup ####
include(lavaan)
include(mice)
include(semTools)

# Randomly sample a seed
# set.seed(110265658)
# Right now, it seems like results do vary a lot between runs. Which I don't love.

# 1.0 Read Data ####

source("Iowa.Data.R")

q2Data = iowaData %>% select(participant, Grade, Fi, Fsbr, RI) %>% 
  filter(Grade < 7)

## 1.1 Add the earliest grade, and number of years of participation ###

q2Data = q2Data %>% 
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

# Reading Interest is not a standardised scale, and clearly increases as the
# kids age
# q2Data %>% group_by(Grade) %>% summarise(mean(RI, na.rm=T), sd(RI, na.rm=T))
# so we need to standardise by grade.

q2.gr13.SEMData = q2Data %>% 
  filter(incl13) %>% 
  pivot_wider(id_cols=participant, values_from=c(Fi, Fsbr, RI), names_from=Grade) %>% 
  mutate(across(starts_with("RI"), ~as.numeric(scale(.))))

q2.gr46.SEMData = q2Data %>% 
  filter(incl46) %>% 
  pivot_wider(id_cols=participant, values_from=c(Fi, Fsbr, RI), names_from=Grade) %>% 
  mutate(across(starts_with("RI"), ~as.numeric(scale(.))))

q2.gr13.SEMAnalysis = q2.gr13.SEMData  %>% 
  mutate(Fi_vals=rowSums(!is.na(select(., starts_with("Fi"))))
         , Fsbr_vals=rowSums(!is.na(select(., starts_with("Fsbr"))))
         , RI_vals=rowSums(!is.na(select(., starts_with("RI"))))
         ) %>% 
  mutate(all_vars = rowMins(pick(ends_with("vals")))>0
         , any_vars = rowMaxes(pick(ends_with("vals"), -all_vars))>0
         # ,two_years = rowMaxes(select(.,ends_with("vals")))>1
  ) %>% 
  filter(any_vars) %>% 
  filter(all_vars) %>%
  # filter(two_years) %>% select(-two_years) %>% 
  select(-ends_with("_vars"))

# Requiring that each participant provides each var at least once costs me 15 
# participants - 12 because they do not have SBR data, and 3 because they do not
# have GORT FLuency data.
# 
# q2.gr13.SEMAnalysis %>% select(matches("[123]$")) %>% mutate(across(everything(), is.na))
# After excluding them, there is still around 36% missingness across the dataset
# Mostly in year 1
#  Fi_3   Fi_2   Fi_1 Fsbr_3 Fsbr_2 Fsbr_1   RI_3   RI_2   RI_1 
# 0.074  0.353  0.647  0.074  0.353  0.647  0.086  0.372  0.673 

q2.gr46.SEMAnalysis = q2.gr46.SEMData  %>% 
  mutate(Fi_vals=rowSums(!is.na(pick(starts_with("Fi"))))
         , Fsbr_vals=rowSums(!is.na(pick(starts_with("Fsbr"))))
         , RI_vals=rowSums(!is.na(pick(starts_with("RI"))))
  ) %>% 
  mutate(all_vars = rowMins(pick(ends_with("vals")))>0
         , any_vars = rowMaxes(pick(ends_with("vals"), -all_vars))>0
         # ,two_years = rowMaxes(select(.,ends_with("vals")))>1
  ) %>% 
  filter(any_vars) %>% 
  filter(!all_vars) %>%
  # filter(two_years) %>% select(-two_years) %>% 
  select(-ends_with("_vars"))

# I lose 7 of 235 - 1 due to missing SBR, and the remaining due to missing Interest.
# 
# Still have 34% missingness
#  Fi_4   Fi_5   Fi_6 Fsbr_4 Fsbr_5 Fsbr_6   RI_4   RI_5   RI_6 
# 0.039  0.320  0.671  0.039  0.320  0.671  0.031  0.316  0.671 

## 2.2 Models ####

q2.gr13.SEMModel = '
# Cross-lagged paths
RI_2 ~ Fi_1+Fsbr_1
RI_3 ~ Fi_2+Fsbr_2

Fi_2 ~ RI_1
Fi_3 ~ RI_2

Fsbr_2 ~ RI_1
Fsbr_3 ~ RI_2

# Auto-regressive paths (Stability)
RI_2 ~ RI_1
RI_3 ~ RI_2

Fi_2 ~ Fi_1
Fi_3 ~ Fi_2

Fsbr_2 ~ Fsbr_1
Fsbr_3 ~ Fsbr_2

# Reciprocal effects
Fi_1 ~~ RI_1 + Fsbr_1
Fsbr_1 ~~ RI_1

Fi_2 ~~ RI_2 + Fsbr_2
Fsbr_2 ~~ RI_2

Fi_3 ~~ RI_3 + Fsbr_3
Fsbr_3 ~~ RI_3
'

q2.gr46.SEMModel = '
# Cross-lagged paths
# RI_2 ~ Fi_1+Fsbr_1
# RI_3 ~ Fi_2+Fsbr_2
# RI_4 ~ Fi_3+Fsbr_3
RI_5 ~ Fi_4+Fsbr_4
RI_6 ~ Fi_5+Fsbr_5

# Fi_2 ~ RI_1
# Fi_3 ~ RI_2
# Fi_4 ~ RI_3
Fi_5 ~ RI_4
Fi_6 ~ RI_5

# Fsbr_2 ~ RI_1
# Fsbr_3 ~ RI_2
# Fsbr_4 ~ RI_3
Fsbr_5 ~ RI_4
Fsbr_6 ~ RI_5

# Auto-regressive paths (Stability)
# RI_2 ~ RI_1
# RI_3 ~ RI_2
# RI_4 ~ RI_3
RI_5 ~ RI_4
RI_6 ~ RI_5

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
# Fi_1 ~~ RI_1 + Fsbr_1
# Fsbr_1 ~~ RI_1
# 
# Fi_2 ~~ RI_2 + Fsbr_2
# Fsbr_2 ~~ RI_2
# 
# Fi_3 ~~ RI_3 + Fsbr_3
# Fsbr_3 ~~ RI_3

Fi_4 ~~ RI_4 + Fsbr_4
Fsbr_4 ~~ RI_4

Fi_5 ~~ RI_5 + Fsbr_5
Fsbr_5 ~~ RI_5

Fi_6 ~~ RI_6 + Fsbr_6
Fsbr_6 ~~ RI_6
'

## 2.3 Fitting ####

q2.gr13.SEM = cfa(q2.gr13.SEMModel, q2.gr13.SEMAnalysis, missing="fiml", orthogonal=T)

q2.gr46.SEM = cfa(q2.gr46.SEMModel, q2.gr46.SEMAnalysis, missing="fiml", orthogonal=T)


## 2.5 Fits ####
rbind(gr13=fitmeasures(q2.gr13.SEM, fit.measures = c("rmsea", "srmr", "tli", "cfi", "agfi"))
, gr46=fitmeasures(q2.gr46.SEM, fit.measures = c("rmsea", "srmr", "tli", "cfi", "agfi")))

#      rmsea  srmr   tli   cfi  agfi
# gr13 0.063 0.062 0.861 0.950 0.960
# gr46 0.090 0.129 0.742 0.907 0.971
# 
# gr13 is very good
# gr46 is weaker, with rmsea srmr not so sure, cfi agfi ok

# 3.0 Output results ####

merge(parameterEstimates(q2.gr13.SEM)
      , parameterEstimates(q2.gr46.SEM) %>% 
        mutate(
          rhs=
            factor(
              rhs
              , levels = paste(gl(3,3,labels=c("RI", "Fi", "Fsbr")), 4:6, sep="_")
              , labels=paste(gl(3,3,labels=c("RI", "Fi", "Fsbr")), 1:3, sep="_")
            )
          , lhs=
            factor(
              lhs
              , levels = paste(gl(3,3,labels=c("RI", "Fi", "Fsbr")), 4:6, sep="_")
              , labels=paste(gl(3,3,labels=c("RI", "Fi", "Fsbr")), 1:3, sep="_")
            )
        )
      , by=c("lhs", "op", "rhs"), suffixes=c(".gr13", ".gr46")) %>%
  write.csv(file="Iowa.q2.results.csv")

# 4.0 EXPLORATORY ####

## 4.1 Parsimonious models ####

# Let's try some parsimonious models, where we only keep effects that were 
# "marginal" or significant.

### 4.1.1 Gr 1-3 ####

q2.gr13.SEMModel.prsmny =
  '
# Cross-lagged paths
#RI_2 ~ Fi_1+Fsbr_1
#RI_3 ~ Fi_2+Fsbr_2

Fi_2 ~ RI_1
Fi_3 ~ RI_2

#Fsbr_2 ~ RI_1
#Fsbr_3 ~ RI_2

# Auto-regressive paths (Stability)
RI_2 ~ RI_1
RI_3 ~ RI_2

Fi_2 ~ Fi_1
Fi_3 ~ Fi_2

Fsbr_2 ~ Fsbr_1
Fsbr_3 ~ Fsbr_2

# Reciprocal effects
Fi_1 ~~ RI_1 + Fsbr_1
Fsbr_1 ~~ RI_1

Fi_2 ~~ RI_2 + Fsbr_2
#Fsbr_2 ~~ RI_2

Fi_3 ~~Fsbr_3 # + RI_3
# Fsbr_3 ~~ RI_3
'

#### 4.1.1.1 Gr 1-3 Fit Parsimonious ####

q2.gr13.SEM.prsmny = cfa(q2.gr13.SEMModel.prsmny, q2.gr13.SEMAnalysis, orthogonal=T, missing="fiml")