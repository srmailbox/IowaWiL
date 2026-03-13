###############################################################################
# Iowa Project - QE Other Reading Ability Measures and Frequency
# 
# This covers an exploration of the other reading measures - GORT_Comprehension
# and CC2 and TOWRE
# 
# Created: 2026-03-13
# Author: Serje Robidoux
# CHANGELOG:

# 0.0 Setup ####
include(lavaan)
include(semTools)


# 1.0 Read Data ####

source("Iowa.Data.R")

expData = iowaData %>% 
  select(participant, Grade, Fi, Fsbr, starts_with("GORT")
         , starts_with("CC2"), starts_with("TOWRE")) %>% 
  filter(Grade < 7)

## 1.1 Add the earliest grade, and number of years of participation ###

expData = expData %>% 
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

exp.gr13.SEMData = expData %>% 
  filter(incl13) %>% 
  rename(RF = GORT_Fluency)  %>% arrange(Grade) %>% 
  # Note that the gort is currently on a 100 +/-15 scale, so I'm going to
  # standardise those
  mutate(across(c(RF, starts_with("TOWRE")), ~(.x-100)/15)) %>% 
  group_by(Grade) %>% 
  mutate(across(starts_with("CC2"), ~scale(.x)[,1])) %>% 
  ungroup() %>% 
  pivot_wider(id_cols=participant
              , values_from=c(Fi, Fsbr, RF, starts_with("TOWRE"), starts_with("CC2"))
              , names_from=Grade)

exp.gr46.SEMData = expData %>% 
  filter(incl46) %>% 
  rename(RF = GORT_Fluency)  %>% arrange(Grade) %>% 
  # Note that the gort is currently on a 100 +/-15 scale, so I'm going to
  # standardise those
  mutate(across(c(RF, starts_with("TOWRE")), ~(.x-100)/15)) %>% 
  group_by(Grade) %>% 
  mutate(across(starts_with("CC2"), ~scale(.x)[,1])) %>% 
  ungroup() %>% 
  pivot_wider(id_cols=participant
              , values_from=c(Fi, Fsbr, RF, starts_with("TOWRE"), starts_with("CC2"))
              , names_from=Grade)


## 2.2 Models ####

### 2.2.1 CC2irr Models ####
exp.gr13.CC2irr.SEMModel = '
# Cross-lagged paths
CC2irr_2 ~ Fi_1+Fsbr_1
CC2irr_3 ~ Fi_2+Fsbr_2

Fi_2 ~ CC2irr_1
Fi_3 ~ CC2irr_2

Fsbr_2 ~ CC2irr_1
Fsbr_3 ~ CC2irr_2

# Auto-regressive paths (Stability)
CC2irr_2 ~ CC2irr_1
CC2irr_3 ~ CC2irr_2

Fi_2 ~ Fi_1
Fi_3 ~ Fi_2

Fsbr_2 ~ Fsbr_1
Fsbr_3 ~ Fsbr_2

# Reciprocal effects
Fi_1 ~~ CC2irr_1 + Fsbr_1
Fsbr_1 ~~ CC2irr_1

Fi_2 ~~ CC2irr_2 + Fsbr_2
Fsbr_2 ~~ CC2irr_2

Fi_3 ~~ CC2irr_3 + Fsbr_3
Fsbr_3 ~~ CC2irr_3
'

exp.gr46.CC2irr.SEMModel = '
# Cross-lagged paths
# CC2irr_2 ~ Fi_1+Fsbr_1
# CC2irr_3 ~ Fi_2+Fsbr_2
# CC2irr_4 ~ Fi_3+Fsbr_3
CC2irr_5 ~ Fi_4+Fsbr_4
CC2irr_6 ~ Fi_5+Fsbr_5

# Fi_2 ~ CC2irr_1
# Fi_3 ~ CC2irr_2
# Fi_4 ~ CC2irr_3
Fi_5 ~ CC2irr_4
Fi_6 ~ CC2irr_5

# Fsbr_2 ~ CC2irr_1
# Fsbr_3 ~ CC2irr_2
# Fsbr_4 ~ CC2irr_3
Fsbr_5 ~ CC2irr_4
Fsbr_6 ~ CC2irr_5

# Auto-regressive paths (Stability)
# CC2irr_2 ~ CC2irr_1
# CC2irr_3 ~ CC2irr_2
# CC2irr_4 ~ CC2irr_3
CC2irr_5 ~ CC2irr_4
CC2irr_6 ~ CC2irr_5

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
# Fi_1 ~~ CC2irr_1 + Fsbr_1
# Fsbr_1 ~~ CC2irr_1
# 
# Fi_2 ~~ CC2irr_2 + Fsbr_2
# Fsbr_2 ~~ CC2irr_2
# 
# Fi_3 ~~ CC2irr_3 + Fsbr_3
# Fsbr_3 ~~ CC2irr_3

Fi_4 ~~ CC2irr_4 + Fsbr_4
Fsbr_4 ~~ CC2irr_4

Fi_5 ~~ CC2irr_5 + Fsbr_5
Fsbr_5 ~~ CC2irr_5

Fi_6 ~~ CC2irr_6 + Fsbr_6
Fsbr_6 ~~ CC2irr_6
'


### 2.2.2 TOWREwd Models ####
exp.gr13.TOWREwd.SEMModel = '
# Cross-lagged paths
TOWREwd_2 ~ Fi_1+Fsbr_1
TOWREwd_3 ~ Fi_2+Fsbr_2

Fi_2 ~ TOWREwd_1
Fi_3 ~ TOWREwd_2

Fsbr_2 ~ TOWREwd_1
Fsbr_3 ~ TOWREwd_2

# Auto-regressive paths (Stability)
TOWREwd_2 ~ TOWREwd_1
TOWREwd_3 ~ TOWREwd_2

Fi_2 ~ Fi_1
Fi_3 ~ Fi_2

Fsbr_2 ~ Fsbr_1
Fsbr_3 ~ Fsbr_2

# Reciprocal effects
Fi_1 ~~ TOWREwd_1 + Fsbr_1
Fsbr_1 ~~ TOWREwd_1

Fi_2 ~~ TOWREwd_2 + Fsbr_2
Fsbr_2 ~~ TOWREwd_2

Fi_3 ~~ TOWREwd_3 + Fsbr_3
Fsbr_3 ~~ TOWREwd_3
'

exp.gr46.TOWREwd.SEMModel = '
# Cross-lagged paths
# TOWREwd_2 ~ Fi_1+Fsbr_1
# TOWREwd_3 ~ Fi_2+Fsbr_2
# TOWREwd_4 ~ Fi_3+Fsbr_3
TOWREwd_5 ~ Fi_4+Fsbr_4
TOWREwd_6 ~ Fi_5+Fsbr_5

# Fi_2 ~ TOWREwd_1
# Fi_3 ~ TOWREwd_2
# Fi_4 ~ TOWREwd_3
Fi_5 ~ TOWREwd_4
Fi_6 ~ TOWREwd_5

# Fsbr_2 ~ TOWREwd_1
# Fsbr_3 ~ TOWREwd_2
# Fsbr_4 ~ TOWREwd_3
Fsbr_5 ~ TOWREwd_4
Fsbr_6 ~ TOWREwd_5

# Auto-regressive paths (Stability)
# TOWREwd_2 ~ TOWREwd_1
# TOWREwd_3 ~ TOWREwd_2
# TOWREwd_4 ~ TOWREwd_3
TOWREwd_5 ~ TOWREwd_4
TOWREwd_6 ~ TOWREwd_5

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
# Fi_1 ~~ TOWREwd_1 + Fsbr_1
# Fsbr_1 ~~ TOWREwd_1
# 
# Fi_2 ~~ TOWREwd_2 + Fsbr_2
# Fsbr_2 ~~ TOWREwd_2
# 
# Fi_3 ~~ TOWREwd_3 + Fsbr_3
# Fsbr_3 ~~ TOWREwd_3

Fi_4 ~~ TOWREwd_4 + Fsbr_4
Fsbr_4 ~~ TOWREwd_4

Fi_5 ~~ TOWREwd_5 + Fsbr_5
Fsbr_5 ~~ TOWREwd_5

Fi_6 ~~ TOWREwd_6 + Fsbr_6
Fsbr_6 ~~ TOWREwd_6
'



### 2.2.3 CC2nw Models ####
# Can't use TOWRE for gr1-3
exp.gr46.CC2nw.SEMModel = '
# Cross-lagged paths
# CC2nw_2 ~ Fi_1+Fsbr_1
# CC2nw_3 ~ Fi_2+Fsbr_2
# CC2nw_4 ~ Fi_3+Fsbr_3
CC2nw_5 ~ Fi_4+Fsbr_4
CC2nw_6 ~ Fi_5+Fsbr_5

# Fi_2 ~ CC2nw_1
# Fi_3 ~ CC2nw_2
# Fi_4 ~ CC2nw_3
Fi_5 ~ CC2nw_4
Fi_6 ~ CC2nw_5

# Fsbr_2 ~ CC2nw_1
# Fsbr_3 ~ CC2nw_2
# Fsbr_4 ~ CC2nw_3
Fsbr_5 ~ CC2nw_4
Fsbr_6 ~ CC2nw_5

# Auto-regressive paths (Stability)
# CC2nw_2 ~ CC2nw_1
# CC2nw_3 ~ CC2nw_2
# CC2nw_4 ~ CC2nw_3
CC2nw_5 ~ CC2nw_4
CC2nw_6 ~ CC2nw_5

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
# Fi_1 ~~ CC2nw_1 + Fsbr_1
# Fsbr_1 ~~ CC2nw_1
# 
# Fi_2 ~~ CC2nw_2 + Fsbr_2
# Fsbr_2 ~~ CC2nw_2
# 
# Fi_3 ~~ CC2nw_3 + Fsbr_3
# Fsbr_3 ~~ CC2nw_3

Fi_4 ~~ CC2nw_4 + Fsbr_4
Fsbr_4 ~~ CC2nw_4

Fi_5 ~~ CC2nw_5 + Fsbr_5
Fsbr_5 ~~ CC2nw_5

Fi_6 ~~ CC2nw_6 + Fsbr_6
Fsbr_6 ~~ CC2nw_6
'

### 2.2.4 TOWREdec Models ####
# TOWRE data is too limited to fit gr 1-3

exp.gr46.TOWREdec.SEMModel = '
# Cross-lagged paths
# TOWREdec_2 ~ Fi_1+Fsbr_1
# TOWREdec_3 ~ Fi_2+Fsbr_2
# TOWREdec_4 ~ Fi_3+Fsbr_3
TOWREdec_5 ~ Fi_4+Fsbr_4
TOWREdec_6 ~ Fi_5+Fsbr_5

# Fi_2 ~ TOWREdec_1
# Fi_3 ~ TOWREdec_2
# Fi_4 ~ TOWREdec_3
Fi_5 ~ TOWREdec_4
Fi_6 ~ TOWREdec_5

# Fsbr_2 ~ TOWREdec_1
# Fsbr_3 ~ TOWREdec_2
# Fsbr_4 ~ TOWREdec_3
Fsbr_5 ~ TOWREdec_4
Fsbr_6 ~ TOWREdec_5

# Auto-regressive paths (Stability)
# TOWREdec_2 ~ TOWREdec_1
# TOWREdec_3 ~ TOWREdec_2
# TOWREdec_4 ~ TOWREdec_3
TOWREdec_5 ~ TOWREdec_4
TOWREdec_6 ~ TOWREdec_5

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
# Fi_1 ~~ TOWREdec_1 + Fsbr_1
# Fsbr_1 ~~ TOWREdec_1
# 
# Fi_2 ~~ TOWREdec_2 + Fsbr_2
# Fsbr_2 ~~ TOWREdec_2
# 
# Fi_3 ~~ TOWREdec_3 + Fsbr_3
# Fsbr_3 ~~ TOWREdec_3

Fi_4 ~~ TOWREdec_4 + Fsbr_4
Fsbr_4 ~~ TOWREdec_4

Fi_5 ~~ TOWREdec_5 + Fsbr_5
Fsbr_5 ~~ TOWREdec_5

Fi_6 ~~ TOWREdec_6 + Fsbr_6
Fsbr_6 ~~ TOWREdec_6
'

## 2.3 Fitting ####

### 2.3.1 CC2irr ####
exp.gr13.CC2irr.SEM = cfa(exp.gr13.CC2irr.SEMModel, exp.gr13.SEMData, missing="fiml", orthogonal=T)
exp.gr46.CC2irr.SEM = cfa(exp.gr46.CC2irr.SEMModel, exp.gr46.SEMData, missing="fiml", orthogonal=T)

### 2.3.2 TOWREwd ####
# exp.gr13.TOWREwd.SEM = cfa(exp.gr13.TOWREwd.SEMModel, exp.gr13.SEMData, missing="fiml", orthogonal=T)
# No coverage of the year 4 TOWRE and any year 6 data.
exp.gr46.TOWREwd.SEM = cfa(exp.gr46.TOWREwd.SEMModel, exp.gr46.SEMData, missing="fiml", orthogonal=T)

### 2.3.3 CC2nw ####
exp.gr13.CC2nw.SEM = cfa(exp.gr13.CC2nw.SEMModel, exp.gr13.SEMData, missing="fiml", orthogonal=T)
exp.gr46.CC2nw.SEM = cfa(exp.gr46.CC2nw.SEMModel, exp.gr46.SEMData, missing="fiml", orthogonal=T)

### 2.3.4 TOWREdec ####
# exp.gr13.TOWREdec.SEM = cfa(exp.gr13.TOWREdec.SEMModel, exp.gr13.SEMData, missing="fiml", orthogonal=T)
exp.gr46.TOWREdec.SEM = cfa(exp.gr46.TOWREdec.SEMModel, exp.gr46.SEMData, missing="fiml", orthogonal=T)
# No coverage of the year 4 TOWRE and any year 6 data.

## 2.5 Fits ####

### 2.5.1 CC2irr ####
rbind(gr13=fitmeasures(exp.gr13.CC2irr.SEM, fit.measures = c("rmsea", "srmr", "tli", "cfi", "agfi"))
      , gr46=fitmeasures(exp.gr46.CC2irr.SEM, fit.measures = c("rmsea", "srmr", "tli", "cfi", "agfi"))) %>% 
  round(3)

# Very good.
#      rmsea  srmr   tli   cfi  agfi
# gr13 0.081 0.067 0.890 0.960 0.625
# gr46 0.088 0.068 0.895 0.962 0.742
# 
# RMSEA and TLI are borderline, but everything else is good.

### 2.5.1 CC2nw ####
rbind(gr13=fitmeasures(exp.gr13.CC2nw.SEM, fit.measures = c("rmsea", "srmr", "tli", "cfi", "agfi"))
      , gr46=fitmeasures(exp.gr46.CC2nw.SEM, fit.measures = c("rmsea", "srmr", "tli", "cfi", "agfi"))) %>% 
  round(3)

# Very good.
#      rmsea  srmr   tli   cfi  agfi
# gr13 0.075 0.072 0.902 0.965 0.714
# gr46 0.086 0.068 0.904 0.965 0.724
# 
# very similar, but now RMSEA is only borderline for gr46, and TLI is fine.


# 3.0 Output results ####

## 3.1 CC2irr ####
merge(parameterEstimates(exp.gr13.CC2irr.SEM)
      , parameterEstimates(exp.gr46.CC2irr.SEM) %>% 
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
  arrange(lhs, rhs) %>% 
  write.csv(file="Iowa.exp.CC2irr.results.csv")

## 3.3 CC2nw ####
merge(parameterEstimates(exp.gr13.CC2nw.SEM)
      , parameterEstimates(exp.gr46.CC2nw.SEM) %>% 
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
  arrange(lhs, rhs) %>% 
  write.csv(file="Iowa.exp.CC2nw.results.csv")
