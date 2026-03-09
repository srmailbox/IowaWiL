###############################################################################
# Iowa Project - Data Setup
# 
# This script reads in and processes the data for use in other scripts.
# 
# Created: 2026-01-22
# Author: Serje Robidoux
# CHANGELOG:
# 2026-01-30: Add the reading value data.
# 2026-03-05: Revised dataset provided that corrects some errors and adds 
#             additional data
# 2026-03-07: Revised dataset provided that corrects some Year 5 errors in CMQ
# 2026-03-09: Revised dataset provided that corrects some Year 5 errors in Env
#             - sets things up to fit an LV for Reading Interest

# 0.0 Setup ####
include(readxl)
include(lavaan)

# 1.0 Raw Data ####
## 1.1 Data Dictionary ###

# The Reading Environment data  contains lots of variables that are not relevant 
# here. The data dictionary includes a column that identifies the ones I need 
# for this purpose.
# 2026-03-05: Is this still right? This wasn't updated.

iowaVarDetails = read_xlsx("BobWiL.DataDictionary.xlsx", sheet = "Variables") %>% 
  filter(MeasurementModel) %>% 
  mutate(
    Options = str_split_i(Options, pattern="\\|", i = 1)
    , Options = trimws(str_split_i(Options, pattern=",", i=2))
    , Options = ifelse(Options=="Never", "Monthly", Options)
    , Options = ifelse(Options=="0-25%", "Proportion", Options)
    , Options = ifelse(Options=="less than 15 minutes", "TimeRange", Options)
  )
msrmntMdlVars = iowaVarDetails$Variable

## 1.2 Reading Environment Data ####

iowaEnv = read_xlsx("ReadAnx_Q2_item.xlsx", sheet="ReadAnx_Q5_item") %>% 
  select(participant, Sample, StudyYear, StartGrade, StartYear, Grade, all_of(msrmntMdlVars)) %>% 
  mutate(StartGrade = as.numeric(substr(StartGrade,1,1))
         # , Grade = StartGrade+(2020+StudyYear)-StartYear
         , across(c(StudyYear, StartGrade, StartYear, Grade), as.numeric)
         )
# 2026-03-04: This file has been renamed to Q5
# 2026-03-05: these are all "text" as read in, so need to be converted

### 1.2.1 Recoding Schemes ####
# most of the variables were answered on a "likert-like" scale
# e.g., how often do you X: never, <1 time per month, once a month... etc...
# I want to recode those into numeric scores, rather than just ordered.

recodeList = list(
  # If the first option is "Never", then the frequency is expressed in momthly/
  # weekly/daily terms, so I convert them assuming a 4wk month (28 days)
  Monthly = c(0, .5, 1, 3, 4, 10, 28, 35)
  # if the first option is 0-25%, then the data is expressed as a proportion
  # of times.
  , Proportion = c(.125, .375, .625, .875)
  # These are expressed in minutes (less than 15, 15-30, 30-60, 1-2hr, 2hr+)
  , TimeRange = c(7, 22, 45, 90, 150)
)

### 1.2.2.0 Recode variables as numeric ####
# This will take each variable, look up the Options for that variable and,
# if necessary, recode it.

for (var in colnames(iowaEnv)[7:32]) {
  varOptions = iowaVarDetails$Options[iowaVarDetails$Variable==var]
  if(!is.na(varOptions))
    iowaEnv[,var]=recodeList[[varOptions]][as.numeric(unlist(iowaEnv[,var]))]
  
}

# str(iowaEnv)

### 1.2.3 Duration fields ####
# Unfortunately, the duration fields were input as strings which means they have
# a lot of non-numeric values that will need to be converted to numeric.

# this bit of code spits out a list of the responses provided across the three
# duration_ fields:
# iowaEnv %>% select(starts_with("duration")) %>% unlist %>% as.matrix %>%
#   unique %>%
#   write.csv(file="durationResponses.csv", row.names = F, quote=T)

# iowaEnv %>% 
#   select(homework, reading_school_related
#          , reading_nonschool_related, internet_reading) %>% 
#   unlist %>% as.matrix %>% unique %>% 
#   write.csv(file="afterSchool_amountoftime.csv", row.names=F, quote=T)


#### NOTE: there is one value of 200 here that has been replaced with 20, after
#### # examining the child's other responses
durationRespMap = read_xlsx("durationResponseMap.xlsx", sheet="duration_fields") %>% 
  mutate(duration_shared_reading_mins = duration
         , duration_read_out_loud_mins = duration
         , duration_read_alone_mins = duration) %>% 
  select(-duration, -`Hm?`)

for (var in colnames(iowaEnv %>% select(starts_with("dura")))) {
  iowaEnv = merge(iowaEnv
                   , durationRespMap %>% 
                     select(strResp, all_of(paste0(var, "_mins")))
                   , by.x=var, by.y="strResp", all.x=T
  )
  
}

### 1.2.4 Derive Reading Interest and Frequency Scores ####
# We had decided to use the results from a latent variable model with 
# Independent Reading and child-active SBR as the main measures of frequency.
# (that is, only consider reading where the child is actively reading, not
# passively being read to)
# 
# See Iowa.SBRMeasurementModel.R for details of this decision

cfaFreqModel = '
# F1 - child reading to parent: child_read_*
childSBR =~ child_read_out_loud+child_read_picture_book+child_read_comics+child_read_chapter_books+child_read_nonfiction

# F2 - independent reading: read_alone, ..._chapter_book, initiate_read_alone (nonschool_related, duration_read_alone)
IR =~ read_alone+read_alone_chapter_books+read_alone_comics+initiate_read_alone#+reading_nonschool_related+duration_read_alone_mins

# F3 - parent reading to child: parent_read_out_loud, .._chapter_books (picture_books, nonfiction)
# prntSBR =~ parent_read_out_loud+parent_read_chapter_books#+parent_read_picture_books+parent_read_nonfiction#+parent_read_comics

# F4 - school work: homework, reading school related - IGNORED since homework includes more than just reading

childSBR~~IR #+ prntSBR
# IR ~~ prntSBR
'
cfaData = iowaEnv %>% 
  drop_na(child_read_out_loud,child_read_picture_book,child_read_comics,
          child_read_chapter_books,child_read_nonfiction,read_alone
          ,read_alone_chapter_books,read_alone_comics,initiate_read_alone
          # ,reading_nonschool_related,duration_read_alone_mins
          )

iowa.cfaFreq = sem(cfaFreqModel, data=cfaData, std.ov = T)

#fitmeasures(iowa.cfaFreq, fit.measures = c("srmr", "rmsea", "cfi", "agfi", "tli"))
# mediocre fits, really
#  srmr rmsea   cfi  agfi   tli 
# 0.088 0.123 0.891 0.864 0.848 

# Assign scores to kids and years
cfaRes = cbind(
  cfaData %>% 
    select(participant, StudyYear)
  , predict(iowa.cfaFreq)) %>% 
  rename(Fi = IR, Fsbr = childSBR)

iowaEnv=merge(iowaEnv, cfaRes, all.x=T)

## 1.3 Reading Ability ####
# 2026-03-05: Grade is now computed and included in the dataset
iowaReading = read_xlsx("ReadAnx_Scores.xlsx", sheet="ReadAnx_Scores") %>%
  mutate(across(-Sample, as.numeric)) %>% 
  select(participantID, Sample, Grade, starts_with("CC2"), starts_with("GORT"))


## 1.4 Reading Interest ####
# We pre-registered just using the mean of the 5 CMQ items:

iowaInterest = read_xlsx('ReadAnx_CMQ_item.xlsx', sheet='ReadAnx_CMQ_item') %>% 
  mutate(across(starts_with("motiv"), as.numeric))

# where we have at least 3 items, scores will be computed from available 
# responses. Otherwise they will be deemed missing:

(iowaInterest = 
    iowaInterest %>% 
    mutate(
      nNA = rowSums(across(c(starts_with("motiv"),-motiv_5), is.na))
      , RI = ifelse(nNA<3,rowMeans(pick(starts_with("motiv"), -motiv_5), na.rm=T), NA)
    )
) %>% select(nNA) %>% table()

# This will cost us 24 ppts - 22 of whom provided no data anyway, so really only
# 2.

# 2.0 Merge data sources ####
iowaData = merge(iowaEnv, iowaReading, by.x=c("participant", "Grade")
                 , by.y=c("participantID", "Grade"), all=T
                 , suffixes=c(".x", "")) %>% 
  select(-ends_with(".x")) %>% 
  merge(iowaInterest %>% select(participant, Grade, starts_with("motiv"),RI)
        , by=c("participant", "Grade"), all=T, suffixes=c("", ".y")) %>% 
  select(-ends_with(".y")) %>% 
  filter(participant!=2881) # This participant provided no data

# 3.0 Data details ####

table(iowaEnv$StudyYear, iowaEnv$Grade)
table(iowaInterest$StudyYear, iowaInterest$Grade)

## 3.1 Interest Scale ####
if(FALSE) {

### 3.1.1 Cronbach's alpha ####
  # Ok, let's take a look at the internal reliability:
  psych::alpha(iowaInterest %>% select(starts_with("motiv")))
  # Across the sample: alpha = .71, so ok, but not brilliant. Item 5 is the
  # weakest reducing the reliability from .74
  
  lapply(
    by(
      iowaInterest %>% select(starts_with("motiv")), iowaInterest$Grade
      , psych::alpha
    )
    , function(x) x$item.stats   
  )
  # gr 1 .75 (.79)
  # g4 2 .68 (.70)
  # gr 3 .64 (.66)
  # gr 4 .70 (.71)
  # gr 5 .67 (.71)
  # gr 6 .72 (.77)
  # 
  # in all grades, the correlation between item 5, and the others (r.drop) is
  # very weak, resulting in reduced reliabilities (parenthesis are the alpha
  # if item 5 is removed).

### 3.1.2 Factor Analysis ####

  factanal(iowaInterest %>% select(starts_with("motiv")) %>% drop_na, 2)

### 3.1.3 PCA ####
  
  princomp(iowaInterest %>% select(starts_with("motiv")) %>% drop_na) %>% summary(loadings=T)
}


