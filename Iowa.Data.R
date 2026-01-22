###############################################################################
# Iowa Project - Data Setup
# 
# This script reads in and processes the data for use in other scripts.
# 
# Created: 2026-01-22
# Author: Serje Robidoux
# CHANGELOG:

# 0.0 Setup ####
include(readxl)

# 1.0 Data ####
# The data file contains lots of variables that are not relevant here. The 
# data dictionary includes a column that identifies the ones I need for this 
# purpose.

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

iowaData = read_xlsx("ReadAnx_Env_item.xlsx", sheet="ReadAnx_Q5_item") %>% 
  select(participant, Sample, StudyYear, StartGrade, StartYear, all_of(msrmntMdlVars)) %>% 
  mutate(StartGrade = as.numeric(substr(StartGrade,1,1))
         , Grade = StartGrade+StudyYear-1)

# hm... need to check that there were only 3 data points in gr. 7... that seems
# wrong.

## 1.1 Recoding Schemes ####
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

# 2.0 Recode variables as numeric ####
# This will take each variable, look up the Options for that variable and,
# if necessary, recode it.

for (var in colnames(iowaData)[6:31]) {
  varOptions = iowaVarDetails$Options[iowaVarDetails$Variable==var]
  if(!is.na(varOptions))
    iowaData[,var]=recodeList[[varOptions]][unlist(iowaData[,var])]
  
}

# str(iowaData)

## 2.1 Duration fields ####
# Unfortunately, the duration fields were input as strings which means they have
# a lot of non-numeric values that will need to be converted to numeric.

# this bit of code spits out a list of the responses provided across the three
# duration_ fields:
# iowaData %>% select(starts_with("duration")) %>% unlist %>% as.matrix %>% 
#   unique %>% 
#   write.csv(file="durationResponses.csv", row.names = F, quote=T)

# iowaData %>% 
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



for (var in colnames(iowaData)[which(substr(colnames(iowaData), 1, 4)=="dura")]) {
  iowaData = merge(iowaData
                   , durationRespMap %>% 
                     select(strResp, all_of(paste0(var, "_mins")))
                   , by.x=var, by.y="strResp", all.x=T
  )
  
}

