###############################################################################
# iowa Project - Bob
# 
# This script is to develop a measurement model for the shared book reading
# measures. There are items related to Frequency/duration, some that seem to be
# about shared book reading (SBR), some about independent reading (IR), and a 
# couple about initiating reading. The question is what structure should we use 
# for creating reading scales. In other data sets we have found that Initiating 
# falls out from SBR and IR. Of course, that may not be the case here.
# 
# This is preliminary analyses to allow us to pre-register a study that will
# look at these variables with reading skill (fluency, comprehension, and word
# reading).
# 
# Created: 2025-10-24
# Author: Serje Robidoux
# CHANGELOG:
# 

# 0.0 Setup ####
include(lavaan)
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

str(iowaData)

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

# 3.0 Measurement Models ####
# OK now that we have useful data, we can start building measurement models to
# compare.

## 3.1 Model structures ####
# For this analysis, we will consider three factor structures and pit them
# against each other.
# 1. a single factor model
# 2. a "duration/frequency" vs SBR model
# 3. a duration/frequency vs SBR vs IR vs Initiation model

### 3.1.1 Single Factor ####
## It occurs to me that this one is fully-specified, so might not really be
# suited to comparisons.

mmF1 = '
  SBR =~ 
        # Durations
        duration_read_alone_mins+duration_read_out_loud_mins+
        duration_shared_reading_mins+
        homework+reading_school_related+reading_nonschool_related+internet_reading+
        # parent to child - SBR
        parent_read_out_loud+parent_read_comics+parent_read_picture_books+
        parent_read_chapter_books+parent_read_nonfiction+
        # child to parent - SBR
        child_read_out_loud+child_read_comics+child_read_picture_book+
        child_read_chapter_books+child_read_nonfiction+
        # independent reading (IR)
        read_alone+read_alone_comics+read_alone_picture_books+
        read_alone_chapter_books+read_alone_nonfiction+read_alone_internet+
        # initiation
        initiate_shared_reading+initiate_read_out_loud+initiate_read_alone
'

### 3.1.2 Two Factor ####
# Separates "Duration/Frequency" from "SBR"
# 
mmF2 = '
  Duration =~ # Durations
        duration_read_alone_mins+duration_read_out_loud_mins+
        duration_shared_reading_mins+
        homework+reading_school_related+reading_nonschool_related+internet_reading
  SBR =~ # parent to child - SBR
        parent_read_out_loud+parent_read_comics+parent_read_picture_books+
        parent_read_chapter_books+parent_read_nonfiction+
        # child to parent - SBR
        child_read_out_loud+child_read_comics+child_read_picture_book+
        child_read_chapter_books+child_read_nonfiction+
        # independent reading (IR)
        read_alone+read_alone_comics+read_alone_picture_books+
        read_alone_chapter_books+read_alone_nonfiction+read_alone_internet+
        # initiation
        initiate_shared_reading+initiate_read_out_loud+initiate_read_alone
        
  Duration ~~ SBR
'

### 3.1.3 Four Factor ####
# Separates "Duration/Frequency" from "Shared Reading" From "Independent Reading"
# from Initiative.
# 
mmF4 = '
  Duration =~ # Durations
        duration_read_alone_mins+duration_read_out_loud_mins+
        duration_shared_reading_mins+
        homework+reading_school_related+reading_nonschool_related+internet_reading
  SBR =~ # parent to child - SBR
        parent_read_out_loud+parent_read_comics+parent_read_picture_books+
        parent_read_chapter_books+parent_read_nonfiction+
        # child to parent - SBR
        child_read_out_loud+child_read_comics+child_read_picture_book+
        child_read_chapter_books+child_read_nonfiction
  IR =~  # independent reading (IR)
        read_alone+read_alone_comics+read_alone_picture_books+
        read_alone_chapter_books+read_alone_nonfiction+read_alone_internet
  Init=~ # initiation
        initiate_shared_reading+initiate_read_out_loud+initiate_read_alone
        
  Duration ~~ SBR+IR+Init
  SBR ~~ IR + Init
  IR ~~ Init
'


## 3.2 Full Sample - age/grade invariant models ####
# First we'll do this across the full sample, ignoring differences in age

### 3.2.1 single factor ####

iowaF1 = cfa(mmF1, data=iowaData, std.ov=T)
fitmeasures(iowaF1, fit.measures = c("srmr", "cfi", "agfi", "rmsea"))
# terrible fit
#  srmr   cfi  agfi rmsea 
# 0.129 0.387 0.537 0.152
# 

### 3.2.2 two factor ####

iowaF2 = cfa(mmF2, data=iowaData, std.ov=T)
fitmeasures(iowaF2, fit.measures = c("srmr", "cfi", "agfi", "rmsea"))

# No real improvement
# srmr   cfi  agfi rmsea 
# 0.128 0.441 0.564 0.146 


### 3.2.3 Four factor ####

iowaF4 = cfa(mmF4, data=iowaData, std.ov=T)
fitmeasures(iowaF4, fit.measures = c("srmr", "cfi", "agfi", "rmsea"))

# More noticeable improvement, but still not a "good" fit.
#  srmr   cfi  agfi rmsea 
# 0.118 0.515 0.602 0.137

### 3.2.4 Compare models ####

anova(iowaF1, iowaF2, iowaF4)
# So the more complex model is clearly improved.


### 3.2.5 Exploratory/Factor Analysis ####
psych::scree(
  iowaData %>% 
    select(-participant, -Sample, -StudyYear, -StartGrade, -StartYear
           , -Grade, -starts_with("duration"), ends_with("_mins")) %>% 
    drop_na())

# hm. Scree plot says 3 factors, PCA says as many as 8 components.
# looking at the factors, 4 seems to make some sense.
factanal(iowaData %>% 
           select(-participant, -Sample, -StudyYear, -StartGrade, -StartYear
                  , -Grade, -starts_with("duration"), ends_with("_mins")) %>% 
           drop_na()
         , factors=4, rotation="promax") %>% 
  print(cutoff=.5)

# Using .5 cutoff to avoid cross-contamination of constructs.

# With three factors we get (with key variables):
# F1 - child reading to parent: child_read_*
# F2 - independent reading: read_alone, ..._chapter_book, initiate_read_alone (nonschool_related, duration_read_alone)
# F3 - parent reading to child: parent_read_out_loud, .._chapter_books (picture_books, nonfiction)
# F4 - school work: homework, reading school related

### 3.2.6 PCA ####
(iowa.pca = prcomp(iowaData %>% 
         select(-participant, -Sample, -StudyYear, -StartGrade, -StartYear
                , -Grade, -starts_with("duration"), ends_with("_mins")) %>% 
         drop_na()))$rotation %>% round(2)


# 4.0 Revised CFA ####

cfaRevModel = '
# F1 - child reading to parent: child_read_*
childSBR =~ child_read_out_loud+child_read_picture_book+child_read_comics+child_read_chapter_books+child_read_nonfiction

# F2 - independent reading: read_alone, ..._chapter_book, initiate_read_alone (nonschool_related, duration_read_alone)
IR =~ read_alone+read_alone_chapter_books+read_alone_comics+initiate_read_alone+reading_nonschool_related+duration_read_alone_mins

# F3 - parent reading to child: parent_read_out_loud, .._chapter_books (picture_books, nonfiction)
prntSBR =~ parent_read_out_loud+parent_read_chapter_books#+parent_read_picture_books+parent_read_nonfiction#+parent_read_comics

# F4 - school work: homework, reading school related - IGNORED since homework includes more than just reading

childSBR~~IR + prntSBR
IR ~~ prntSBR
'

cfaRevModelAlt = '
# F1 - SBR (both child and parent)
SBR =~ child_read_out_loud+child_read_picture_book+child_read_comics+child_read_chapter_books+child_read_nonfiction+
      parent_read_out_loud+parent_read_chapter_books#+parent_read_picture_books+parent_read_nonfiction#+parent_read_comics

# F2 - independent reading: read_alone, ..._chapter_book, initiate_read_alone (nonschool_related, duration_read_alone)
IR =~ read_alone+read_alone_chapter_books+read_alone_comics+initiate_read_alone+reading_nonschool_related+duration_read_alone_mins

# F3 - parent reading to child: parent_read_out_loud, .._chapter_books (picture_books, nonfiction)
#prntSBR =~ parent_read_out_loud+parent_read_chapter_books#+parent_read_picture_books+parent_read_nonfiction#+parent_read_comics

# F4 - school work: homework, reading school related - IGNORED since homework includes more than just reading

IR ~~ SBR
'

iowa.cfaRev = sem(cfaRevModel, data=iowaData, std.ov = T)
fitmeasures(iowa.cfaRev, fit.measures = c("srmr", "cfi", "agfi", "rmsea"))


iowa.cfaRevAlt = sem(cfaRevModelAlt, data=iowaData, std.ov = T)
fitmeasures(iowa.cfaRevAlt, fit.measures = c("srmr", "cfi", "agfi", "rmsea"))

include(semPlot)
semPaths(iowa.cfaRevAlt, whatLabels = "est")
semPaths(iowa.cfaRev, whatLabels = "est")

parameterEstimates(iowa.cfaRevAlt) %>% filter(rhs=="SBR")
