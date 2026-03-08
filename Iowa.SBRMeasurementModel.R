###############################################################################
# Iowa Project
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
# 2026-01-22 Modified to use a script for data processing.
# 2026-03-07: New data provided, including a 5th year of data collection.

# 0.0 Setup ####
include(lavaan)
include(readxl)
include(umx)

# 1.0 Data ####
# 2.0 Duration fields ####
# These two sections have been moved and encapsulated in a separate scripts to
# be shared by all analyses.
source("Iowa.Data.R")

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

iowaF1 = cfa(mmF1, data=iowaEnv, std.ov=T, missing="fiml", missing="fiml")
fitmeasures(iowaF1, fit.measures = c("srmr", "rmsea", "cfi", "agfi", "tli"))
# terrible fit
#  srmr rmsea   cfi  agfi   tli 
# 0.126 0.135 0.447 0.517 0.399
# 

### 3.2.2 two factor ####

iowaF2 = cfa(mmF2, data=iowaEnv, std.ov=T, missing="fiml")
fitmeasures(iowaF2, fit.measures = c("srmr", "rmsea", "cfi", "agfi", "tli"))

# No real improvement
#  srmr rmsea   cfi  agfi   tli 
# 0.125 0.132 0.472 0.534 0.424 


### 3.2.3 Four factor ####

iowaF4 = cfa(mmF4, data=iowaEnv, std.ov=T, missing="fiml")
fitmeasures(iowaF4, fit.measures = c("srmr", "rmsea", "cfi", "agfi", "tli"))

# More noticeable improvement, but still not a "good" fit.
#  srmr rmsea   cfi  agfi   tli
# 0.124 0.123 0.551 0.581 0.501

### 3.2.4 Compare models ####

anova(iowaF1, iowaF2, iowaF4)
# So the more complex model is clearly improved.


### 3.2.5 Exploratory/Factor Analysis ####
# 2026-03-07: In StudyYears 3-5, there are no complete data - so we need to take
# a FIML approach, using umx

efaF4 = umxEFA(iowaEnv %>% 
         select(-participant, -Sample, -StudyYear, -StartGrade, -StartYear
                , -Grade, -starts_with("duration"), ends_with("_mins"), -Fi, -Fsbr)
       , factors=4, rotation="varimax")

# fits aren't great, but ok CFI = 0.809; TLI = 0.727; RMSEA = 0.09
# F1: parent, F2 - picture books, F3 - child SBR, F4-independent

efaF3 = umxEFA(iowaEnv %>% 
                 select(-participant, -Sample, -StudyYear, -StartGrade, -StartYear
                        , -Grade, -starts_with("duration"), ends_with("_mins"), -Fi, -Fsbr)
               , factors=3, rotation="varimax")

# bad fits CFI = 0.747; TLI = 0.671; RMSEA = 0.099
# F1: parent SBR
# F2: child SBR
# F3: independent reading

efaF2 = umxEFA(iowaEnv %>% 
                 select(-participant, -Sample, -StudyYear, -StartGrade, -StartYear
                        , -Grade, -starts_with("duration"), ends_with("_mins"), -Fi, -Fsbr)
               
               , factors=2, rotation="promax")

# Terrible fits again.CFI = 0.614; TLI = 0.543; RMSEA = 0.117
# F1: muddled SBR - both parent and child
# F2: read alone

### 3.2.6 PCA ####
iowa.pca=umxEFA(iowaEnv %>% 
         select(-participant, -Sample, -StudyYear, -StartGrade, -StartYear
                , -Grade, -starts_with("duration"), ends_with("_mins"), -Fi, -Fsbr)
       , factors=9, rotation="none")


# 4.0 Revised CFA ####

cfaRevModel = '
# F1 - child reading to parent: child_read_*
childSBR =~ child_read_out_loud+child_read_picture_book+child_read_comics+child_read_chapter_books+child_read_nonfiction

# F2 - independent reading: read_alone, ..._chapter_book, initiate_read_alone (nonschool_related, duration_read_alone)
IR =~ read_alone+read_alone_chapter_books+read_alone_comics+initiate_read_alone#+reading_nonschool_related+duration_read_alone_mins

# F3 - parent reading to child: parent_read_out_loud, .._chapter_books (picture_books, nonfiction)
prntSBR =~ parent_read_out_loud+parent_read_chapter_books+parent_read_picture_books+parent_read_nonfiction#+parent_read_comics

# F4 - school work: homework, reading school related - IGNORED since homework includes more than just reading

childSBR~~IR + prntSBR
IR ~~ prntSBR
'

cfaRevModelAlt = '
# F1 - SBR (both child and parent)
SBR =~ child_read_out_loud+child_read_picture_book+child_read_comics+child_read_chapter_books+child_read_nonfiction+
      parent_read_out_loud+parent_read_chapter_books#+parent_read_picture_books+parent_read_nonfiction#+parent_read_comics

# F2 - independent reading: read_alone, ..._chapter_book, initiate_read_alone (nonschool_related, duration_read_alone)
IR =~ read_alone+read_alone_chapter_books+read_alone_comics+initiate_read_alone#+reading_nonschool_related+duration_read_alone_mins

# F3 - parent reading to child: parent_read_out_loud, .._chapter_books (picture_books, nonfiction)
#prntSBR =~ parent_read_out_loud+parent_read_chapter_books#+parent_read_picture_books+parent_read_nonfiction#+parent_read_comics

# F4 - school work: homework, reading school related - IGNORED since homework includes more than just reading

IR ~~ SBR
'

iowa.cfaRev = sem(cfaRevModel, data=iowaEnv, std.ov = T, missing="fiml")
fitmeasures(iowa.cfaRev, fit.measures = c("srmr", "rmsea", "cfi", "agfi", "tli"))


iowa.cfaRevAlt = sem(cfaRevModelAlt, data=iowaEnv, std.ov = T, missing="fiml")
fitmeasures(iowa.cfaRevAlt, fit.measures = c("srmr", "rmsea", "cfi", "agfi", "tli"))

include(semPlot)
semPaths(iowa.cfaRevAlt, whatLabels = "est")
semPaths(iowa.cfaRev, whatLabels = "est")

parameterEstimates(iowa.cfaRevAlt) %>% filter(lhs=="SBR")

cor(predict(iowa.cfaRevAlt), predict(iowa.cfaRev), use="pairwise.complete")

## OK, so despite the slight shift in the FA results, it seems that we are still
## ok to just use the two factor model with childSBR and independent reading.
## 
## TO BE CONFIRMED WITH THE CORRECTED Env Data for Year 5