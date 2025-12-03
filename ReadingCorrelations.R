###############################################################################
# iowa Project - Bob
# 
# This script is to examine the reading performance data more closely to help
# with choosing the right DV for the preregistration.
# 
# Created: 2025-12-03
# Author: Serje Robidoux
# CHANGELOG:
# 

# 0.0 Setup ####
include(readxl)

# 1.0 Data ####
iowaReading = read_xlsx("ReadAnx_Scores.xlsx", sheet="ReadAnx_Scores") %>% 
  select(participantID, Sample, Truegrade, starts_with("CC2"), starts_with("GORT"))
# str(iowaData) # All looks good

# 2.0 Correlations of reading scores by grade ####
# This will take each variable, look up the Options for that variable and,
# if necessary, recode it.

iowaReadingCorsbyGrade = tapply(iowaReading, iowaReading$Truegrade
       , function(x)
         cor(x %>% 
               rename(GORTFlu = GORT_Fluency, GORTComp=GORT_Comprehension) %>% 
               select(starts_with("CC2"), starts_with("GORT"), -CC2reg)
             , use="pairwise.complete") %>% round(3)
       )
