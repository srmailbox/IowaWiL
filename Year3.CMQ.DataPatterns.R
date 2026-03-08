merge(iowaInterest %>% 
  # select(starts_with("motiv"), StudyYear) %>% 
  mutate(across(-StudyYear, is.na)) %>% 
  group_by(pick(starts_with("motiv"), StudyYear)) %>% summarize(nPattern=n())
  , iowaInterest %>% 
  group_by(StudyYear) %>% summarize(n=n())
  , by="StudyYear") %>% 
  mutate(patternP = round(nPattern/n,3)) %>% 
  arrange(pick(starts_with("motiv")))

# There's something odd about Year 3, where the response rate overall is huge
# (98.4%) but very few provide a full set of data (53.7% - compared to 75-100%
# for the other Study Years). Further, for some reason a tonne of them have values
# for just 3,4,5 and not 1,2 (22% vs 0-2% for that exact pattern)
# 
### 2026-03-07: New file provided, fixes issues with missingness in Year 5, but
### the Year 3 issue remains - it's been verified that these are the data, but
### it's not clear why it occurred. Change in RedCap system perhaps?