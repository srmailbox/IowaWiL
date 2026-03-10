###############################################################################
# Iowa Project - Q1 Reading Ability and Frequency
# 
# We do not always have data for all 4 motivation questions, but we would like
# to get "Motivation" scores based on whatever responses have been provided.
# Here I'm using FIML to "fill in" missing values, which is nearly identical to
# just using the average of the data that is available
# 
# Actually, this is completely unnecessary - I can literally just calculate the
# mean of available data.
# 
# Created: 2026-03-10
# Author: Serje Robidoux
# CHANGELOG:


source('Iowa.Data.R')

semModSpec = '
Motiv =~ 1*motiv_1+1*motiv_2+1*motiv_3+1*motiv_4
'

motivByGrade=by(
  iowaData, iowaData$Grade
  , function(x) {
    semMod = sem(semModSpec, x, missing="fiml")
    list(sem = semMod, dat = data.frame(x, predict(semMod)))
  }
)

# lapply(motivByGrade, function(x) parameterestimates(x$sem))

# lapply(motivByGrade, function(x) cor(x$dat$RI, x$dat$Motiv, use="pairwise.complete"))

iowaMotivations = lapply(motivByGrade, function(x) x$dat %>% select(participant, Grade, Motiv)) %>% bind_rows()

