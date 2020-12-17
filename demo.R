# demo using gimmedata

source('helpers.R')
# in case of error, install.packages('tidyverse')

DataDir <- '/home/maximilien.chaumon/ownCloud/Lab/00-Projects/TimeSocialDistancing/DATA/data_exp_201215'
mydata <- gimmedata(DataDir,ExperimentName = 'JP', UniqueName = 'TapVis', Session = 1)



