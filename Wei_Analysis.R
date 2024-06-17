#Testing out graphing Wei et al data

library(dplyr)
library(ggplot2)
soilC <- read.csv(file = 'GCAM_soilC.csv')
PostKwon <- read.csv(file= 'Experimental Data.csv', na.strings = c("", "NA"))
timescales <- read.csv(file = 'soil_timescales.csv')
glus <- read.csv(file = 'GLU_codes.csv')
regions <- read.csv('GCAM_regions.csv')
