#Load libraries
library(dplyr)
library(ggplot2)

#Join GLU codes with soilC data
soilC %>%
  mutate(GLU_code = GLU) %>%
  right_join(glus, by='GLU_code') %>%
  right_join(regions, by='GCAM_region_ID') %>%
  right_join(timescales, by='GCAM_region_ID')-> soilC_regions


#Simplify the data to just the stuff we'll need for the Post & Kwon comparisons 
soilC_with_regions %>%
  select(Land_Type, soil_c, GLU_code, Continent, soilTimeScale, GCAM_region_ID) -> simple_soilC_regions


#Now, we will start comparing GCAM to Post & Kwon
PostKwon <- read.csv(file= 'C:/Users/bian240/OneDrive - PNNL/Desktop/Initial Project Code/Experimental Data.csv', na.strings = c("", "NA"))

#Post & Kwon has some NA values but we'll just ignore those for now  
#Note: Post & Kwon data was manually changed to GCAM region data with the help of the following:
#https://stash.pnnl.gov/projects/JGCRI/repos/gcam-core/browse/input/gcamdata/inst/extdata/aglu/SAGE_LT.csv


#Matching experimental data to GCAM data
PostKwon %>%
  select(Initial_Land_Use, Final_Land_Use, GLU_code, GCAM_region_ID, Time, Exp_Rate) %>%
  na.omit() %>%
  mutate(Land_Type = Initial_Land_Use) %>%
  right_join( select(simple_soilC_with_regions, -Continent), by = c('GLU_code', 'Land_Type', 'GCAM_region_ID')) %>%
  rename(initial_soil_c = soil_c) %>%
  select(-Land_Type) %>%
  mutate(Land_Type = Final_Land_Use) %>%
  right_join( select(simple_soilC_with_regions, -Continent), by = c('GLU_code', 'Land_Type', 'GCAM_region_ID')) %>%
  rename(final_soil_c = soil_c) %>%
  select(-Land_Type, -soilTimeScale.x) %>%
  rename(soilTimeScale = soilTimeScale.y) %>%
  na.omit() %>%
  mutate(GCAM_Rate = (final_soil_c - initial_soil_c)/soilTimeScale, Rate_Difference = Exp_Rate - GCAM_Rate, 
         Exp_k = -log(abs(Exp_Rate)*Time +1)/Time,
         GCAM_k = -log(final_soil_c/initial_soil_c)/Time, 
         ) %>%
  #This next line corrects the sign of Exp_k--we had to take the absolute value to avoid NaNs, so this accounts for that 
  mutate(Exp_k = ifelse(sign(Exp_k) == sign(Exp_Rate), Exp_k*(-1), Exp_k)) -> Rate_Comparison


#Plot the two rates against each other with a 1:1 line as well
ggplot(data = Rate_Comparison, aes(x = Exp_Rate, y = GCAM_Rate)) + 
  geom_point(aes(shape = `Final_Land_Use`, color = `Initial_Land_Use`, size = 2)) + 
  scale_shape(solid = FALSE) +
  geom_abline() + 
  xlab('Experimental Rates') + ylab('GCAM Derived Rates')


#Plot overlapping rate histograms for the different rate sources
ggplot() +
  geom_histogram(aes(x = Rate_Comparison$Exp_Rate, fill ='Experimental Rate' ), alpha = 0.5) +
  geom_histogram(aes(x = Rate_Comparison$GCAM_Rate, fill = 'GCAM Rate'), alpha = 0.5) +
  xlab('Rate') + ylab('Count') +
  scale_fill_manual(values = c('Experimental Rate' = '#45912c', 'GCAM Rate'='#e3962b'))


#Plot overlapping k histograms for the different k sources
ggplot() +
  geom_histogram(aes(x = Rate_Comparison$Exp_k,fill ='Experimental k'), alpha = 0.5) +
  geom_histogram(aes(x = Rate_Comparison$GCAM_k,  fill = 'GCAM k'), alpha = 0.5) +
  xlab('k') + ylab('Count') +
  scale_fill_manual(values = c('Experimental k' = '#45912c', 'GCAM k'='#e3962b'))


#T-tests, just for funsies
t.test(Rate_Comparison$Exp_Rate, Rate_Comparison$GCAM_Rate, alternative = 'two.sided') ->Rate_T_test
#According to this, there is not a meaningful difference in the means

t.test(Rate_Comparison$Exp_k, Rate_Comparison$GCAM_k, alternative = 'two.sided') ->k_T_test
#According to this, there is a meaningful difference in the means
