#Testing out graphing Wei et al data

library(dplyr)
library(ggplot2)
soilC <- read.csv(file = 'Data/GCAM_soilC.csv')
Wei <- read.csv(file= 'Data/Wei et al Data.csv', na.strings = c("", "NA"))
timescales <- read.csv(file = 'Data/soil_timescales.csv')
glus <- read.csv(file = 'Data/GLU_codes.csv')
regions <- read.csv('Data/GCAM_regions.csv')


#Join GLU codes with soilC data
soilC %>%
  mutate(GLU_code = GLU) %>%
  right_join(glus, by='GLU_code') %>%
  right_join(regions, by='GCAM_region_ID') %>%
  right_join(timescales, by='GCAM_region_ID')-> soilC_regions


#Simplify the data to just the stuff we'll need for the Post & Kwon comparisons 
soilC_regions %>%
  select(Land_Type, soil_c, GLU_code, soilTimeScale, GCAM_region_ID) -> simple_soilC_regions

#Matching experimental data to GCAM data
Wei %>%
  select(Initial_Land_Use, Final_Land_Use, GLU_code, GCAM_region_ID, OC_decrease, Time) %>%
  na.omit() %>%
  mutate(Land_Type = Initial_Land_Use) %>%
  right_join(simple_soilC_regions, by = c('GLU_code', 'Land_Type', 'GCAM_region_ID')) %>%
  rename(initial_soil_c = soil_c) %>%
  select(-Land_Type) %>%
  mutate(Land_Type = Final_Land_Use) %>%
  right_join(simple_soilC_regions, by = c('GLU_code', 'Land_Type', 'GCAM_region_ID')) %>%
  rename(final_soil_c = soil_c) %>%
  select(-Land_Type, -soilTimeScale.x) %>%
  rename(soilTimeScale = soilTimeScale.y) %>%
  na.omit() %>%
  mutate(GCAM_Rate = (final_soil_c - initial_soil_c)/soilTimeScale,
         GCAM_k = -log(final_soil_c/initial_soil_c)/Time,
         Exp_k = -log(1/((abs(OC_decrease)/100) +1))/Time,
  ) %>%
  #This next line corrects the sign of Exp_k--we had to take the absolute value to avoid NaNs, so this accounts for that 
  mutate(Exp_k = ifelse(sign(Exp_k) == sign(OC_decrease), Exp_k, Exp_k*(-1))) -> Rate_Comparison


#Plot the two k vals against each other with a 1:1 line as well
ggplot(data = Rate_Comparison, aes(x = Exp_k, y = GCAM_k)) + 
  geom_point(aes(shape = Final_Land_Use, color = Initial_Land_Use), size = 2) + 
  scale_shape_manual(values = c(4, 8, 16, 17)) +
  scale_shape(solid = TRUE) +
  geom_abline() + 
  xlab('Experimental k (1/y)') + ylab('GCAM Derived k (1/y)') +
  theme_light() + 
  xlim(-.15, .35) + ylim(-.15, .35)  +
  labs(title = 'SOC rate comparison during land use transition', color =  'Initial Land Use', shape = 'Final Land Use')


#Plot overlapping k histograms for the different k sources
ggplot() +
  geom_histogram(aes(x = Rate_Comparison$Exp_k,fill ='Experimental'), alpha = 0.5) +
  geom_histogram(aes(x = Rate_Comparison$GCAM_k,  fill = 'GCAM'), alpha = 0.5) +
  xlab(expression(k~(y^-1))) + ylab('Count') +
  scale_fill_manual(name = "Data Source", values = c('Experimental' = '#45912c', 'GCAM'='#e3962b')) +
  theme_light() 

#T test
t.test(Rate_Comparison$Exp_k, Rate_Comparison$GCAM_k, alternative = 'two.sided') ->k_T_test
#According to this, there IS not a meaningful difference in the means
