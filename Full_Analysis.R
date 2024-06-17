#Load packages
library(dplyr)
library(ggplot2)

#Load GCAM data
soilC <- read.csv(file = 'GCAM_soilC.csv')
timescales <- read.csv(file = 'soil_timescales.csv')
glus <- read.csv(file = 'GLU_codes.csv')
regions <- read.csv('GCAM_regions.csv')

#Load experimental data
PostKwon <- read.csv(file= 'Experimental Data.csv', na.strings = c("", "NA"))
Wei <- read.csv(file= 'Wei et al Data.csv', na.strings = c("", "NA"))


#Join GLU codes with soilC data
soilC %>%
  mutate(GLU_code = GLU) %>%
  right_join(glus, by='GLU_code') %>%
  right_join(regions, by='GCAM_region_ID') %>%
  right_join(timescales, by='GCAM_region_ID')-> soilC_regions


#Simplify the data to just the stuff we'll need to compare with experimental data
soilC_regions %>%
  select(Land_Type, soil_c, GLU_code, soilTimeScale, GCAM_region_ID) -> simple_soilC_regions

#Creating the Post & Kwon comparison data 
PostKwon %>%
  select(Initial_Land_Use, Final_Land_Use, GLU_code, GCAM_region_ID, Time, Exp_Rate) %>%
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
  mutate(GCAM_Rate = (final_soil_c - initial_soil_c)/soilTimeScale, Rate_Difference = Exp_Rate - GCAM_Rate, 
         Exp_k = -log(abs(Exp_Rate)*Time +1)/Time,
         GCAM_k = -log(final_soil_c/initial_soil_c)/Time, 
         source = 'Post & Kwon'
  ) %>%
  #This next line corrects the sign of Exp_k--we had to take the absolute value to avoid NaNs, so this accounts for that 
  mutate(Exp_k = ifelse(sign(Exp_k) == sign(Exp_Rate), Exp_k*(-1), Exp_k)) -> PostKwon_Comparison


#Creating the Wei et al comparison data
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
         source = 'Wei et al'
  ) %>%
  #This next line corrects the sign of Exp_k--we had to take the absolute value to avoid NaNs, so this accounts for that 
  mutate(Exp_k = ifelse(sign(Exp_k) == sign(OC_decrease), Exp_k, Exp_k*(-1))) -> Wei_Comparison

#Now, we bind the two rows together. Because the Wei et al data doesn't have any raw rates, we won't include that data from Post & Kwon either
Full_Comparison <- bind_rows(
  select(PostKwon_Comparison, -Exp_Rate, -Rate_Difference),
  select(Wei_Comparison, -OC_decrease)
  )

#Plot the two k vals against each other with a 1:1 line as well
ggplot(data = Full_Comparison, aes(x = Exp_k, y = GCAM_k)) + 
  geom_point(aes(shape = Final_Land_Use, color = Initial_Land_Use), size = 2) + 
  scale_shape_manual(values = c(4, 8, 16, 17)) +
  scale_shape(solid = TRUE) +
  geom_abline() + 
  xlab('Experimental k (1/y)') + ylab('GCAM Derived k (1/y)') +
  theme_light() + 
  xlim(-.1, .4) + ylim(-.1, .4)  +
  labs(title = 'SOC k comparison during land use transition', color =  'Initial Land Use', shape = 'Final Land Use')


#Plot overlapping k histograms for the different k sources
ggplot() +
  geom_histogram(aes(x = Full_Comparison$Exp_k,fill ='Experimental k'), alpha = 0.5) +
  geom_histogram(aes(x = Full_Comparison$GCAM_k,  fill = 'GCAM k'), alpha = 0.5) +
  xlab('k (y^-1)') + ylab('Count') +
  scale_fill_manual(name = "Data Source", values = c('Experimental k' = '#45912c', 'GCAM k'='#e3962b')) +
  theme_light() +
  labs(title = 'SOC k value comparison during land use transition')
