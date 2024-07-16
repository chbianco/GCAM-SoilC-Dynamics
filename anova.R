library(tidyr)
library(metafor)
library(dplyr)
library(ggplot2)

#Load GCAM data
soilC <- read.csv(file = 'Data/GCAM_soilC.csv')
timescales <- read.csv(file = 'Data/soil_timescales.csv')
glus <- read.csv(file = 'Data/GLU_codes.csv')
regions <- read.csv('Data/GCAM_regions.csv')

#Load experimental data
PostKwon <- read.csv(file= 'Data/Experimental Data.csv', na.strings = c("", "NA"))
Wei <- read.csv(file= 'Data/Wei et al Data.csv', na.strings = c("", "NA"))


#Join GLU codes with soilC data
soilC %>%
  mutate(GLU_code = GLU) %>%
  right_join(glus, by='GLU_code') %>%
  right_join(regions, by='GCAM_region_ID') %>%
  right_join(timescales, by='GCAM_region_ID')-> soilC_regions


#Simplify the data to just the stuff we'll need to compare with experimental data
soilC_regions %>%
  select(Land_Type, soil_c, GLU_code, soilTimeScale, GCAM_region_ID, Basin_long_name) -> simple_soilC_regions

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
  select(-Land_Type, -soilTimeScale.x, -Basin_long_name.x) %>%
  rename(soilTimeScale = soilTimeScale.y, Basin_long_name = Basin_long_name.y) %>%
  na.omit() %>%
  mutate(GCAM_Rate = (final_soil_c - initial_soil_c)/soilTimeScale, Rate_Difference = Exp_Rate - GCAM_Rate, 
         Exp_k = -log(abs(Exp_Rate)*Time +1)/Time,
         GCAM_k = -log(final_soil_c/initial_soil_c)/soilTimeScale, 
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
  select(-Land_Type, -soilTimeScale.x, -Basin_long_name.x) %>%
  rename(soilTimeScale = soilTimeScale.y, Basin_long_name = Basin_long_name.y) %>%
  na.omit() %>%
  mutate(GCAM_Rate = (final_soil_c - initial_soil_c)/soilTimeScale,
         GCAM_k = -log(final_soil_c/initial_soil_c)/soilTimeScale,
         Exp_k = -log(1/((abs(OC_decrease)/100) +1))/Time,
         source = 'Wei et al'
  ) %>%
  #This next line corrects the sign of Exp_k--we had to take the absolute value to avoid NaNs, so this accounts for that 
  mutate(Exp_k = ifelse(sign(Exp_k) == sign(OC_decrease), Exp_k, Exp_k*(-1))) -> Wei_Comparison

#Merging the two papers...
#Now, we bind the two rows together. Because the Wei et al data doesn't have any raw rates, we won't include that data from Post & Kwon either
Full_Comparison <- bind_rows(
  select(PostKwon_Comparison, -Exp_Rate, -Rate_Difference),
  select(Wei_Comparison, -OC_decrease)
)

#EVERYTHING ABOVE THIS LITERALLY JUST LOADS DATA!!!! DON'T CHANGE IT!!!!!

#Make data longer
Full_Comparison %>%
  pivot_longer(cols = Exp_k:GCAM_k,
               names_to = "Type",
               values_to = "k") -> full_long_data

Wei_Comparison %>%
  pivot_longer(cols = Exp_k:GCAM_k,
                            names_to = "Type",
                            values_to = "k") -> Wei_long_data

PostKwon_Comparison %>%
  pivot_longer(cols = Exp_k:GCAM_k,
               names_to = "Type",
               values_to = "k") -> PostKwon_long_data


#Full ANOVA
aov_Full <- aov(k ~ Type + Basin_long_name,
                      data = full_long_data)
summary(aov_Full)
TukeyHSD(aov_Full)


#PostKwon ANOVA
aov_PostKwon <- aov(k ~ Type + Basin_long_name,
                     data = PostKwon_long_data)
summary(aov_PostKwon)
TukeyHSD(aov_PostKwon)


#Wei ANOVA
aov_Wei <- aov(k ~ Type + Basin_long_name,
                    data = Wei_long_data)
summary(aov_Wei)
TukeyHSD(aov_Wei)

#First, let's add transition type
full_long_data %>% 
  mutate(change = paste(Initial_Land_Use, Final_Land_Use, sep = '')) -> change_long_data

#Now, we'll do some averages
#Average by change
change_long_data %>%
  group_by(Type, change) %>%
  summarize(mean_k = mean(k), std_dev_k = sd(k)) -> change_grouped_long

aov_change <- aov(mean_k ~ Type + change,
               data = change_grouped_long)
summary(aov_change)


#Average by region
change_long_data %>%
  group_by(Type, Basin_long_name) %>%
  summarize(mean_k = mean(k), std_dev_k = sd(k)) -> basin_grouped_long

aov_basin <- aov(mean_k ~ Type + Basin_long_name,
                  data = basin_grouped_long)
summary(aov_change)

#Averages by region per paper
#Post & Kwon
change_long_data %>%
  filter(source == 'Post & Kwon') %>%
  group_by(Type, Basin_long_name) %>%
  summarize(mean_k = mean(k), std_dev_k = sd(k)) -> PostKwon_basin_grouped_long

aov_basin_postkwon <- aov(mean_k ~ Type + Basin_long_name,
                 data = PostKwon_basin_grouped_long)
summary(aov_basin_postkwon)

#Wei et al
change_long_data %>%
  filter(source == 'Wei et al') %>%
  group_by(Type, Basin_long_name) %>%
  summarize(mean_k = mean(k), std_dev_k = sd(k)) -> Wei_basin_grouped_long

aov_basin_wei <- aov(mean_k ~ Type + Basin_long_name,
                          data = Wei_basin_grouped_long)
summary(aov_basin_postkwon)



  