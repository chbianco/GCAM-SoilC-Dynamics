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

#Merging the two papers...
#Now, we bind the two rows together. Because the Wei et al data doesn't have any raw rates, we won't include that data from Post & Kwon either
Full_Comparison <- bind_rows(
  select(PostKwon_Comparison, -Exp_Rate, -Rate_Difference),
  select(Wei_Comparison, -OC_decrease)
)


#EVERYTHING ABOVE THIS LITERALLY JUST LOADS DATA!!!! DON'T CHANGE IT!!!!!
##ow, we get into the actual meta analysis 

#First, we need to get mean, std dev, and n
PostKwon_Comparison %>%
  group_by(GLU_code) %>%
  summarize(mean_control = mean(Exp_Rate), sd_control= sd(Exp_Rate), n_control = n(),
            mean_GCAM = mean(GCAM_Rate), sd_GCAM = sd(GCAM_Rate), n_GCAM = n()
            ) -> PostKwon_MA_data

#Now, we can use escalc to get the standardized mean difference 
PostKwon_effect_sizes <-
  escalc('SMD',
    m1i = mean_control, n1i = n_control, sd1i = sd_control,
    m2i = mean_GCAM, n2i = n_GCAM, sd2i = sd_GCAM,
    data = PostKwon_MA_data
  )

#We're going to use a fixed effect model for this analysis, which we set up below
PostKwon_fixed_effect_results <- rma(yi, vi, method = 'FE',
                                     slab = GLU_code,
                                     data = PostKwon_effect_sizes)

#Forest plot for Post & Kwon
forest(
  PostKwon_effect_sizes$yi, PostKwon_effect_sizes$vi,
  annotate = FALSE,
  slab = PostKwon_fixed_effect_results$slab,
  xlab = 'ln(Response Ratio)',
  #Below sets the size of study labels, shape of bars, and size of x labels 
  cex = .8, pch = 15, cex.lab = 1
)

#Adding the summary effect size
addpoly(
  PostKwon_fixed_effect_results, 
  col = 'orange', cex = 1, annotate = TRUE, mlab = 'Summary'
)


#Now we do the same for Wei et al.
#First, we need to get mean, std dev, and n
Wei_Comparison %>%
  group_by(GLU_code) %>%
  summarize(mean_control = mean(Exp_k), sd_control= sd(Exp_k), n_control = n(),
            mean_GCAM = mean(GCAM_k), sd_GCAM = sd(GCAM_k), n_GCAM = n()
  ) -> Wei_MA_data

#Now, we can use escalc to get the standardized mean difference 
Wei_effect_sizes <-
  escalc('SMD',
         m1i = mean_control, n1i = n_control, sd1i = sd_control,
         m2i = mean_GCAM, n2i = n_GCAM, sd2i = sd_GCAM,
         data = Wei_MA_data
  )

#We're going to use a fixed effect model for this analysis, which we set up below
Wei_fixed_effect_results <- rma(yi, vi, method = 'FE',
                                     slab = GLU_code,
                                     data = Wei_effect_sizes)

#Forest plot for Post & Kwon
forest(
  Wei_effect_sizes$yi, Wei_effect_sizes$vi,
  annotate = FALSE,
  slab = Wei_fixed_effect_results$slab,
  xlab = 'ln(Response Ratio)',
  #Below sets the size of study labels, shape of bars, and size of x labels 
  cex = .8, pch = 15, cex.lab = 1
)

#Adding the summary effect size
addpoly(
  Wei_fixed_effect_results, 
  col = 'orange', cex = 1, annotate = TRUE, mlab = 'Summary'
)



#Now we do it for all the data...
#First, we need to get mean, std dev, and n
Full_Comparison %>%
  group_by(GLU_code) %>%
  summarize(mean_control = mean(Exp_k), sd_control= sd(Exp_k), n_control = n(),
            mean_GCAM = mean(GCAM_k), sd_GCAM = sd(GCAM_k), n_GCAM = n()
  ) -> Full_MA_data

#Now, we can use escalc to get the standardized mean difference 
Full_effect_sizes <-
  escalc('SMD',
         m1i = mean_control, n1i = n_control, sd1i = sd_control,
         m2i = mean_GCAM, n2i = n_GCAM, sd2i = sd_GCAM,
         data = Full_MA_data
  )

#We're going to use a fixed effect model for this analysis, which we set up below
Full_fixed_effect_results <- rma(yi, vi, method = 'FE',
                                     slab = GLU_code,
                                     data = Full_effect_sizes)

#Forest plot for full dataset
forest(
  Full_effect_sizes$yi, Full_effect_sizes$vi,
  annotate = TRUE,
  slab = Full_fixed_effect_results$slab,
  xlab = 'ln(Response Ratio)',
  #Below sets the size of study labels, shape of bars, and size of x labels 
  cex = .8, pch = 15, cex.lab = 1
)

#Adding the summary effect size
addpoly(
  Full_fixed_effect_results, 
  col = 'orange', cex = 1, annotate = TRUE, mlab = 'Summary'
)


