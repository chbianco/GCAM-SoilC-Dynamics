#Making the rmap showing where experimental data is from 

#Load packages
library(dplyr)
library(ggplot2)
library(rmap)
library(RColorBrewer)
library(graphics)

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

#EVERTHING ABOVE THIS JUST LOADS DATA!!!! DO NOT CHANGE!!!!
mapGCAMBasins -> mapGCAMBasins

#Creating the dataframe we will load into rmap
Full_Comparison %>%
  select(GLU_code, Basin_long_name, GCAM_region_ID, source) %>%
  count(Basin_long_name)-> location_freq


data.frame(subRegion = location_freq$Basin_long_name, value = location_freq$n) %>%
  mutate(value = value * -1) -> location_freq_data  

location_freq_data %>% mutate(value = value * -1) -> location_freq_data 

#Now, we need to rename some basins...
new_basins <- function(land){
  if(grepl('Arkansas_White_Red_Basin', land)){return('Arkansas_White_Red')}
  else if(grepl('Brahmani', land)){return('Brahamani')}
  else if(grepl('Great_Lakes_Basin', land)){return('Great_Lakes')}
  else if(grepl('Hong_\\(Red_River\\)', land)){return('Hong_Red_River')}
  else if(grepl('Lower_Colorado_River_Basin',land)){return('Lower_Colorado_River')}
  else if(grepl('Mahanadi', land)){return('Mahandi')}
  else if(grepl('Missouri_River_Basin', land)){return('Missouri_River')}
  else if(grepl('New_England_Basin', land)){return('New_England')}
  else if(grepl('Ohio_River_Basin', land) ){return('Ohio_River')}
  else if(grepl('Pacific_Northwest_Basin', land) ){return('Pacific_Northwest')}
  else if(grepl('South_Atlantic_Gulf_Basin', land) ){return('South_Atlantic_Gulf')}
  else if(grepl('Upper_Mississippi_Basin', land) ){return('Upper_Mississippi')}
  else{return(land)}
}

#Apply the function to the dataset
sapply(location_freq_data$subRegion, new_basins) -> better_basins

location_freq_data %>%
  mutate(subRegion = better_basins) -> location_freq_data

#Making the map look pretty 
# Build your palette
getcol = colorRampPalette(brewer.pal(6, 'Oranges'));
reds = getcol(5)[1:5];
my_pal <- c((reds))
pie(rep(1,length(my_pal)),label=names(my_pal),col=my_pal)

# Create a list of ranges and categorical color scales for each parameter
numeric2Cat_param <- list("param")
numeric2Cat_breaks <- (list(c(0, 1, 4, 10, 20, 30)))
numeric2Cat_labels <- (list(c('1',"1 to 4", "4 to 10", "10 to 20", "20 to 30")))
names(my_pal) <- unlist(numeric2Cat_labels)
pie(rep(1,length(my_pal)),label=names(my_pal),col=my_pal)
numeric2Cat_palette <- list(my_pal) # Can be a custom scale or an R brewer palette or an rmap palette
numeric2Cat_legendTextSize <- list(c(1))
numeric2Cat_list <-list(numeric2Cat_param = numeric2Cat_param,
                        numeric2Cat_breaks = numeric2Cat_breaks,
                        numeric2Cat_labels = numeric2Cat_labels,
                        numeric2Cat_palette = numeric2Cat_palette,
                        numeric2Cat_legendTextSize = numeric2Cat_legendTextSize); numeric2Cat_list



#Creating the map
rmap::map(location_freq_data, 
          overLayer= mapGCAMBasins,
          numeric2Cat_list = numeric2Cat_list,
          crop = FALSE
          )


