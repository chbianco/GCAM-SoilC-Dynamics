library(dplyr)
library(ggplot2)

#these files should contain everything needed but need to be checked
glus <- read.csv(file = "C:/Users/bian240/OneDrive - PNNL/Desktop/Initial Project Code/GLU_codes.csv")
regions <- read.csv(file = "C:/Users/bian240/OneDrive - PNNL/Desktop/Initial Project Code/GCAM_regions.csv") 
soilC <- read.csv(file = "C:/Users/bian240/OneDrive - PNNL/Desktop/Initial Project Code/GCAM_soilC.csv")
timescales <- read.csv(file = 'C:/Users/bian240/OneDrive - PNNL/Desktop/Initial Project Code/soil_timescales.csv')

str(soilC)

soilC %>%
  #filter to one region to make data more manageable
  filter(GCAM_region_ID == 1) %>%
  #Get average and std deviation for each land type
  group_by(Land_Type) %>%
  summarise_at(vars(soil_c), list(mean = mean, sd = sd)) -> soilC_avg

#Plot the average soilC with std dev as error bars 
ggplot(data = soilC_avg, aes(x=Land_Type, y=mean)) + 
  geom_bar(stat='identity') + 
  geom_errorbar(aes( ymin = mean-sd, ymax=mean+sd))


#Now, lets do this for all the regions:
soilC %>%
  group_by(GCAM_region_ID, Land_Type) %>%
  summarise_at(vars(soil_c), list(mean = mean, sd = sd)) -> all_soilC_avg

ggplot(data = all_soilC_avg, aes(x=Land_Type, y=mean)) + 
  geom_bar(stat='identity') + 
  geom_errorbar(aes( ymin = mean-sd, ymax=mean+sd)) +
  facet_wrap(~GCAM_region_ID)


#Join GLU codes with soilC data
soilC %>%
  mutate(GLU_code = GLU) %>%
  right_join(glus, by='GLU_code') %>%
  right_join(regions, by='GCAM_region_ID') %>%
  right_join(timescales, by='GCAM_region_ID')-> soilC_with_regions


#Simplify the data to just the stuff we'll need for the Post & Kwon comparisons 
soilC_with_regions %>%
  select(Land_Type, soil_c, GLU_code, Continent, soilTimeScale, GCAM_region_ID) -> simple_soilC_with_regions


#Simple plotting exercise: plot the average soilC for grassland, forest, and cropland for each continent
simple_soilC_with_regions %>%
  filter(Land_Type == 'Forest' | Land_Type == 'Grassland' | Land_Type == 'Cropland') %>%
  group_by(Continent, Land_Type) %>%
  summarise_at(vars(soil_c), list(mean = mean, sd = sd)) -> Continent_soilC_avg

ggplot(data = Continent_soilC_avg, aes(x=Land_Type, y=mean, fill = Land_Type)) + 
  geom_bar(stat='identity') + 
  geom_errorbar(aes( ymin = mean-sd, ymax=mean+sd)) +
  facet_wrap(~Continent) +
  scale_fill_manual(values=c('#f5ce42', '#298a17', '#526cde')) +
  xlab('Land Type') + ylab('Soil Carbon (kg/m^2)')


#Now, we will start comparing GCAM to Post & Kwon
PostKwon <- read.csv(file= 'C:/Users/bian240/OneDrive - PNNL/Desktop/Initial Project Code/Post_Kwon.csv', na.strings = c("", "NA"))

#Post & Kwon data isn't complete but this should work generally...
#Note: Post & Kwon data was manually changed to GCAM region data with the help of the following:
#https://stash.pnnl.gov/projects/JGCRI/repos/gcam-core/browse/input/gcamdata/inst/extdata/aglu/SAGE_LT.csv

PostKwon %>%
  na.omit() %>%
  mutate(Land_Type = Initial_Land_Use) %>%
  right_join( select(simple_soilC_with_regions, -Continent), by = c('GLU_code', 'Land_Type', 'GCAM_region_ID')) %>%
  rename(initial_soil_c = soil_c) %>%
  select(-Land_Type, -Rate_old) %>%
  mutate(Land_Type = Final_Land_Use) %>%
  right_join( select(simple_soilC_with_regions, -Continent), by = c('GLU_code', 'Land_Type', 'GCAM_region_ID')) %>%
  rename(final_soil_c = soil_c) %>%
  select(-Land_Type, -soilTimeScale.x) %>%
  rename(soilTimeScale = soilTimeScale.y) %>%
  na.omit() %>%
  mutate(GCAM_Rate = (final_soil_c - initial_soil_c)/soilTimeScale, Rate_Difference = Rate - GCAM_Rate) %>%
  mutate(Experimental_k = -log(abs(Rate)*Time +1)/Time) %>%
  mutate(GCAM_k = -log(final_soil_c/initial_soil_c)/Time) -> Rate_Comparison

#I had to to the absolute value of Rate in the ln expression to avoid lns--not sure how we account for that


#Plot the two rates against each other with a 1:1 line as well
ggplot(data = Rate_Comparison, aes(x = Rate, y = GCAM_Rate)) + 
  geom_point() + 
  geom_abline() + 
  xlab('Experimental Rates') + ylab('GCAM Derived Rates')


#Plot overlapping rate histograms for the different rate sources
ggplot() +
  geom_histogram(aes(x = Rate_Comparison$Rate, fill ='Experimental Rate' ), alpha = 0.5) +
  geom_histogram(aes(x = Rate_Comparison$GCAM_Rate, fill = 'GCAM Rate'), alpha = 0.5) +
  xlab('Rate') + ylab('Count') +
  scale_fill_manual(values = c('Experimental Rate' = '#45912c', 'GCAM Rate'='#e3962b'))


#Plot overlapping rate histograms for the different rate sources
ggplot() +
  geom_histogram(aes(x = Rate_Comparison$Experimental_k,fill ='Experimental k'), alpha = 0.5) +
  geom_histogram(aes(x = Rate_Comparison$GCAM_k,  fill = 'GCAM k'), alpha = 0.5) +
  xlab('k') + ylab('Count') +
  scale_fill_manual(values = c('Experimental k' = '#45912c', 'GCAM k'='#e3962b'))






