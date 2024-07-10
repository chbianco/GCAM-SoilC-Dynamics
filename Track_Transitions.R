library(dplyr)
library(ggplot2)

#Load the transition data into R
all_transitions <- read.csv(file = 'Data/transitions_combined_GCAM.csv')

#Filter out zeros
all_transitions %>%
  filter(sqkm_change != 0) -> filtered_transitions

#Create a function to map the detailed land uses to their simple counterparts
simple_land <- function(land){
  if(grepl('forest', land)){return('Forest')}
  else if(grepl('grassland', land)){return('Grassland')}
  else if(grepl('rockicedesert', land)){return('RockIceDesert')}
  else if(grepl('shrub', land)){return('Shrubland')}
  else if(grepl('tundra',land)){return('Tundra')}
  else if(grepl('pasture', land)){return('Pasture')}
  else if(grepl('urban', land)){return('Urbanland')}
  else if(grepl('irrigated', land) | grepl('rainfed', land)){return('Cropland')}
  else if(grepl('otherarableland', land) ){return('OtherArableLand')}
  else{return(NA)}
  
}

#Apply the function to the dataset
sapply(filtered_transitions$to, simple_land) -> to_Land_Use
sapply(filtered_transitions$from, simple_land) -> from_Land_Use

#Replace the to and from columns with the simplified version
filtered_transitions %>%
  mutate(to = to_Land_Use) %>%
  mutate(from = from_Land_Use) %>%
  mutate(change = paste(from, to, sep = ' to ')) -> transitions

#Sum total land use transitions and filter for ones that are meaningfully high 
transitions %>%
  group_by(change) %>%
  summarize(sum(sqkm_change)) %>%
  rename(total_skqm_change = `sum(sqkm_change)`) %>%
  arrange(desc(total_skqm_change)) %>%
  filter(total_skqm_change > 500000)-> total_transitions

#Sort and sum land use transition by GCAM32 region
transitions %>%
  group_by(region_id, change) %>%
  summarize(sum(sqkm_change)) %>%
  arrange((region_id)) %>%
  rename(total_skqm_change = `sum(sqkm_change)`) -> total_transitions_gcam32

#Sort and sum land use transition by basin
transitions %>%
  group_by(metric_id, change) %>%
  summarize(sum(sqkm_change)) %>%
  arrange((metric_id)) %>%
  rename(total_skqm_change = `sum(sqkm_change)`) -> total_transitions_glu


#Plotting
#Plot by GCAM32 region
ggplot(data = total_transitions_gcam32, aes(x=total_skqm_change, y=change)) + 
  geom_bar(stat='identity') + 
  facet_wrap(~region_id)
 
#Plot in total
ggplot(data = total_transitions, aes(x = total_skqm_change, y = change)) + 
  geom_bar(stat='identity') + 
  xlab(expression(Total~area~transitioned~(km^2))) + ylab('Transition Type') +
  theme_light() 

ggsave('total_transitions_bar.jpeg', path = 'Graphs')



#Now, we do this for only after to see what is more common in the future
transitions_soon <- filter(transitions, year > 2015) 

transitions_soon %>%  
  group_by(change) %>%
  summarize(sum(sqkm_change)) %>%
  rename(total_skqm_change = `sum(sqkm_change)`) %>%
  arrange(desc(total_skqm_change)) %>%
  filter(total_skqm_change > 10000)-> total_transitions_soon

#Plot in total for only transitions after 2015
ggplot(data = total_transitions_soon, aes(x = total_skqm_change, y = change)) + 
  geom_bar(stat='identity') + 
  xlab(expression(Total~area~transitioned~(km^2))) + ylab('Transition Type') +
  theme_light() 

ggsave('post_2015_transitions_bar.jpeg', path = 'Graphs')




#Now, we do this for only BEFORE  2015 to see what is more common in the past
transitions_soon <- filter(transitions, year < 2015) 

transitions_soon %>%  
  group_by(change) %>%
  summarize(sum(sqkm_change)) %>%
  rename(total_skqm_change = `sum(sqkm_change)`) %>%
  arrange(desc(total_skqm_change)) %>%
  filter(total_skqm_change > 500000)-> total_transitions_soon

#Plot in total for only transitions in the past
ggplot(data = total_transitions_soon, aes(x = total_skqm_change, y = change)) + 
  geom_bar(stat='identity') + 
  xlab(expression(Total~area~transitioned~(km^2))) + ylab('Transition Type') +
  theme_light() 

ggsave('pre_2015_transitions_bar.jpeg', path = 'Graphs')



#Now, we'll look at only the United States
transitions_soon %>%
  filter(region_id == 1) %>%
  group_by(change) %>%
  summarize(sum(sqkm_change)) %>%
  rename(total_skqm_change = `sum(sqkm_change)`) %>%
  arrange(desc(total_skqm_change)) %>%
  ggplot(aes(x=total_skqm_change, y=change)) + 
    geom_bar(stat = 'identity')
    

