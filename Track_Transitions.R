library(dplyr)

#Load the transition data into R
all_transitions <- read.csv(file = 'Data/transitions_combined_GCAM.csv')

all_transitions %>%
  filter(sqkm_change != 0) -> filtered_transitions



simple_land <- function(land){
  if(grepl('forest', land)){return('Forest')}
  else if(grepl('grassland', land)){return('Grassland')}
  else if(grepl('rockicedesert', land)){return('RockIceDesert')}
  else if(grepl('shrub', land)){return('Shrubland')}
  else if(grepl('tundra',land)){return('Tundra')}
  else if(grepl('pasture', land)){return('Pasture')}
  else if(grepl('urban', land)){return('Urbanland')}
  else if(grepl('irrigated', land) | grepl('rainfed', land)){return('Cropland')}
  else if(grepl('otherarableland', land) ){return('Cropland')}
  else{return(NA)}
  
}

sapply(filtered_transitions$to, simple_land) -> to_Land_Use
sapply(filtered_transitions$from, simple_land) -> from_Land_Use

filtered_transitions %>%
  mutate(to = to_Land_Use) %>%
  mutate(from = from_Land_Use) %>%
  mutate(change = paste(to, from, sep = '')) -> transitions
  
transitions %>%
  group_by(change) %>%
  summarize(sum(sqkm_change)) %>%
  arrange(desc(`sum(sqkm_change)`)) %>%
  rename(total_skqm_change = `sum(sqkm_change)`) -> total_transitions

#Plotting
ggplot(data = total_transitions, aes(x=change, y=total_skqm_change)) + 
  geom_bar(stat='identity') 

