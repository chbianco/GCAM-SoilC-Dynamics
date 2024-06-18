
#June 2nd 2024
#A short script for getting oriented with GCAM's soil C densities
#initial author: K. A. Morris
#for: C. Bianco

some_gcamSoilC <- read.csv(file = "C:/Users/bian240/OneDrive - PNNL/Desktop/Initial Project Code/L2252.LN5_MgdCarbon_crop.csv")
more_gcamSoilC <- read.csv(file = "C:/Users/bian240/OneDrive - PNNL/Desktop/Initial Project Code/L2252.LN5_MgdCarbon_bio.csv")

#let's look at what we've got
str(some_gcamSoilC)
str(more_gcamSoilC)
#file with crop data has more columns - does that seem right?

#useful checks for orienting yourself to the data
colnames(some_gcamSoilC)
unique(some_gcamSoilC$region)

#ok - we've been told that all cropland within a basin has the same soil C density, is that true?

library(dplyr)
some_gcamSoilC %>%
  #filter to one region to make data more manageable
  filter(region == "USA") -> USA_SoilC

#where in here does the river basin information live?
unique(USA_SoilC$LandNode1)
unique(USA_SoilC$LandNode2)
unique(USA_SoilC$LandNode3) #bingo
unique(USA_SoilC$LandNode4)

library(ggplot2)

USA_SoilC %>%
  group_by(LandNode3) %>%
  ggplot(., aes(LandNode3, soil.carbon.density)) +
  geom_jitter()
#well that looks like crap
#we still have too much information to get a good visual assessment

head(USA_SoilC$LandNode5)
#separating the crop type from the other LandNode info
#will allow us to group our graph by another factor

library(tidyr)
USA_SoilC %>%
  mutate(crop_type = LandNode4) %>%
  separate(crop_type, sep = "_", into = c("crop_type", NA)) %>%
  ggplot(., aes(crop_type, soil.carbon.density, color = crop_type)) +
  geom_point() + facet_wrap(~LandNode3)

#could be more beautiful
#but it's clear that all basins DO have the same soil C content

#what's in this other guy?
colnames(more_gcamSoilC)
unique(more_gcamSoilC$LandNode5)
#more crops! this time biofuel crops
#do the same thing to confirm soil C density is consistent

#Get just the USA regions
USA_SoilC_more <- filter(more_gcamSoilC, region =='USA')

#The biomass LandNode3 already separates into forest or crop--we need to undo that
USA_SoilC_more %>%
  mutate(state = LandNode3) %>%
  separate(state, sep = '_', into = c(NA, 'state')) -> USA_SoilC_more2

USA_SoilC_more2 %>%
  mutate(crop_type = LandNode4) %>%
  separate(crop_type, sep = "_", into = c("crop_type", NA)) %>%
  ggplot(., aes(crop_type, soil.carbon.density, color = crop_type)) +
  geom_point() + facet_wrap(~state)


#then check a second region's river basins for both files

#Let's do India!

#First, with cropland

some_gcamSoilC %>%
  #filter to one region to make data more manageable
  filter(region == "India") -> India_SoilC

#Separating and plotting
India_SoilC %>%
  mutate(crop_type = LandNode4) %>%
  separate(crop_type, sep = "_", into = c("crop_type", NA)) %>%
  ggplot(., aes(crop_type, soil.carbon.density, color = crop_type)) +
  geom_point() + facet_wrap(~LandNode3)

#Now, with biomass
India_SoilC_more <- filter(more_gcamSoilC, region =='India')

India_SoilC_more %>%
  mutate(state = LandNode3) %>%
  separate(state, sep = '_', into = c(NA, 'state')) -> India_SoilC_more2

India_SoilC_more2 %>%
  mutate(crop_type = LandNode4) %>%
  separate(crop_type, sep = "_", into = c("crop_type", NA)) %>%
  ggplot(., aes(crop_type, soil.carbon.density, color = crop_type)) +
  geom_point() + facet_wrap(~state)

#Should I do anything below this?

#none cropland
#these files should contain everything needed but need to be checked
glus <- read.csv(file = "C:/Users/bian240/OneDrive - PNNL/Desktop/Initial Project Code/GLU_codes.csv") #adding here in  case helpful
regions <- read.csv(file = "C:/Users/bian240/OneDrive - PNNL/Desktop/Initial Project Code/GCAM_regions.csv") #adding here in  case helpful
soilC <- read.csv(file = "C:/Users/bian240/OneDrive - PNNL/Desktop/Initial Project Code/GCAM_soilC.csv")

str(soilC) #this may have everything
#except soil time scale