library(rgcam)
library(ggplot2)
library(dplyr)

#Connect to the database
conn <- localDBConn('C:/Users/bian240/OneDrive - PNNL/Desktop/GCAM/output', 'database_basexdb')

#Assign the query to a project file 
prj <- addScenario(conn, 'rgcam_test_proj', 
                   queryFile = 'C:/Users/bian240/OneDrive - PNNL/Desktop/GCAM/output/queries/rcam_test_queries.xml', 
                   scenario = 'Reference', clobber = TRUE)

#Pull the land allocation dataframe out of the project
land_allocation <- getQuery(prj, 'aggregated land allocation')

#Plotting the results
land_allocation %>%
  filter(year > 2015) %>%
  ggplot(aes(x = year, y = value, fill = landleaf)) +
  geom_bar(stat='identity') +
  labs(title = "Land leaf in southern Africa", x = "Year", y = expression(km^2%*%10^3)) +
  theme_bw() +
  theme(legend.title = element_blank())

options(repr.plot.width = 10, repr.plot.height = 4, repr.plot.res = 100)
  
  