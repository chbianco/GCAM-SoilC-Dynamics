
library(tidyr)

Full_Comparison %>%
  pivot_longer(cols = Exp_k:GCAM_k,
               names_to = "Type",
               values_to = "k") -> long_data

test_long_data <- aov(k ~ Type + Basin_long_name,
                      data = long_data)
summary(test_long_data)
TukeyHSD(test_long_data)
  