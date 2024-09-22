library(dplyr)
library(tidyr)
library(afex)
library(performance)

change_long_data$Type <- as.factor(change_long_data$Type)
change_long_data$change <- as.factor(change_long_data$change)
change_long_data$Basin_long_name <- as.factor(change_long_data$Basin_long_name)

library(ggplot2)

change_long_data %>%
  ungroup() %>%
  filter(source != "Post & Kwon") %>%
#-> wei_gcam selecting only Wei et al data for now
  group_by(Basin_long_name, change, Type) %>%
  summarize(n = length(k)) -> summary

summary %>%
  ungroup() %>%
  filter(n <= 1) %>%
  select(Basin_long_name) -> basins_to_drop

wei_gcam %>%
  filter(! Basin_long_name %in% basins_to_drop$Basin_long_name) -> wei_gcam2

ggplot(wei_gcam2, aes(Type, k, fill = Basin_long_name)) +
         geom_boxplot()

library(car)

model <- lm(k ~ Type * Basin_long_name,
            data = wei_gcam2)
Anova(model, type = "III", white.adjust = TRUE)

BC <- aov(k ~ Basin_long_name * change,
                      data = change_long_data)
TC <- aov(k ~ Type * change,
          data = change_long_data)
BT <- aov(k ~ Basin_long_name * Type,
          data = wei_gcam)

performance::check_homogeneity(TC, method = "auto")

TukeyHSD(test_long_data)

wei_gcam %>%
  ungroup() %>%
  dplyr::select(k, Type, Basin_long_name) %>%
  droplevels(wei_gcam$Basin_long_name)-> friedman_data_alt
  #group_by(Basin_long_name, Type) %>%
  #complete() -> friedman_data

library(rstatix)

res.fried <- friedman.data %>% friedman_test(k ~ Type|Basin_long_name)
  