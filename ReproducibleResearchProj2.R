library(dplyr)
library(ggplot2)
#Loading and Processing Data
storm_data <- read.csv("./repdata_data_StormData.csv.bz2")

#Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health?
#Calculating Harm index
storm_damage_data <- storm_data %>% select(EVTYPE, STATE, FATALITIES:INJURIES)
storm_damage_data <- storm_damage_data %>% mutate(HARM_COUNT = (FATALITIES * 2) + INJURIES)
storm_damage_data_grouped <- storm_damage_data %>%group_by(EVTYPE, STATE) %>%
  mutate(HARM_COUNT_TOTAL = sum(HARM_COUNT)) 
#Summarizing
quantile_90 <- quantile(storm_damage_data_grouped$HARM_COUNT_TOTAL, probs = 0.90)
storm_data_filtered <- storm_damage_data_grouped %>% filter(HARM_COUNT_TOTAL > quantile_90) %>%
  arrange(desc(HARM_COUNT_TOTAL)) 
#Plotting
qplot(HARM_COUNT_TOTAL, STATE, data = storm_data_filtered, color = EVTYPE)
#Tornado being generally the most harmful for most states !!!