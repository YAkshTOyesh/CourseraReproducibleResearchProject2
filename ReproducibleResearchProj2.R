library(dplyr)
library(ggplot2)
#Loading and Processing Data
storm_data <- read.csv("./repdata_data_StormData.csv.bz2")

#Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health?
#Calculating Harm index
storm_health_damage_data <- storm_data %>% select(EVTYPE, STATE, FATALITIES:INJURIES)
storm_health_damage_data <- storm_health_damage_data %>% mutate(HARM_COUNT = (FATALITIES * 2) + INJURIES)
storm_health_damage_data_grouped <- storm_health_damage_data %>%group_by(EVTYPE, STATE) %>%
  mutate(HARM_COUNT_TOTAL = sum(HARM_COUNT)) 

#Summarizing
health_damage_quantile_90 <- quantile(storm_health_damage_data_grouped$HARM_COUNT_TOTAL, probs = 0.90)
storm_health_damage_data_filtered <- storm_health_damage_data_grouped %>% filter(HARM_COUNT_TOTAL > quantile_90) %>%
  arrange(desc(HARM_COUNT_TOTAL)) 

#Plotting
qplot(HARM_COUNT_TOTAL, STATE, data = storm_health_damage_data_filtered, color = EVTYPE)

#Saving Plot in PNG File
png(file = "Plot_Most_Hamrful_Events_On_Health_By_State.png")
qplot(HARM_COUNT_TOTAL, STATE, data = storm_health_damage_data_filtered, color = EVTYPE)
dev.off()
#Tornado being generally the most harmful for most states !!!




#Across the United States, which types of events have the greatest economic consequences?

#Calculating Economic index
storm_economic_damage_data <- storm_data %>% select(EVTYPE, STATE, PROPDMG:CROPDMGEXP)
##Converting all exponents to mathematical values
storm_economic_damage_data <- storm_economic_damage_data %>% mutate(PROPDMGEXP = ifelse(PROPDMGEXP == "K", 1000, PROPDMGEXP)) %>%
  mutate(PROPDMGEXP = ifelse(PROPDMGEXP == "M", 1000000, PROPDMGEXP)) %>%
  mutate(PROPDMGEXP = ifelse(PROPDMGEXP == "K", 1000, PROPDMGEXP)) %>%
  mutate(PROPDMGEXP = ifelse(PROPDMGEXP == "B", 1000000000, PROPDMGEXP)) %>%
  mutate(CROPDMGEXP = ifelse(CROPDMGEXP == "M", 1000000, CROPDMGEXP)) %>%
  mutate(CROPDMGEXP = ifelse(CROPDMGEXP == "K", 1000, CROPDMGEXP)) %>%
  mutate(PROPDMGEXP = ifelse(PROPDMGEXP == "", 0, PROPDMGEXP)) %>%
  mutate(CROPDMGEXP = ifelse(CROPDMGEXP == "", 0, CROPDMGEXP)) 
storm_economic_damage_data <- storm_economic_damage_data %>%
  mutate(PROPDMGEXP = as.numeric(PROPDMGEXP)) %>%
  mutate(CROPDMGEXP = as.numeric(CROPDMGEXP))

##Calculating Economic Index
storm_economic_damage_data <- storm_economic_damage_data %>%
  mutate(Total_economic_Value = (PROPDMG * PROPDMGEXP) + (CROPDMG * CROPDMGEXP))
storm_economic_damage_data_grouped <- storm_economic_damage_data %>%group_by(EVTYPE, STATE) %>%
  mutate(Total_Total_economic_Value = sum(Total_economic_Value)) 

#Summarizing
economic_damage_quantile_90 <- quantile(storm_economic_damage_data_grouped$Total_Total_economic_Value, probs = 0.90, na.rm = TRUE)
storm_economic_damage_data_filtered <- storm_economic_damage_data_grouped %>% filter(Total_Total_economic_Value > economic_damage_quantile_90) %>%
  arrange(desc(Total_Total_economic_Value)) 

#Plotting
qplot(Total_Total_economic_Value, STATE, data = storm_economic_damage_data_filtered, color = EVTYPE)

#Saving Plot in PNG File
png(file = "Plot_Most_Hamrful_Events_On_Economy_By_State.png")
qplot(HARM_COUNT_TOTAL, STATE, data = storm_health_damage_data_filtered, color = EVTYPE)
dev.off()
#Varies per State