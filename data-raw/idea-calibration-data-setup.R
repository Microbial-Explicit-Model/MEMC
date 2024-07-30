
library(dplyr)
library(ggplot2)
library(tidyr)


# Read in the Oak Ridge SFA data set 
"~/Desktop/Soil_Respiration_Microbial_Biomass_From_Soil_Incubations_20170519 (1).csv" %>% 
  read.csv(skip = 7) -> 
  data 

# Since the 1st of "data" in the table are the unit information save a copy of 
# these values and eventually drop this row. 
units <- data[1,]
data[1, ] <- NA
data <- data[!is.na(data$Sample_ID), ] 


# Subset the data so that it only includes the results from the control experiment. 
control_data <- data[data$Amendment == "Control", ]



control_data %>% 
 # filter(Experiment == "Short") %>% 
  mutate(Resp_rate = as.numeric(Resp_rate)) %>% 
  mutate(category = paste0(State, "_", Veg_Type, "_", Rep)) %>% 
  ggplot(aes(TimePoint, Resp_rate, group = category)) + 
  geom_line() + 
  facet_grid(Veg_Type~State)


control_data %>% 
  filter(Experiment == "Short") %>% 
  mutate(MBC = as.numeric(MBC)) %>% 
  mutate(category = paste0(State, "_", Veg_Type, "_", Rep)) %>% 
  ggplot(aes(TimePoint, MBC, group = category)) + 
  geom_line() + 
  facet_grid(Veg_Type~State)



control_data %>% 
  mutate(TimePoint = as.integer(TimePoint)) %>% 
  filter(State == "TN", Veg_Type == "Grass", Experiment == "Short") %>% 
  ggplot(aes(TimePoint, Resp_rate, group = Rep)) + 
  geom_line()

