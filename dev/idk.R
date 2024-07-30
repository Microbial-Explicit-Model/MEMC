library(ggplot2)
library(dplyr)
devtools::load_all()
#devtools::load_all("~/Documents/2023/Soil Modeling/MEMC")
MEND_model
# This data is in days! but the model runs in hours! okay so will want to convert! 
read.csv(file.path("/Users/dorh012/Documents/2023/Soil Modeling/exploring/data", "Gelisol_control.csv")) %>% 
  mutate(type = "Gelisol")-> 
  raw_data

raw_data %>% 
  group_by(time, type) %>% 
  summarise(IC = mean(IC)/25) %>%  
  # Change the data from days to hours! 
  mutate(time = time * 24) %>%  
  select(-type) -> 
  data_hrs


# TODO add an update inital stating conditions function! 
update_state(c("POM" = 4.25), MEND_model$state)

Gelisol_MEND <- MEND_model
Gelisol_MEND$state <- c("POM" = 4.25, "MOM" = 11.04, "QOM"=  0.1104, "MB"=0.05, "DOM" = 0.17, "EP" = 0.00001, "EM" = 0.00001, "IC" = 0, "Tot" = 15.61002) 

test <- solve_model(Gelisol_MEND, time = 1:8760)




x = c("K_d" = 0.15, "V_d" = 0.045, "V_p" = 20, "V_m" = 1)

fit1 <- memc_modfit(config = Gelisol_MEND, comp_data = data_hrs, 
                    x = c("V_p" = 3, "K_p" = 50, "V_m" = 1), 
                    lower = c("V_p" = 0.1, "K_p" = 1, "V_m" = 0.05), 
                    upper = c("V_p" = 50, "K_p" = 100, "V_m" = 2))
fit1$par
test1 <- solve_model(Gelisol_MEND, time = 0:8760, params = fit1$par)



ggplot()+ 
  geom_line(data = data_hrs, aes(time, IC)) + 
  geom_line(data = test %>% filter(variable == "IC"), aes(time, value)) +
  geom_line(data = test1 %>% filter(variable == "IC"), aes(time, value), color = "red")


test1 %>%  
  ggplot(aes(time, value)) +
  geom_line() + 
  facet_wrap("variable", scales = "free")


