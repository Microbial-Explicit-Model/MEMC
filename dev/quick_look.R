# Let's take a quick look at the ratio of the initial carbon pools, specifically 
# let's consider the DOC, POC, and MB since those are the the pools that are 
# most realted to our little problem.... 

# let's load our pacakge! 
devtools::load_all()



# Let's see what happens when we use different initial starting conditions.... 
# Create a vector of the time steps to solve the model run at.
t <- 0:600 


memc_initial_state

memc_incubation_andisol$state
memc_incubation_gelisol$state
memc_incubation_mollisol$state
memc_incubation_mollisol$state



source("scripts/env.R")

# The remainder of the script are  included in the documentation manuscript. 
# Section 4.1 --------------------------------------------------------------


# Create a vector of the time steps to solve the model run at.
t <- 0:50

# Use the MEND_model configuration included in the package 
mend_out1 <- memc_solve(mod = MEND_config, time = t)
mend_out1$name <- "default"

mend_out2 <- memc_solve(mod = MEND_config, time = t, state = memc_incubation_andisol$state)
mend_out2$name <- "andisol"

memc_incubation_mollisol

mend_out3 <- memc_solve(mod = MEND_config, time = t, state = memc_incubation_gelisol$state)
mend_out3$name <- "gelisol"

mend_out4 <- memc_solve(mod = MEND_config, time = t, state = memc_incubation_mollisol$state)
mend_out4$name <- "mollisol"


mend_out5 <- memc_solve(mod = MEND_config, time = t, state = memc_incubation_ultisol$state)
mend_out5$name <- "ultisol"

rbind(mend_out1, 
      mend_out2, 
      mend_out3, 
      mend_out4, 
      mend_out5) %>% 
  dplyr::filter(time <= 100) %>% 
  plot()



# Use the MEND_model configuration included in the package 
mend_out <- memc_solve(mod = MEND_config, time = t, state = memc_incubation_andisol$state)

# Preview the run results
head(mend_out)

# Plot the results 
plot(mend_out)

# Save figure  for the manuscript 
ggsave(filename = file.path("figures", "fig_3.png"), width = WIDTH, height = HEIGHT)

# Section 4.2 --------------------------------------------------------------

# Use the memc_configure function to build the model, using this function 
# will print a table describing the model configuration. 
my_model <- memc_configure(params = memc_params, 
                           state = memc_initial_state, 
                           name = "my model", 
                           F1 = "MM", 
                           F2 = "ECA", 
                           F8 = "LM")

# Take a look at the dynamics defined using the summary function 
summary(my_model)

# Use the MEND_model configuration included in the package 
my_out <- memc_solve(mod = my_model, time = t)

# Preview the run results
head(my_out)

# Compare the results from the first simulation with the results from 
# the toy model. 
to_plot <- rbind(mend_out, my_out)
plot(to_plot)

# Save figure  for the manuscript 
ggsave(filename = file.path("figures", "fig_4.png"), width = WIDTH, height = HEIGHT)






