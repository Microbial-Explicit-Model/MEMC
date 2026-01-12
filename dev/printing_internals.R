
devtools::load_all()

# Create a vector of the time steps to solve the model run at.
t <- 0:50


file.remove("./dev/flux.csv", 
            "./dev/delta.csv", 
            "./dev/pool.csv")

# Use the MEND_model configuration included in the package 
mend_out1 <- memc_solve(mod = MEND_config, time = t)
mend_out1$name <- "default"

# Load and format the debugging data ----------------------------------------
# Format  all the flux data --------------------------------------

colnames <- c("F1", 
              "F2",
              "F3",
              "F4",
              "F5",
              "F6",
              "F7",
              "F8",
              "F9_ep",
              "F9_em",
              "F10_ep",
              "F10_em")


F_data <- read.csv("dev/flux.csv", header = FALSE)
colnames(F_data) <- colnames

F_data %>% 
  mutate(count = 1:nrow(.)) -> 
  F_data



# Format deltas  -----------------------------------------------------------
colnames <- c("dPOC",
           "dMOC",
           "dQOC", 
           "dMB", 
           "dDOC",
           "dEP", 
           "dEM",
           "dIC",
           "dTot")

delta_data <- read.csv("dev/delta.csv", header = FALSE)
colnames(delta_data) <- colnames

delta_data %>% 
  cbind(count = 1:nrow(.)) -> 
  delta_df


# format the pool sizes -----------------------------------------------------

colnames <- c("POC",
              "MOC",
              "QOC", 
              "MB", 
              "DOC",
              "EP", 
              "EM",
              "IC",
              "Tot")

pool_data <- read.csv("dev/pool.csv", header = FALSE)
colnames(pool_data) <- colnames

pool_data %>% 
  cbind(count = 1:nrow(.)) -> 
  pool_df


# Check the first time step -----------------------------------------------
# Make sure that all the pool values are what we would expect (they should 
# be equal to to the initial pool sizes)
pool_df %>% 
  filter(count == 1) == memc_initial_state
  

# EM Pool -----------------------------------------------------------------------
# Take a look at what is going on in the EM pool (it is one of the simplest pools)

# Start by using the flux data frame to approimate the delta, it should be 
# nbe equal to the delta data frame returned. 
F_data %>% 
  mutate(into = F9_em,
         out = F10_em) %>% 
  select(into, out, count) %>% 
  mutate(my_delta = into - out) -> 
  flux_df 

delta_df %>% 
  select(dEM, count) %>% 
  left_join(flux_df) -> 
  delta_inout

# This value should be ~0 
stopifnot(abs(mean(delta_inout$my_delta - delta_inout$dEM)) < 1e-8)


# How does the change in the pool size compare with the dEM? 
# It is bigger than I was expecting honestly... 
pool_df %>% 
  select(EM, count) %>% 
  mutate(pool_diff = c(diff(EM), NA)) %>% 
  left_join(delta_df %>% 
              select(dEM, count)) %>% 
  na.omit() %>% 
  mutate(err = dEM - pool_diff) %>% 
  pull(err) %>%
  mean()

# What is going on with DOC --------------------------------------

pool_df %>%  
  ggplot(aes(count, DOC)) + 
  geom_line() + 
  labs(title = "DOC pool size - from internal solver") + 
  labs(x = "solver itteration")

  # Calculate all the flux into DOC 
  F_data %>% 
    select(F2,  F8, F3, F7, F10_em, F10_ep) %>%  
    mutate(F8 = F8 * 0.5, F2 = F2 * 0.5) %>% 
    mutate(F_in = F2 + F8 + F3 + F7 + F10_em + F10_ep) -> 
    influx
  
  # Calculate the flux out of DOC 
  F_data %>% 
    select(F1, F6) %>% 
    mutate(F_out = F1 + F6)-> 
    outflux
  
  
  data.frame(F_in = influx$F_in, 
             F_out = outflux$F_out) %>% 
    cbind(count = 1:nrow(.)) -> 
    doc_flux
  
head(doc_flux)
  
ggplot(data = doc_flux) + 
  geom_line(aes(count, F_in, color = "into DOC")) + 
  geom_line(aes(count, F_out, color = "out of DOC")) + 
  labs(x = "itteration") 


ggplot(data = pool_df)  + 
  geom_line(aes(count, DOC))



# Calculate the flux out of DOC 
doc_flux %>% 
  left_join(pool_df %>% select(count, DOC)) %>% 
  select(count, DOC, F_in, F_out) %>% head()

  
  
  
