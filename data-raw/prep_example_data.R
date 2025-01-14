# Format the example data to be used in the fit data example.

DIR <- here::here("data-raw")

# Load the data example flux data
# These are from Jianqiu Zheng's google sheet, and originally from ORNL LDRD work
# See Wang et al. 2013 10.1890/12-0681.1
obs <- data.table::fread(file.path(DIR, "example-data-fluxes.csv"))
obs$Variable <- NULL

# Format the data into a long data frame
memc_data_all <- data.table::melt(
  obs,
  id.vars = "Day",
  variable.name = "Soil",
  variable.factor = FALSE, 
  value.name = "IC"
)
memc_data_all <- as.data.frame(memc_data_all)

# Load the example data for the initial pool sizes. 
# These are from Jianqiu Zheng's google sheet, and originally from ORNL LDRD work
# See Wang et al. 2013 10.1890/12-0681.1
inital_pools <- data.table::fread(file.path(DIR, "example-initial-state.csv"))


# Create the object to save incubation information in 
memc_incubation <- list()

memc_incubation$ultisol$data <- subset(memc_data_all, Soil == "Ultisol")
memc_incubation$ultisol$state <- as.numeric(subset(inital_pools, Soil == "Ultisol")[ ,-c("Soil")])

memc_incubation$andisol$data <- subset(memc_data_all, Soil == "Andisol")
memc_incubation$andisol$state <- as.numeric(subset(inital_pools, Soil == "Andisol")[ ,-c("Soil")])

memc_incubation$gelisol$data <- subset(memc_data_all, Soil == "Gelisol")
memc_incubation$gelisol$state <- as.numeric(subset(inital_pools, Soil == "Gelisol")[ ,-c("Soil")])

memc_incubation$mollisol$data <- subset(memc_data_all, Soil == "Mollisol")
memc_incubation$mollisol$state <- as.numeric(subset(inital_pools, Soil == "Mollisol")[ ,-c("Soil")])


names(memc_incubation$ultisol$state) <- names(memc_incubation$andisol$state) <- 
  names(memc_incubation$gelisol$state) <- names(memc_incubation$mollisol$state) <- names(memc_initial_state)
  
  
  
usethis::use_data(memc_incubation, overwrite = TRUE)
