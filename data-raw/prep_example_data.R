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
inital_pools <-
  data.table::fread(file.path(DIR, "example-initial-state.csv"))


# Each incubation data set, the time series and the initial pools will
# stored within a list.

memc_incubation_ultisol <- memc_incubation_andisol <-
  memc_incubation_gelisol <- memc_incubation_mollisol <- list()

memc_incubation_ultisol$data <-
  subset(memc_data_all, Soil == "Ultisol")
memc_incubation_ultisol$state <-
  as.numeric(subset(inital_pools, Soil == "Ultisol")[, -c("Soil")])

memc_incubation_andisol$data <-
  subset(memc_data_all, Soil == "Andisol")
memc_incubation_andisol$state <-
  as.numeric(subset(inital_pools, Soil == "Andisol")[, -c("Soil")])


memc_incubation_gelisol$data <-
  subset(memc_data_all, Soil == "Gelisol")
memc_incubation_gelisol$state <-
  as.numeric(subset(inital_pools, Soil == "Gelisol")[, -c("Soil")])

memc_incubation_mollisol$data <-
  subset(memc_data_all, Soil == "Mollisol")
memc_incubation_mollisol$state <-
  as.numeric(subset(inital_pools, Soil == "Mollisol")[, -c("Soil")])

# Make sure all names are formatted 
names(memc_incubation_ultisol$state) <-
  names(memc_incubation_andisol$state) <-
  names(memc_incubation_gelisol$state) <-
  names(memc_incubation_mollisol$state) <- names(memc_initial_state)

# Save the incubation data
usethis::use_data(memc_incubation_ultisol, overwrite = TRUE, internal = FALSE)
usethis::use_data(memc_incubation_andisol, overwrite = TRUE, internal = FALSE)
usethis::use_data(memc_incubation_gelisol, overwrite = TRUE, internal = FALSE)
usethis::use_data(memc_incubation_mollisol, overwrite = TRUE, internal = FALSE)


