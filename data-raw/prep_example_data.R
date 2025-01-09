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
  variable.factor = FALSE
)
memc_data_all <- as.data.frame(memc_data_all)

# Save individual soil type datasets
memc_ultisol_data <- subset(memc_data_all, Soil == "Ultisol")

# The initial values for ultisol that we were given.
memc_ultisol_state <- c(
  POM = 4.71,
  MOM = 17.67,
  QOM = 0,
  MB = 0.82,
  DOM = 0.148,
  EP = 0.0082,
  EM = 0.0082,
  IC = 0,
  Tot = 23.484
)


memc_incubation_ultisol <- list()
memc_incubation_ultisol[["state"]] <- memc_ultisol_state
memc_incubation_ultisol[["data"]] <- memc_ultisol_data

usethis::use_data(memc_incubation_ultisol, overwrite = TRUE)