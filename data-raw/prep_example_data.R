# Format the example data to be used in the fit data example.

DIR <- here::here("data-raw")

# Load the data example flux data
# These are from Jianqiu Zheng's google sheet, and originally from ORNL LDRD work
# See Wang et al. 2013 10.1890/12-0681.1
obs <- data.table::fread(file.path(DIR, "example-data-fluxes.csv"))
obs$Variable <- NULL

# Format the data into a long data frame
memc_data_all <- data.table::melt(obs, id.vars = "Day",
                                  variable.name = "Soil",
                                  variable.factor = FALSE)
memc_data_all <- as.data.frame(memc_data_all)

# Save individual soil type datasets
memc_data_ultisol <- subset(memc_data_all, Soil == "Ultisol")
usethis::use_data(memc_data_ultisol, overwrite = TRUE)


# The initial values for ultisol that we were given.
Ultisol_state <-
  c(
    P = 4.71,
    M = 17.67,
    Q = 0,
    D = 0.148,
    B = 0.82,
    EP = 0.0082,
    EM = 0.0082,
    IC = 0,
    Tot = 23.484
  )
state <- data.frame(state = names(Ultisol_state),
                    value = Ultisol_state)
write.csv(state,
          here::here("inst", "example", "exmaple_initial.csv"),
          row.names = FALSE)
