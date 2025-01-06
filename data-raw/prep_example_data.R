# Format the example data to be used in the fit data example.

DIR <- here::here("data-raw")

# Load the data example data
# These are from Jianqiu Zheng's google sheet, and originally from ORNL LDRD work
# See Wang et al. 2013 10.1890/12-0681.1
obs <- read.csv(file.path(DIR, "example-data-raw.csv"), check.names = FALSE, stringsAsFactors = FALSE)
obs <- data.table::as.data.table(obs)

# Format the data into a long data frame
memc_data_all <- data.table::melt(obs, id.vars = c("Day", "Variable"),
                                  variable.name = "Soil", variable.factor = FALSE)
memc_data_all <- as.data.frame(memc_data_all)

# Save individual soil type datasets
memc_data_andisol <- subset(memc_data_all, Soil == "Andisol")
usethis::use_data(memc_data_andisol, overwrite = TRUE)
memc_data_gelisol <- subset(memc_data_all, Soil == "Gelisol")
usethis::use_data(memc_data_gelisol, overwrite = TRUE)
memc_data_mollisol <- subset(memc_data_all, Soil == "Mollisol")
usethis::use_data(memc_data_mollisol, overwrite = TRUE)
memc_data_oxisol <- subset(memc_data_all, Soil == "Oxisol")
usethis::use_data(memc_data_oxisol, overwrite = TRUE)
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
