# Format the example data to be used in the fit data example.

DIR <- here::here("data-raw")

# Load the data example flux data
# These are from Jianqiu Zheng's google sheet, and originally from ORNL LDRD work
# See Wang et al. 2013 10.1890/12-0681.1
obs <- read.csv(file.path(DIR, "example-data-fluxes.csv"), check.names = FALSE, stringsAsFactors = FALSE)
obs$Variable <- NULL
obs <- data.table::as.data.table(obs)

# Format the data into a long data frame
memc_data_all <- data.table::melt(obs, id.vars = "Day",
                                  variable.name = "Soil",
                                  variable.factor = FALSE)
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

# Load the data example pool (state) data
# These are from Jianqiu Zheng's google sheet, and originally from
# Wang et al. 2013 10.1890/12-0681.1
pools <- read.csv(file.path(DIR, "example-data-pools.csv"), check.names = FALSE, stringsAsFactors = FALSE)
rownames(pools) <- pools$Pool
make_state_vector <- function(soil, x) {
    v <- c(POM = x["POC", soil],
           MOM = x["MOC", soil],
           QOM = 0,
           MB = x["MBC", soil],
           DOM = x["DOC", soil],
           EP = 0.0082,
           EM = 0.0082,
           IC = 0)
    v <- c(v, c("Tot" = sum(v)))

    return(v)
}

memc_state_gelisol <- make_state_vector("Gelisol", pools)
usethis::use_data(memc_state_gelisol, overwrite = TRUE)
memc_state_andisol <- make_state_vector("Andisol", pools)
usethis::use_data(memc_state_andisol, overwrite = TRUE)
memc_state_mollisol <- make_state_vector("Mollisol", pools)
usethis::use_data(memc_state_mollisol, overwrite = TRUE)
memc_state_ultisol <- make_state_vector("Ultisol", pools)
usethis::use_data(memc_state_ultisol, overwrite = TRUE)
