# Format the Sulman (2018) data subset and save as package data
# BBL 2025

library(dplyr)
library(tidyr)

# Utility function
read_model_files <- function(model_dir) {
    files <- list.files(file.path(DIR, model_dir),
                        pattern = "csv$", full.names = TRUE)
    message("Reading ", length(files), " files for ", model_dir)
    x <- lapply(files, readr::read_csv)
    names(x) <- basename(files)
    bind_rows(x, .id = "file")
}

# Load CORPSE data and process into a common format
load_CORPSE <- function(DIR) {
    x <- read_model_files("CORPSE")

    # Clean up columns
    x$file <- gsub(".csv", "", x$file, fixed = TRUE)
    x <- separate(x, file, into = c("model", "output", "clay", "experiment"), sep = "-")
    x <- separate(x, clay, into = c("clay", "litter"))

    clay_map <- c("lowclay" = "low", "highclay" = "medium", "higherclay" = "high")
    x$clay <- clay_map[x$clay]

    # The following code tracks Ben Sulman's Python code
    # https://github.com/bsulman/INTERFACE-model-experiment-synthesis/blob/master/Analysis-code/plot_figures.py
    x$total_protectedC <- x$protectedC
    x$total_unprotectedC <- with(x, unprotectedC_fast + unprotectedC_slow + unprotectedC_deadmic)
    x$total_C <- with(x, total_protectedC + total_unprotectedC + microbeC)
    x$total_microbeC <- x$microbeC
    x$total_litterC <- 0.0

    # Resample to monthly resolution
    x$month <- ceiling(x$Day / 30)
    x$year <- ceiling(x$month / 12)

    x_split <- split(x, x$experiment)
    y <- lapply(x_split, function(x) {
        x$CO2flux <- c(diff(x$CO2), NA)
        # Compute mean values by month
        # This is a little different than the Sulman Python code,
        # which just takes the first value from each month
        aggregate(cbind(total_protectedC, total_unprotectedC,
                        total_C, total_microbeC, total_litterC, CO2flux) ~
                      model + clay + litter + experiment + month,
                  data = x,
                  FUN = mean)
    })
    as_tibble(dplyr::bind_rows(y))
}

# Load MIMICS data and process into a common format
load_MIMICS <- function(DIR) {
    x <- read_model_files("MIMICS")

    # Clean up columns
    x$file <- gsub(".csv", "", x$file, fixed = TRUE)
    x <- separate(x, file, into = c("model", "experiment", "clay", "litter"), sep = "_")

    # Rename experiment column entries to match names used by MIMICS
    exp_map <- c("Control" = "control",
                 "0xLitter" = "litter_removal",
                 "2.0xLitter" = "total_addition_100",
                 "1.3xLitter" = "total_addition_30")
    x$experiment <- exp_map[x$experiment]

    clay_map <- c("CLAY5" = "low", "CLAY20" = "medium", "CLAY70" = "high")
    x$clay <- clay_map[x$clay]

    litter_map <- c("LIG24.4" = "highquality", "LIG16.6" = "lowquality")
    x$litter <- litter_map[x$litter]

    # The following code tracks Ben Sulman's Python code
    # https://github.com/bsulman/INTERFACE-model-experiment-synthesis/blob/master/Analysis-code/plot_figures.py
    x$month <- ceiling(x$`...1` / 30)
    x$year <- ceiling(x$month / 12)
    x$CO2flux <- x$CO2 * 1e-6 * 100**2 * 20  # mgC/cm3 -> kgC/m2

    # Based on conversation with Will... (see comments at GitHub link above)
    x$total_protectedC <- x$SOMp * 1e-6 * 100**2 * 20  # mgC/cm3 -> kgC/m2
    x$total_unprotectedC <- (x$SOMa + x$SOMc) * 1e-6 * 100**2 * 20  # mgC/cm3 -> kgC/m2
    x$total_litterC <- (x$LITm + x$LITs) *1e-6 * 100**2 * 20  # mgC/cm3 -> kgC/m2
    x$total_microbeC <- (x$MICr + x$MICk) * 1e-6 * 100**2 * 20  # mgC/cm3 -> kgC/m2
    x$total_C <- x$total_protectedC + x$total_unprotectedC + x$total_microbeC

    x <- x[c("model", "clay", "litter", "experiment", "month", "total_protectedC",
             "total_unprotectedC", "total_C", "total_microbeC", "total_litterC", "CO2flux")]

    # Compute mean values by month
    x_split <- split(x, x$experiment)
    y <- lapply(x_split, function(x) {
        aggregate(cbind(total_protectedC, total_unprotectedC,
                        total_C, total_microbeC, total_litterC, CO2flux) ~
                      model + clay + litter + experiment + month,
                  data = x,
                  FUN = mean)
    })
    as_tibble(dplyr::bind_rows(y))
}

# Load all model data
DIR <- here::here("data-raw/sulman2018-data")
message("DIR is ", DIR)
corpse_data <- load_CORPSE(DIR)
mimics_data <- load_MIMICS(DIR)

# Restructure
sulman2018 <- bind_rows(corpse_data, mimics_data)
sulman2018 <- pivot_longer(sulman2018, total_protectedC:CO2flux)

# Save
usethis::use_data(sulman2018, overwrite = TRUE, internal = FALSE)
