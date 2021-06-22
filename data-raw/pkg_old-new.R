## Generate the comparison for the old new tests, for minor package devlopment we do not expect
## changes in the MEND output. However there may be code changing devlopments, in those cases
## this script will need to be re-run to update the comparison data.
devtools::load_all()

# Define the time steps to solve the model at.
t <- seq(0, 1e3, by = 10)

# Single pool MEND models -------------------------------------------------------------------------------
# Solve single pool MEND 2013
MEND_out <- solver(params = MEND_params, time = t, state = MEND_initalState, carbon_pools_func = MEND_pools, carbon_fluxes_func = MEND_fluxes)
MEND_out$name <- "MEND"

MEND_RM_out <- MEND_RM(parameters = MEND_params, time = t, inital_state = MEND_initalState)
MEND_RM_out$name <- "MEND_RM"

MEND_ECA_out <- MEND_ECA(parameters = MEND_params, time = t, inital_state = MEND_initalState)
MEND_ECA_out$name <- "MEND_ECA"

MEND_MILLEN_out <- MEND_MILLEN(parameters = MEND_params, time = t, inital_state = MEND_initalState)
MEND_MILLEN_out$name <- "MEND_MILLEN"

# Doubble pool MEND models -------------------------------------------------------------------------------
MEND2_out <- MEND2(parameters = MEND2_params, time = t, inital_state = MEND2_initalState)
MEND2_out$name <- "MEND2"

MEND2_RM_out <- MEND2_RM(parameters = MEND2_params, time = t, inital_state = MEND2_initalState)
MEND2_RM_out$name <- "MEND2_RM"

MEND2_ECA_out <- MEND2_ECA(parameters = MEND2_params, time = t, inital_state = MEND2_initalState)
MEND2_ECA_out$name <- "MEND2_ECA"

MEND2_MILLEN_out <- MEND2_MILLEN(parameters = MEND2_params, time = t, inital_state = MEND2_initalState)
MEND2_MILLEN_out$name <- "MEND2_MILLEN"



# Save output -------------------------------------------------------------------------------
# Combine into a single data frame.
out <- rbind(MEND_out, MEND_RM_out, MEND2_out, MEND2_RM_out, MEND2_ECA_out, MEND2_MILLEN_out,
             MEND_ECA_out, MEND_MILLEN_out)

# Write output to the test file.
out_dir <- here::here('tests',  'testthat', 'compdata')
dir.create(out_dir, showWarnings = FALSE)
ofile <- file.path(out_dir, "comp.csv")
write.csv(out, ofile, row.names = FALSE)


