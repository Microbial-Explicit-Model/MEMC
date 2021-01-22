## Generate the comparison for the old new tests, for minor package devlopment we do not expect
## changes in the MEND output. However there may be code changing devlopments, in those cases
## this script will need to be re-run so that generate comparison data.
devtools::load_all()

# Define the input data
state <- c(P = 10,  M = 5,  Q = 0.1,  B = 2,  D = 1,  EP = 0.00001,  EM = 0.00001,  IC = 0,  Tot = 18.10002)

# Solve MEND
MEND2013_default <- solver(params = MEND2013_params,
                            time = seq(0, 1e3, by = 0.1),
                            state = state,
                            carbon_pools_func = MEND2013_pools,
                            carbon_fluxes_func = MEND2013_fluxes)

out_dir <- here::here('tests',  'testthat', 'compdata')
dir.create(out_dir, showWarnings = FALSE)
write.csv(MEND2013_default, file = file.path(out_dir, 'old_new-MEND2013.csv'), row.names = FALSE)
