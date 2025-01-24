## Generate the comparison for the old new tests, for minor package development we do not expect
## changes in the MEND output. However there may be code changing developments, in those cases
## this script will need to be re-run to update the comparison data.
devtools::load_all()

# Define the time steps to solve the model at.
t <- seq(0, 1e3, by = 10)

# Solve for MEND
mend_out <- memc_solve(mod = MEND_config, time = t)
comission_out <- memc_solve(mod = COMISSION_config, time = t)
corpse_out <- memc_solve(CORPSE_config, t)

# Write output to the test file.
out <- rbind(mend_out, comission_out, corpse_out)
names(out) <-  c("time", "variable", "old_value", "units", "name")

out_dir <- here::here('tests',  'testthat')
dir.create(out_dir, showWarnings = FALSE)

ofile <- file.path(out_dir, "compdata.csv")
write.csv(out, ofile, row.names = FALSE)
