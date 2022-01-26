## Generate the comparison for the old new tests, for minor package development we do not expect
## changes in the MEND output. However there may be code changing developments, in those cases
## this script will need to be re-run to update the comparison data.
devtools::load_all()

# Define the time steps to solve the model at.
t <- seq(0, 1e3, by = 10)

# Solve for MEND
mend_out <- solve_model(mod = MEND_model, time = t)
comission_out <- solve_model(mod = COMISSION_model, time = t)

# Write output to the test file.
out <- rbind(mend_out, comission_out)
names(out) <-  c("time", "variable", "old_value", "units", "name")

out_dir <- here::here('tests',  'testthat', 'compdata')
dir.create(out_dir, showWarnings = FALSE)

ofile <- file.path(out_dir, "com-new.csv")
write.csv(out, ofile, row.names = FALSE)
