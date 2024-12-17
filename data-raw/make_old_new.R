# Set location where to write the comparison data out to.
BASE_DIR <- here::here()
TEST_DIR <- file.path(BASE_DIR, "tests", "testthat")

time <- floor(seq(0, 36500, length.out = 25))

# Run the different outputs.
mend_out <- memc_solve(mod = MEND_model, time)
names(mend_out) <-
  c("time", "variable", "old_value", "units", "name")

comisssion_out <- memc_solve(mod = COMISSION_model, time)
names(comisssion_out) <-
  c("time", "variable", "old_value", "units", "name")

corpse_out <- memc_solve(CORPSE_model, time)
names(corpse_out) <-
  c("time", "variable", "old_value", "units", "name")

corpse_out <- memc_solve(CORPSE_model, time)
names(corpse_out) <-
  c("time", "variable", "old_value", "units", "name")

mimcs_out <- memc_solve(MIMCS_model, time)
names(mimcs_out) <-
  c("time", "variable", "old_value", "units", "name")

mems_out <- memc_solve(MEMS_model, time)
names(mems_out) <-
  c("time", "variable", "old_value", "units", "name")

bams_out <- memc_solve(BAMS_model, time)
names(bams_out) <-
  c("time", "variable", "old_value", "units", "name")

out <- rbind(mend_out, comisssion_out, corpse_out, mimcs_out, mems_out, bams_out)

write.csv(out,
          file = file.path(TEST_DIR, "old-new.csv"),
          row.names = FALSE)
