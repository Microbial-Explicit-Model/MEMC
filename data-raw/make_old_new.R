# Set location where to write the comparison data out to.
BASE_DIR <- here::here()
TEST_DIR <- file.path(BASE_DIR, "tests", "testthat")

time <- floor(seq(0, 36500, length.out = 25))

# Run the different outputs.
mend_out <- solve_model(mod = MEND_model, time)
names(mend_out) <-
  c("time", "variable", "old_value", "units", "name")

comisssion_out <- solve_model(mod = COMISSION_model, time)
names(comisssion_out) <-
  c("time", "variable", "old_value", "units", "name")

corpse_out <- solve_model(CORPSE_model, time)
names(corpse_out) <-
  c("time", "variable", "old_value", "units", "name")

out <- rbind(mend_out, comisssion_out, corpse_out)

write.csv(out,
          file = file.path(TEST_DIR, "old-new.csv"),
          row.names = FALSE)
