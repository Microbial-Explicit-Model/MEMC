

# start by making the old
config <- list("name" = "test",
               "table" = NULL,
               "params" = MEMC::default_params,
               "state" = MEMC::default_initial,
               "derivs" = MEMC::derivs)

mend_out <- solve_model(mod = config, time = seq(0,36500, by=1))
names(mend_out) <- c("time", "variable", "old_value", "units", "name")
write.csv(mend_out, "/Users/dorh012/Documents/2022/MEMC/tests/testthat/old_mend.csv", row.names = FALSE)
