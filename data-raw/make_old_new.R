

# start by making the old
config <- configure_model(params = MEMC::default_params, state = MEMC::default_initial)

mend_out <- solve_model(mod = config, time = seq(0,36500, by=100))
mend_out <- mend_out$results
names(mend_out) <- c("time", "variable", "old_value", "units", "name")
write.csv(mend_out, "/Users/dorh012/Documents/2022/MEMC/tests/testthat/old_mend.csv", row.names = FALSE)
