ptable <- MEMC::default_params
state  <- MEMC::default_initial

# Set up and run a basic model and use the output as comparison data.
time <- floor(seq(from = 0, to = 100, length.out = 6))
mod <- configure_model(params = ptable, state = state)
out <- solve_model(mod, time)[["results"]]

comp_data <- data.frame(time = unique(out$time),
                        IC = out[out$variable == "IC", ][["value"]])

test_that("objective fxn", {

  # First run the objective function using model data as the comparison data.
  # We would expect the cost of model input to output to be 0.
  objective <- make_memc_objective(comp_data = comp_data, x = MEMC::default_initial, config = mod)
  cost1 <- objective(x = MEMC::default_initial)
  expect_lte(cost1$model, 1e-8)

  # When change a parameter value the model output and cost should also change.
  cost2 <- objective(x = c("V.d" = 50))
  expect_gt(abs(mean(cost1$residuals$mod - cost2$residuals$mod)), 0)
  expect_gt(cost2$model, cost1$model)

  # When the comparison data is perturbed the cost should be larger than 0
  comp_data$IC <- comp_data$IC * 2
  objective <- make_memc_objective(comp_data, x = MEMC::default_initial, mod)
  cost <- objective(x = MEMC::default_initial)
  expect_gt(cost$model, 0)

})

test_that("memc_modfit", {

  # Save a copy of the default Vd, that is the value we expect to be found via the mod fit.
  # Since the comparison data is output from the default result.
  default_vd <- ptable[ptable$parameter == "V.d", "value"]
  out <-  memc_modfit(config = mod, x = c("V.d" = 10), comp_data = comp_data, lower = c(0))
  expect_lt(abs(out$par - default_vd), 1e-4)
  expect_gt( out$iterations, 1)


  # Make sure that the fit works with multiple parameters.
  default_B <- default_initial[["B"]]
  out <-  memc_modfit(config = mod, x = c("V.d" = 10, "B" = 5), comp_data = comp_data, lower = c(0))
  expect_lt(mean(abs(out$par - c(default_vd, default_B))), 1e-4)

})
