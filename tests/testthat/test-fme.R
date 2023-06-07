ptable <- MEMC::default_params
state  <- MEMC::default_initial
mod <- configure_model(params = ptable, state = state)
zero <- 1e-6

# Set up and run a basic model and use the output as comparison data.
time <- floor(seq(from = 0, to = 100, length.out = 6))
out <- solve_model(mod, time)

comp_data <- data.frame(time = unique(out$time),
                        IC = out[out$variable == "IC",][["value"]])

test_that("make_memc_objective", {

  # First run the objective function using model data as the comparison data.
  # We would expect the cost of model input to output to be 0.
  objective <- make_memc_objective(comp_data = comp_data,
                                   x = MEMC::default_initial,
                                   config = mod)
  cost1 <- objective(x = MEMC::default_initial)
  expect_lte(cost1$model, zero)

  # When there is a change in parameter value the most cost should be greater than 0.
  cost2 <- objective(x = c("V_d" = 50))
  expect_gt(cost2$model, zero)

  # When the comparison data and model data are different from one another , as in most instances, the
  # model cost should be non 0.
  comp_data$IC <- comp_data$IC * 2
  objective2 <- make_memc_objective(comp_data, x = MEMC::default_initial, mod)
  cost3 <- objective2(x = MEMC::default_initial)
  expect_gt(abs(cost3$model), zero)

})


test_that("memc_modfit", {
  # Try out the memc_modfit routine using comparison data from a default run.
  # The function should allow us to solve for an unknown model parameter.


  # Solving for V_d
  default_vd <- ptable[ptable$parameter == "V_d", "value"]
  out <- memc_modfit(config = mod,
                     x = c("V_d" = 10),
                     comp_data = comp_data,
                     lower = c(0))
  expect_lt(abs(out$par - default_vd), 1e-4)
  expect_gt(out$iterations, 1)


  # Test to see if it works for multiple model parameters
  default_B <- default_initial[["B"]]
  out <- memc_modfit(config = mod,
                     x = c("V_d" = 10, "B" = 5),
                     comp_data = comp_data,
                     lower = c(0))
  expect_lt(mean(abs(out$par - c(default_vd, default_B))), 1e-4)

})

test_that("memc_sensrange", {

  # Set up the parameter values to test and use in the sensrange.
  frac <- 0.5
  n <- 3
  p <- c("V_p" = 1.4e+01)
  prange <- data.frame(min = p - p * frac, max = p + p * frac)

  # Run the sense range and check the output.
  out <- memc_sensrange(MEND_model, t = time, pars = p, n = n, dist = "latin", parRange = prange)
  expect_true(all(dim(out) == c(n, (length(time) * 9) + 1)))

  out2 <- memc_sensrange(MEND_model, t = time, pars = p, num = 6, dist = "latin", parRange = prange)
  expect_true(nrow(out2) > nrow(out))
})
