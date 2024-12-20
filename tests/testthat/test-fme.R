ptable <- MEMC::default_params
state  <- MEMC::default_initial
mod <- memc_configure(params = ptable, state = state)
zero <- 1e-6

# Set up and run a basic model and use the output as comparison data.
time <- floor(seq(
  from = 0,
  to = 100,
  length.out = 6
))
out <- memc_solve(mod, time)

comp_data <- data.frame(time = unique(out$time),
                        IC = out[out$variable == "IC",][["value"]])

test_that("make_memc_objective", {
  # First run the objective function using model data as the comparison data.
  # We would expect the cost of model input to output to be 0.
  objective <- make_memc_objective(
    comp_data = comp_data,
    x = MEMC::default_initial,
    config = mod
  )
  cost1 <- objective(x = MEMC::default_initial)
  expect_lte(cost1$model, zero)
  
  # When there is a change in parameter value the most cost should be greater than 0.
  cost2 <- objective(x = c("V_d" = 50))
  expect_gt(cost2$model, zero)
  
  # When the comparison data and model data are different from one another , as in most instances, the
  # model cost should be non 0.
  comp_data$IC <- comp_data$IC * 2
  objective2 <-
    make_memc_objective(comp_data, x = MEMC::default_initial, mod)
  cost3 <- objective2(x = MEMC::default_initial)
  expect_gt(abs(cost3$model), zero)
  
})

test_that("memc_modfit", {
  # Try out the memc_modfit routine using comparison data from a default run.
  # The function should allow us to solve for an unknown model parameter.
  
  # Solving for V_d
  default_vd <- ptable[ptable$parameter == "V_d", "value"]
  out <- memc_modfit(
    config = mod,
    x = c("V_d" = 10),
    comp_data = comp_data,
    lower = c(0)
  )
  expect_lt(abs(out$par - default_vd), 1e-4)
  expect_gt(out$iterations, 1)
  
  # Test to see if fitting works fo an initial carbon pool value.
  default_B <- default_initial[["MB"]]
  out <- memc_modfit(
    config = mod,
    x = c("MB" = 5),
    comp_data = comp_data,
    lower = c(0)
  )
  expect_lt(mean(abs(out$par - c(default_B))), 1e-4)
  
})

test_that("memc_sensrange", {
  # Set up the parameter values to test and use in the sensrange.
  frac <- 0.5
  n <- 3
  p <- c("V_p" = 1.4e+01)
  prange <- data.frame(min = p - p * frac, max = p + p * frac)
  
  # Run the sense range and check the output.
  out <-
    memc_sensrange(
      MEND_model,
      t = time,
      x = p,
      n = n,
      dist = "latin",
      parRange = prange
    )
  expect_true(all(dim(out) == c(n, (length(
    time
  ) * 9) + 1)))
  
  # Check to make sure that object returned by sense range can be plotted
  # using the helper function
  x <- memc_plot(obj = out)
  expect_equal(class(x), c("gg", "ggplot"))
  
  
  out2 <-
    memc_sensrange(
      MEND_model,
      t = time,
      x = p,
      num = 6,
      dist = "latin",
      parRange = prange
    )
  expect_true(nrow(out2) > nrow(out))
})

test_that("memc_sensfunc", {
  # Test that the function runs and returns the expected output that
  # can be plotted.
  pars <- c("V_d" = 3.0e+00,
            "V_p" = 1.4e+01,
            "V_m" = 2.5e-01)
  out <- memc_sensfunc(config = MEND_model, t = time, x = pars)
  
  expect_equal(class(out), c("sensFun", "data.frame"))
  
  x <- memc_plot(out)
  expect_equal(class(x), c("gg", "ggplot"))
  
  
  # Check to see that the function also works with initial states
  inital_pools <- c("MOM" = 5, "DOM" = 1)
  out2 <-
    memc_sensfunc(config = MEND_model, t = time, x = inital_pools)
  expect_equal(class(out2), c("sensFun", "data.frame"))
  
  # The results from out and out2 should be different since by test
  # design the returned data frames will be different sized.
  expect_error(out - out2,
               regexp = "'-' only defined for equally-sized data frame")
  
  
})