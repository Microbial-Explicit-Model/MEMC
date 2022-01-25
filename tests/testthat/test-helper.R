
params <- MEMC::default_params
init   <- MEMC::default_initial
env <- internal_load_params(params, init)

test_that("configure_model", {

  # check to make sure that the object returned is a list with the correct number of elements.
  out1 <- configure_model(params = params, state = init, carbon_pools_func = carbon_pools,
                          carbon_fluxes_func = carbon_fluxes)
  expect_true(is.list(out1))
  expect_true(length(out1) == 7)
  expect_true(is.character(out1$name))

  # Make sure that we can reset the model name behavior.
  test_name <- "test"
  out2 <- configure_model(params = params, state = init, carbon_pools_func = carbon_pools,
                          carbon_fluxes_func = carbon_fluxes, name = test_name)
  expect_equal(out2$name, test_name)

  # Checks to make sure that the appropriate errors are being thrown when the incorrect inputs are being read in.
  expect_error(configure_model(params = params, state = init, carbon_pools_func = carbon_fluxes,
                               carbon_fluxes_func = carbon_fluxes),
               regexp = "problem with carbon_pools_func format")
  expect_error(configure_model(params = params, state = init[1:3], carbon_pools_func = carbon_pools,
                               carbon_fluxes_func = carbon_fluxes, name = test_name),
               regexp = "object 'B' not found")
  expect_error(configure_model(params = params[1:5, ], state = init, carbon_pools_func = carbon_pools,
                               carbon_fluxes_func = carbon_fluxes, name = test_name),
               regexp = "object 'I.p' not found")

  # Check to make sure that when new parameter values are being used.
  x <- configure_model(params, init, carbon_pools, carbon_fluxes)
  new_table <- params
  new_table$value <- new_table$value * 2
  y <- configure_model(new_table, init, carbon_pools, carbon_fluxes)
  expect_true(all(y$params$value != x$params$value))
  expect_gt(abs(x$env$E.c - y$env$E.c), 0)
})

test_that("solve_model", {

  # Start by setting up a model and then solving, make sure that the structure of the object being returned
  # is correct. Note, we are not checking model solutions here that will be done in the old new test to make
  # sure that the numerical solutions are robust to coding changes.
  test_time <- 1:5
  mod1 <- configure_model(params = params,
                          state = init,
                          carbon_pools_func = carbon_pools,
                          carbon_fluxes_func = carbon_fluxes)
  out1 <- solve_model(mod = mod1, time = test_time, params = NULL, state = NULL)

  expect_equal(unique(out1$time), test_time)
  expect_equal(length(unique(out1$time)) * length(unique(out1$variable)), nrow(out1))
  expect_equal(unique(out1$name), "MEND")
  expect_named(out1, c("time", "variable", "value", "units", "name"))

  # Test to see that additional arguments works, aka passing in new parameter values
  # should change the numeric results.
  new_params <- MEMC::default_params
  new_params$value <- new_params$value * 2
  out2 <- solve_model(mod = mod1, time = test_time, params = new_params)
  expect_gt(mean(abs(out2$value - out1$value)), 0)

  # Check to make sure that additional arguments can be pased into the ode solver.
  # Using the different solver methods should have a trivial impact on the numerical output.
  lsode <- solve_model(mod = mod1, time = test_time, method = "lsode")
  ode45 <- solve_model(mod = mod1, time = test_time, method = "ode45")
  expect_lte(mean(abs(lsode$value - ode45$value)), 1e-5)
  expect_lte(mean(abs(lsode$value - ode45$value)), 1e-5)

  # Check that appropriate error are being thrown.
  expect_error(solve_model(mod = mod1, time = "f"))
  expect_error(solve_model(mod = mod1[1:2], time = test_time), regexp = "mod must a model object created by configure_model")
  expect_error(solve_model(mod = mod1, time = test_time, method = "fake"))
  expect_error(solve_model(mod = mod1, time = test_time, fake = "fake"))

})

test_that("update_params", {

  # make up a test data frame to work with, note that it needs to have the MEMC default parameter table
  # column names.
  int <- 1:3
  table <- data.frame("parameter" = letters[int], "description" = "testthat", units = "testthat", value = int)
  new_params <- table$value

  # Check to make sure that all of the values are updated.
  names(new_params) <- rev(table$parameter)
  out1 <- update_params(new_params = new_params, param_table = table)
  expect_true(any(table$value != out1$value))

  # Check to make sure that a single value can be updated at a time.
  new_params <- 10
  names(new_params) <- 'c'
  out2 <- update_params(new_params = new_params, param_table = table)
  expect_equal( sum(table$value != out2$value), 1)

  # Ensure errors are thrown when appropriate!
  new_params <- 10
  names(new_params) <- 'fake'
  expect_error(update_params(new_params = new_params, param_table = table), "new_params must refer to a parameter already existing in param_table")

  # Update an actual data table, make sure that only one value was updated.
  change        <- params[3, ]$value * 3
  names(change) <- params[3, ]$parameter
  new_output <- update_params(new_params = change, param_table = params)
  expect_equal(sum(new_output$value != params$value), 1)

})

test_that("solve_model", {

  mod1 <- configure_model(params = params, state = init, carbon_pools_func = carbon_pools,
                          carbon_fluxes_func = carbon_fluxes)
  t    <- seq(from = 1, to = 50, length.out = 5)
  out1 <- solve_model(mod1, time = t)

  # Change the half saturation constant for biomass uptake.
  change <- params[params["parameter"] == "K.d", ]$value * 10
  names(change) <- "K.d"
  new_table <- update_params(new_params = change, param_table = params)
  out2 <- solve_model(mod1, time = t, params = new_table)

  names(out1) <- c("time", "variable", "value1", "units", "name")
  names(out2) <- c("time", "variable", "value2", "units", "name")
  joined_df <- merge(out1, out2, all=FALSE)

  # Since all starting with the same initial states make sure that the values
  # being returned at time step 1 are all equal
  time1 <- joined_df[joined_df$time == t[1], ]
  expect_equal(time1$value1, time1$value2)

  # Overall the system is being held constant (there is no C being added to the system)
  # so we would expect the results for total carbon be be the same...
  total <- joined_df[joined_df$variable == "Tot",]
  expect_equal(total$value1, total$value2, tolerance = 1e-2)

  # Because of the parameter that we changed is related to biomass update check to make sure that
  # the biomass results are different from one another.
  biomass <- joined_df[(joined_df$time != 1 & joined_df$variable == "B"), ]
  expect_gte(mean(abs(biomass$value1 - biomass$value2)), 1e-2)

})

test_that("internal_load_params", {

  # the function works
  x  <- internal_load_params(ptable = params, state = init)
  expect_true(is.environment(x))
  expect_true(all(c(params$parameter, names(init)) %in% ls(x)))

  # check that errors are thrown
  expect_error(internal_load_params(ptable = params[, 1:3 ], state = init),
               "ptable does not have all of these name(s): 'parameter', 'description', 'units', 'value'",
               fixed = TRUE)
  expect_error(internal_load_params(ptable = params, state = "x"), "state is not a numeric or integer vector")
  expect_error(internal_load_params(ptable = params, state = 1), "state must be named")

})

test_that("modify_env", {

  # Make sure that environment is set up can be added.
  x <- new.env(parent = emptyenv())
  expect_true(length(ls(x)) == 0)

  new <- 2
  names(new) <- "test"
  modify_env(x, new)
  expect_true(length(ls(x)) == 1)
  expect_true(ls(x) == "test")

  # Make sure that can actually update the value.
  out <- internal_load_params(params, init)
  new <- 3
  names(new) <- "EP"
  old_EP <- out$EP
  modify_env(out, new)
  expect_gte(abs(out$EP - old_EP), 1e-3)

  # Make sure that there are errors thrown
  new <- "3"
  names(new) <- "test"
  expect_error(modify_env(out, new), "new is not a numeric or integer vector")

  new <- 2
  expect_error(modify_env(out, new), "names(new) is not a character vector", fixed = TRUE)

  new <- 3
  names(new) <- "test"
  xx <- 2
  expect_error(modify_env(xx, new), "env is not an environment")

})
