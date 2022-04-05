params <- MEMC::default_params
init   <- MEMC::default_initial
env <- internal_load_params(params, init)
mod <- MEMC::MEND_model
t <- 1:10

test_that("sm_internal_check_args", {

  expect_true(sm_internal_check_args(mod = mod, time = t, params = params, state = init))

  # Check that appropriate error are being thrown.
  expect_error(sm_internal_check_args(mod = mod, time = "f"))
  expect_error(sm_internal_check_args(mod = mod[1:2], time = test_time), regexp = "mod must a model object created by configure_model")
  expect_error(sm_internal_check_args(mod = mod, time = test_time, method = "fake"))
  expect_error(sm_internal_check_args(mod = mod, time = test_time, fake = "fake"))

})

test_that("sm_internal", {

  # Ensure that sm_internal function returns an object from desolve
  out1 <- sm_internal(mod = mod, time = t, params = params, state = init)
  expect_true(is.matrix(out1))
  expect_equal(dim(out1), c(length(t), length(init) + 1))
  expect_equal(class(out1), c("deSolve", "matrix"))

  # Make sure methods argument can be passed into desolve.
  out2 <- sm_internal(mod = mod, time = t, params = params, state = init, method = "euler")
  expect_true(all(abs(out1[, "B"] - out2[, "B"]) <= 1e-5))

})

test_that("sm_format_out", {

  # Check to the output returned, is it the structure we are expecting?
  desolve_out <- sm_internal(mod = mod, time = t, params = params, state = init)
  out <- sm_format_out(rslt = desolve_out, mod = mod)
  expect_true(is.data.frame(out))
  expect_true(all(names(out) %in% c("time", "variable", "value", "units", "name")))

})

test_that("solve_model", {

  # Check for expected output
  out1 <- solve_model(mod = mod, time = t, params = params, state = init)
  expect_true(is.data.frame(out1))
  expect_true(all(names(out1) %in% c("time", "variable", "value", "units", "name")))

  # Test to see that additional arguments works, aka passing in new parameter values
  # should change the numeric results.
  new_params <- MEMC::default_params
  new_params$value[1:20] <- new_params$value[1:20] * 2
  out2 <- solve_model(mod = mod, time = t, params = new_params)
  expect_gt(mean(abs(out2$value - out1$value)), 0)

})
