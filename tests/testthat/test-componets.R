
params <- MEMC::default_params
init   <- MEMC::default_inital
env    <- internal_load_params(ptable = params, state = init)

test_that("carbon_fluxes_internal", {

  # Make sure that all of the elements in returned by the carbon fluxes are numeric elements
  out <- carbon_fluxes_internal(env = env, state = init, params = params)
  expect_true(is.list(out))
  for (x in out){
    xx <- x()
    expect_true(is.numeric(xx))
  }

  # Make sure that when we modify the inputs that carbon fluxes change,
  # note that the env is not reset
  init2 <- init*2
  out2  <- carbon_fluxes_internal(env = env, state = init2)
  expect_gte(abs(out$F3() - out2$F3()), 0e-8)

  p2 <- params
  p2$value <- p2$value * 2
  out3 <- carbon_fluxes_internal(env = env, params = p2)
  expect_gte(abs(out3$F1() -  out2$F1()), 0e-8)

})

test_that("carbon_pools", {

  out1 <- unlist(carbon_pools(t = 1, env = env, flux_function = carbon_fluxes(POMdecomp = "MM")))
  expect_true(all(is.numeric(out1)))

  # change parameters, make sure that the changing a parameter involved in B update
  # causes the B value & downstream fluxes to change appropriately.
  new_table <- params
  pname <- 'V.d'
  index <- which(new_table$parameter == pname)
  new_table[index, ]$value <- new_table[index, ]$value * 10

  changes_expected_in <- c("B", "D", "IC")
  index <- which(names(index) %in% changes_expected_in)
  out2 <- unlist(carbon_pools(t = 10, env = env, params = new_table, flux_function = carbon_fluxes(POMdecomp = "MM")))
  expect_true(all(abs(out1[index] - out2[index]) >= 1e-4))


  out3 <- unlist(carbon_pools(t = 10, env = env, params = new_table, flux_function = carbon_fluxes(POMdecomp = "RMM")))
  expect_true(mean(abs(out1 - out2)) >= 1e-4)


  # change the state values
  new_init <- init * 2
  out3 <- unlist(carbon_pools(t = 1, env = env, state = new_init))
  expect_true(any(out1 != out3))

  # Make sure that error messages are thrown!
  expect_error(object = carbon_pools(t = "l", env, flux_function = carbon_fluxes(POMdecomp = "MM")),
               regexp = "t is not a numeric or integer vector", fixed = TRUE)

  bad_env <- internal_load_params(ptable = params, state = init[1:2])
  expect_error(object = carbon_pools(t = 1, env =  bad_env, flux_function = carbon_fluxes(POMdecomp = "MM")),
               regexp = "object 'B' not found", fixed = TRUE)

  bad_env2 <- internal_load_params(ptable = params[1:3,], state =  init)
  expect_error(object = carbon_pools(t = 1, env =  bad_env2, flux_function = carbon_fluxes(POMdecomp = "MM")),
               regexp = "object 'I.p' not found", fixed = TRUE)
})



