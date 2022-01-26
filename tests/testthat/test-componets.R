params <- MEMC::default_params
init   <- MEMC::default_initial
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

  bad_env  <- internal_load_params(ptable = params, state = init[1])
  x <- carbon_fluxes_internal(env = bad_env)


})

test_that("POMdecomp kinetics", {

  test_MM  <- carbon_fluxes(POMdecomp = "MM", env = env)$flux_function(env = env)
  test_RMM <- carbon_fluxes(POMdecomp = "RMM", env = env)$flux_function(env = env)
  test_ECA <- carbon_fluxes(POMdecomp = "ECA", env = env)$flux_function(env = env)
  test_LM  <- carbon_fluxes(POMdecomp = "LM", env = env)$flux_function(env = env)

  # We expect all of the values to be returned to be different from one another.
  # The DOM output should all be the same but the POM output should all be unique.
  dom_out <- c(test_MM$F1(), test_RMM$F1(), test_ECA$F1(), test_LM$F1())
  expect_equal(length(unique(dom_out)), 1)
  pom_out <- c(test_MM$F2(), test_RMM$F2(), test_ECA$F2(), test_LM$F2())
  expect_equal(length(unique(pom_out)), length(pom_out))

})

test_that("DOMdecomp kinetics", {

  test_MM  <- carbon_fluxes(DOMdecomp  = "MM", env = env)$flux_function(env = env)
  test_RMM <- carbon_fluxes(DOMdecomp = "RMM", env = env)$flux_function(env = env)
  test_ECA <- carbon_fluxes(DOMdecomp = "ECA", env = env)$flux_function(env = env)
  test_LM  <- carbon_fluxes(DOMdecomp = "LM", env = env)$flux_function(env = env)

  # We expect all of the values to be returned to be different from one another.
  dom_out <- c(test_MM$F1(), test_RMM$F1(), test_ECA$F1(), test_LM$F1())
  expect_equal(length(dom_out), length(unique(dom_out)))


})

test_that("MBdecay kinetics", {

  expect_error(carbon_fluxes(MBdecay = "fake", env = env)$flux_function(env = env), 'MBdecay must be "MM", "RMM", "ECA", "LM"', fixed = TRUE)

})

test_that("carbon_pools", {

  out1 <- unlist(carbon_pools(t = 1, env = env))
  expect_true(all(is.numeric(out1)))

  # change parameters, make sure that the changing a parameter involved in B update
  # causes the B value & downstream fluxes to change appropriately.
  new_table <- params
  pname <- 'V.d'
  index <- which(new_table$parameter == pname)
  new_table[index, ]$value <- new_table[index, ]$value * 10
  out2 <- unlist(carbon_pools(t = 1, env = env, params = new_table))
  expect_true(any(abs(out2 - out1) > 0))

  # change the state values
  new_init <- init * 2
  out3 <- unlist(carbon_pools(t = 1, env = env, state = new_init))
  expect_true(any(out1 != out3))

  # Make sure that error messages are thrown!
  expect_error(object = carbon_pools(t = "l", env, flux_function = carbon_fluxes(POMdecomp = "MM")),
               regexp = "t is not a numeric or integer vector", fixed = TRUE)

  bad_env <- internal_load_params(ptable = params, state = init[1:2])
  expect_error(object = carbon_pools(t = 10, env =  bad_env), regexp = "object 'B' not found", fixed = TRUE)

  bad_env2 <- internal_load_params(ptable = params[1:3,], state =  init)
  expect_error(object = carbon_pools(t = 1, env =  bad_env2, flux_function = carbon_fluxes(POMdecomp = "MM")),
               regexp = "object 'I.p' not found", fixed = TRUE)
})
