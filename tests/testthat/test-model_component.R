# this really needs to be expanded upon! but for now making sure that
# running under different conditions has an affect on the output
ptable <- MEMC::MEND_model$params
state <- MEMC::MEND_model$state
mod <- MEMC::MEND_model
time <- seq(0, 1000, length.out = 10)

zero <- 1e-5

# These first set of tests make sure that all the internal functions are running
# and not throwing errors.
test_that("carbon_pool_derivs", {

  p <- ptable$value
  names(p) <- ptable$parameter
  x <- carbon_pool_derivs(t = 1,
                          state = mod[["state"]],
                          p = p,
                          DOMuptake = mod$table$DOMuptake,
                          POMdecomp = mod$table$POMdecomp,
                          MBdecay = mod$table$MBdecay)
  expect_true(is.list(x))
  expect_true(is.numeric(unlist(x)))

})

test_that("memc_solve", {
  out <- memc_solve(mod, time = 1:2)
  expect_true(is.data.frame(out))

})

test_that("carbon_fluxes_internal", {

  p <- ptable$value
  names(p) <- ptable$parameter
  x <- carbon_fluxes_internal(p = p)
  expect_true(is.list(x))
  expect_true(all(sapply(x, is.function)))

})

test_that("memc_configure", {
  config <- memc_configure(params = ptable,
                            state = state)
  expect_true(is.list(config))
  expect_length(config, 4)

})



# With this second set of tests are slightly more sophisticated and focus on
# testing function behavior.

test_that("change param", {

  out1 <- memc_solve(mod = mod, time)
  out2 <- memc_solve(mod = mod, time, params = c("K_m" = 10))
  expect_gte(mean((out1$value - out2$value)^2), zero)

})

test_that("change starting state", {

  out1 <- memc_solve(mod = mod, time)
  out2 <- memc_solve(mod = mod, time, state = c(MB = 4))
  expect_gte(mean((out1$value - out2$value)^2), zero)

})

test_that("changing dynamics should change results", {

  config <- memc_configure(params = ptable, state = state)
  default <- memc_solve(mod = config, time)

  # Change DOM decomposition dynamics
  config <- memc_configure(params = ptable,
                            state = state,
                            DOMuptake = "RMM")
  out1 <- memc_solve(mod = config, time)
  expect_gte(mean((default$value - out1$value)^2), zero)

  config <- memc_configure(params = ptable,
                            state = state,
                            DOMuptake = "ECA")
  out2 <- memc_solve(mod = config, time)
  expect_gte(mean((default$value - out2$value)^2), zero)


  # Change DOM decomposition dynamics
  config <- memc_configure(params = ptable,
                            state = state,
                            POMdecomp = "RMM")
  out3 <- memc_solve(mod = config, time)
  expect_gte(mean((default$value - out3$value)^2), zero)

  config <- memc_configure(params = ptable,
                            state = state,
                            POMdecomp = "LM")
  out4 <- memc_solve(mod = config, time)
  expect_gte(mean((default$value - out4$value)^2), zero)

  config <- memc_configure(params = ptable,
                    state = state,
                    POMdecomp = "ECA")
  out5 <- memc_solve(mod = config, time)
  expect_gte(mean((default$value - out2$value)^2), zero)


  # Change microbial biomass decay dynamics
  config <- memc_configure(params = ptable,
                    state = state,
                    MBdecay = "DD")
  expect_error(memc_solve(mod = config, time), label = 'p[["dd_beta"]] not greater than 1')

  out6 <- memc_solve(mod = config,
                params = c("dd_beta" = 2),
                time = time)
  expect_gte(mean((default$value - out6$value)^2), zero)

})
