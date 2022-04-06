params <- MEMC::default_params
init   <- MEMC::default_initial
env <- internal_load_params(params, init)
mod <- MEMC::MEND_model
t <- seq(from = 1, to = 100, length.out = 10)

obs <- data.frame(time = t,
           IC = seq(from = 0.12, to = 2.76, length.out = length(t)))


test_that("make_objective_function", {

  out <- make_objective_function(obs = obs, config = mod)
  expect_true(is.function(out))

  # Check that a different observation can be used.
  names(obs) <- c("time", "B")
  out <- make_objective_function(obs = obs, config = mod)

  # Check that when different parameter values are used the mod cost & residuals are
  # different from one another.
  p <- 5.0e-04
  names(p) <- 'V.d'
  rslt1 <- out(p)

  p <- p/10
  names(p) <- 'V.d'
  rslt2 <- out(p)

  expect_gt(mean((rslt1$residuals$res -  rslt2$residuals$res)^2), 0)
  expect_true(rslt1$model != rslt2$model)

  # Check that bad data cannot be read in.
  names(obs) <- c("fake", "B")
  expect_error(make_objective_function(obs = obs, config = mod),
               'Elements 1 of names(obs) %in% c("time", names(MEMC::default_initial)) are not true', fixed = TRUE)

})

test_that("MEMC_modFit", {

  p <- 100
  names(p) <- 'V.d'
  out <- MEMC_modFit(obs, mod, p, method = "BFGS", lower = c(0))


})
