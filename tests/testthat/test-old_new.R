
context('old new test')

testthat::test_that("MEND2013", {

  t <- seq(0, 1e3, by = 0.1)

  out1 <- solver(params = MEND2013_params,
                time = t,
                state = MEND2013_initalState,
                carbon_pools_func = MEND2013_pools,
                carbon_fluxes_func = MEND2013_fluxes)

  old <- read.csv('compdata/old_new-MEND2013.csv')

  expect_equal(dim(old), dim(out1))
  expect_equal(old$value, out1$value)

  out2 <- MEND2013(parameters = MEND2013_params, time = t, inital_state = MEND2013_initalState)
  expect_equal(out2, out1)

})

