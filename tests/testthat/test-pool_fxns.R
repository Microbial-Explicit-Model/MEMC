context('carbon pool functions')

# Define the time steps to solve the model at.
t <- seq(0, 100, by = 20)


# MEND pools -------------------------------------------------
# All of the MEND family of functions use the same MEND_pools function.
test_that("MEND_pools", {

  # Make sure that the function throws errors when the incorrect inputs are used.
  expect_error(MEND_pools(t = t, state = MEND_initalState, parms = MEND_params, flux_function =  'not a list'),
               "flux_function is not a function", fixed = TRUE)
  expect_error(MEND_pools(t = t, state = rev(MEND_initalState), parms = MEND_params, flux_function =  'not a list'),
               "state pools must be in the following order: P,  M,  Q,  B,  D,  EP,  EM,  IC,  Tot", fixed = TRUE)

  # Length of the output should equal the number of states (pools of carbon) and is some number.
  xx <- MEND_pools(state = MEND_initalState, parms = MEND_params)
  expect_true(is.numeric(sum(unlist(xx))))
  expect_equal(length(xx[[1]]), length(MEND_initalState))

})


# MEND2 pools -------------------------------------------------
test_that("MEND2_pools", {

  # Make sure that the function throws errors when the incorrect inputs are used.
  expect_error(MEND2_pools(t = t, state = MEND2_initalState, parms = MEND_params, flux_function =  'not a list'),
               "flux_function is not a function", fixed = TRUE)
  expect_error(MEND2_pools(t = t, state = rev(MEND2_initalState), parms = MEND_params, flux_function =  'not a list'),
               "state pools must be in the following order: P1,  P2,  M,  B,  D1,  D2,  Q1,  Q2,  EP1,  EP2,  EM,  IC,  Tot", fixed = TRUE)

  # Length of the output should equal the number of states (pools of carbon) and is some number.
  xx <- MEND2_pools(state = MEND2_initalState, parms = MEND2_params)
  expect_true(is.numeric(sum(unlist(xx))))
  expect_equal(length(xx[[1]]), length(MEND2_initalState))

})
