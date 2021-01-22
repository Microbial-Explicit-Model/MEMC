context('MEND2013')

# Define the inital state values
B = 2; D = 1
P = 10; Q = 0.1
M = 5; EP = 0.00001
EM =  0.00001; IC = 0
Tot = 18.10002

state <- c(P = P,  M = M,  Q = Q,  B = B,  D = D,  EP = EP,  EM = EM,  IC = IC,  Tot = Tot)


# Test the functions that make up the MEND 2013 system of equations.
test_that("MEND2013_RM_fluxes", {

  # Make sure that an error is thrown when the incorrect inputs are  used.
  expect_error(MEND2013_RM_fluxes(state = state[1:2], parms = MEND2013_params),
               "state does not have all of these name(s): 'B', 'D', 'P', 'Q', 'M', 'EP', 'EM'", fixed = TRUE)
  expect_error(MEND2013_RM_fluxes(state = state, parms = MEND2013_params[ ,1:2]),
               "parms does not have all of these name(s): 'parameter', 'description', 'units', 'value'", fixed = TRUE)
  expect_error(MEND2013_RM_fluxes(state = state, parms = MEND2013_params[1:2,]),
               "parms missing values for:  E.c, V.d, m.r, K.d, V.m, K.m, K.ads, Q.max, K.des, p.ep, p.em, r.ep, r.em", fixed = TRUE)

  #  MEND2013_fluxes should return a functions that can return numeric values.
  x <- MEND2013_RM_fluxes(state = state, parms = MEND2013_params)
  expect_true(all(unlist(lapply(x, is.function))))

  xx <- x$F1()
  expect_true(is.numeric(xx))

})


test_that("MEND2013_pools", {

  # Make sure that the function throws errors when the incorrect inputs are used.
  state <- c(P = P,  M = M,  Q = Q,  B = B,  D = D,  EP = EP,  EM = EM,  IC = IC,  Tot = Tot)
  expect_error(MEND2013_pools(t = t, state = state, parms = MEND2013_params, flux_function =  'not a list'),
               "flux_function is not a function", fixed = TRUE)
  expect_error(MEND2013_pools(t = t, state = state[1:2], parms = MEND2013_params, flux_function = function(){}),
               'missing states: Q,  B,  D,  EP,  EM,  IC,  Tot')
  expect_error(MEND2013_pools(t = t, state = state, parms = MEND2013_params[ , 1:2], flux_function =  function(){}),
               "parms does not have all of these name(s): 'parameter', 'description', 'units', 'value'", fixed = TRUE)
  expect_error(MEND2013_pools(t = t, state = state, parms = MEND2013_params[1:2, ], flux_function =  function(){}),
               "parms missing values for:  I.p, g.d, f.d, I.d", fixed = TRUE)
  expect_error(MEND2013_pools(t = t, state = rev(state), parms = MEND2013_params, flux_function =  MEND2013_fluxes),
               'state pools must be in the following order: P,  M,  Q,  B,  D,  EP,  EM,  IC,  Tot')


  xx <- MEND2013_pools(state = state, parms = MEND2013_params)
  # Length of the output should equal the number of states (pools of carbon) and is some number.
  expect_true(is.numeric(sum(unlist(xx))))
  expect_equal(length(xx[[1]]), length(state))

  # Make sure the function works when the flux is defined.
  yy <- MEND2013_pools(state = state, parms = MEND2013_params, flux_function = MEND2013_fluxes)
  # Length of the output should equal the number of states (pools of carbon) and is some number.
  expect_true(is.numeric(sum(unlist(yy))))
  expect_equal(length(yy[[1]]), length(state))

})


test_that("deafult MEND values", {

  # Make sure that the default MEND values exist and that the total carbon equals the sum of the other pools.
  expect_true(is.vector(MEND2013_initalState))
  expect_equal(sum(MEND2013_initalState[names(MEND2013_initalState) != 'Tot']), MEND2013_initalState[['Tot']])

  expect_true(data.table::is.data.table(MEND2013_params))

})
