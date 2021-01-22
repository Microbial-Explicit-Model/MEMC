context('MEND2013_RM')

state <- MEND2013_initalState

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


test_that("MEND2013_RM", {

  out <- MEND2013_RM(MEND2013_params, t = 1:10, MEND2013_initalState)
  expect_true(all(is.numeric(out$value)))

})
