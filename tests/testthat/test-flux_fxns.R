context('carbon flux functions')

# Define the time steps to solve the model at.
t <- seq(0, 100, by = 20)

# MEND fluxes -------------------------------------------------
test_that("MEND_fluxes", {

  # Make sure that an error is thrown when the incorrect inputs are  used.
  expect_error(MEND_fluxes(state = MEND2_initalState, parms = MEND_params),
               "state does not have all of these name(s): 'B', 'D', 'P', 'Q', 'M', 'EP', 'EM'", fixed = TRUE)

  expect_error(MEND_fluxes(state = MEND_initalState, parms = MEND2_params),
               "parms missing values for:  E.c, V.d, m.r, K.d, V.p, K.p, Q.max", fixed = TRUE)

  #  MEND_fluxes should return a functions that can return numeric values.
  x <- MEND_fluxes(state = MEND_initalState, parms = MEND_params)
  expect_true(all(unlist(lapply(x, is.function))))

  # Make sure that every flux function returns a number!
  is_numeric <- c()
  for(i in seq_along(x)){
    is_numeric <- c(is_numeric,  is.numeric(x[[i]]()))
  }
  expect_true(all(is_numeric))

})

test_that("MEND_RM_fluxes", {

  # Make sure that an error is thrown when the incorrect inputs are  used.
  expect_error(MEND_RM_fluxes(state = MEND2_initalState, parms = MEND_params),
               "state does not have all of these name(s): 'B', 'D', 'P', 'Q', 'M', 'EP', 'EM'", fixed = TRUE)

  expect_error(MEND_RM_fluxes(state = MEND_initalState, parms = MEND2_params),
               "parms missing values for:  E.c, V.d, m.r, K.d, V.p, K.p, Q.max", fixed = TRUE)

  #  MEND_fluxes should return a functions that can return numeric values.
  x <- MEND_RM_fluxes(state = MEND_initalState, parms = MEND_params)
  expect_true(all(unlist(lapply(x, is.function))))

  # Make sure that every flux function returns a number!
  is_numeric <- c()
  for(i in seq_along(x)){
    is_numeric <- c(is_numeric,  is.numeric(x[[i]]()))
  }
  expect_true(all(is_numeric))

})

test_that("MEND_ECA_fluxes", {

  # Make sure that an error is thrown when the incorrect inputs are  used.
  expect_error(MEND_ECA_fluxes(state = MEND2_initalState, parms = MEND_params),
               "state does not have all of these name(s): 'B', 'D', 'P', 'Q', 'M', 'EP', 'EM'", fixed = TRUE)

  expect_error(MEND_ECA_fluxes(state = MEND_initalState, parms = MEND2_params),
               "parms missing values for:  E.c, V.d, m.r, K.d, V.p, K.p, Q.max", fixed = TRUE)

  #  MEND_fluxes should return a functions that can return numeric values.
  x <- MEND_ECA_fluxes(state = MEND_initalState, parms = MEND_params)
  expect_true(all(unlist(lapply(x, is.function))))

  # Make sure that every flux function returns a number!
  is_numeric <- c()
  for(i in seq_along(x)){
    is_numeric <- c(is_numeric,  is.numeric(x[[i]]()))
  }
  expect_true(all(is_numeric))

})

test_that("MEND_MILLEN_fluxes", {

  # Make sure that an error is thrown when the incorrect inputs are  used.
  expect_error(MEND_MILLEN_fluxes(state = MEND2_initalState, parms = MEND_params),
               "state does not have all of these name(s): 'B', 'D', 'P', 'Q', 'M', 'EP', 'EM'", fixed = TRUE)

  expect_error(MEND_MILLEN_fluxes(state = MEND_initalState, parms = MEND2_params),
               "parms missing values for:  E.c, V.d, m.r, K.d, V.p, K.p, Q.max", fixed = TRUE)

  #  MEND_fluxes should return a functions that can return numeric values.
  x <- MEND_MILLEN_fluxes(state = MEND_initalState, parms = MEND_params)
  expect_true(all(unlist(lapply(x, is.function))))

  # Make sure that every flux function returns a number!
  is_numeric <- c()
  for(i in seq_along(x)){
    is_numeric <- c(is_numeric,  is.numeric(x[[i]]()))
  }
  expect_true(all(is_numeric))

})


# 2 pool MEND fluxes -------------------------------------------------
test_that("MEND2_fluxes", {

  # Make sure that an error is thrown when the incorrect inputs are  used.
  expect_error(MEND2_fluxes(state = MEND_initalState, parms = MEND_params),
               "state does not have all of these name(s): 'P1', 'P2', 'M', 'B', 'D1', 'D2', 'Q1', 'Q2', 'EP1', 'EP2', 'EM', 'IC', 'Tot'", fixed = TRUE)

  expect_error(MEND2_fluxes(state = MEND2_initalState, parms = MEND_params),
               "parms missing values for:  E.c1, E.c2, V.d1, V.d2, K.d1, K.d2, V.p1, V.p2, K.p1, K.p2, Q.max1, Q.max2", fixed = TRUE)

  #  MEND_fluxes should return a functions that can return numeric values.
  x <- MEND2_fluxes(state = MEND2_initalState, parms = MEND2_params)
  expect_true(all(unlist(lapply(x, is.function))))

  xx <- x$F1d1()
  expect_true(is.numeric(xx))

  # Make sure that every flux function returns a number!
  is_numeric <- c()
  for(i in seq_along(x)){
    is_numeric <- c(is_numeric,  is.numeric(x[[i]]()))
  }
  expect_true(all(is_numeric))

})

test_that("MEND2_RM_fluxes", {

  # Make sure that an error is thrown when the incorrect inputs are  used.
  expect_error(MEND2_RM_fluxes(state = MEND_initalState, parms = MEND_params),
               "state does not have all of these name(s): 'P1', 'P2', 'M', 'B', 'D1', 'D2', 'Q1', 'Q2', 'EP1', 'EP2', 'EM', 'IC', 'Tot'", fixed = TRUE)

  expect_error(MEND2_RM_fluxes(state = MEND2_initalState, parms = MEND_params),
               "parms missing values for:  E.c1, E.c2, V.d1, V.d2, K.d1, K.d2, V.p1, V.p2, K.p1, K.p2, Q.max1, Q.max2", fixed = TRUE)

  #  MEND_fluxes should return a functions that can return numeric values.
  x <- MEND2_RM_fluxes(state = MEND2_initalState, parms = MEND2_params)
  expect_true(all(unlist(lapply(x, is.function))))

  xx <- x$F1d1()
  expect_true(is.numeric(xx))

  # Make sure that every flux function returns a number!
  is_numeric <- c()
  for(i in seq_along(x)){
    is_numeric <- c(is_numeric,  is.numeric(x[[i]]()))
  }
  expect_true(all(is_numeric))

})

test_that("MEND2_ECA_fluxes", {

  # Make sure that an error is thrown when the incorrect inputs are  used.
  expect_error(MEND2_ECA_fluxes(state = MEND_initalState, parms = MEND_params),
               "state does not have all of these name(s): 'P1', 'P2', 'M', 'B', 'D1', 'D2', 'Q1', 'Q2', 'EP1', 'EP2', 'EM', 'IC', 'Tot'", fixed = TRUE)

  expect_error(MEND2_ECA_fluxes(state = MEND2_initalState, parms = MEND_params),
               "parms missing values for:  E.c1, E.c2, V.d1, V.d2, K.d1, K.d2, V.p1, V.p2, K.p1, K.p2, Q.max1, Q.max2", fixed = TRUE)

  #  MEND_fluxes should return a functions that can return numeric values.
  x <- MEND2_ECA_fluxes(state = MEND2_initalState, parms = MEND2_params)
  expect_true(all(unlist(lapply(x, is.function))))

  xx <- x$F1d1()
  expect_true(is.numeric(xx))

  # Make sure that every flux function returns a number!
  is_numeric <- c()
  for(i in seq_along(x)){
    is_numeric <- c(is_numeric,  is.numeric(x[[i]]()))
  }
  expect_true(all(is_numeric))

})

test_that("MEND2_MILLEN_fluxes", {

  # Make sure that an error is thrown when the incorrect inputs are  used.
  expect_error(MEND2_MILLEN_fluxes(state = MEND_initalState, parms = MEND_params),
               "state does not have all of these name(s): 'P1', 'P2', 'M', 'B', 'D1', 'D2', 'Q1', 'Q2', 'EP1', 'EP2', 'EM', 'IC', 'Tot'", fixed = TRUE)

  expect_error(MEND2_MILLEN_fluxes(state = MEND2_initalState, parms = MEND_params),
               "parms missing values for:  E.c1, E.c2, V.d1, V.d2, K.d1, K.d2, V.p1, V.p2, K.p1, K.p2, Q.max1, Q.max2", fixed = TRUE)

  #  MEND_fluxes should return a functions that can return numeric values.
  x <- MEND2_MILLEN_fluxes(state = MEND2_initalState, parms = MEND2_params)
  expect_true(all(unlist(lapply(x, is.function))))

  xx <- x$F1d1()
  expect_true(is.numeric(xx))

  # Make sure that every flux function returns a number!
  is_numeric <- c()
  for(i in seq_along(x)){
    is_numeric <- c(is_numeric,  is.numeric(x[[i]]()))
  }
  expect_true(all(is_numeric))

})

