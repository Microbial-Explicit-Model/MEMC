# Test the functions that check to make sure the inputs to functions are correct!


test_that("is_param_table works", {

  params <- MEMC::default_params

  # The default parameter table should always pass this test.
  expect_true(is_param_table(params))

  # A number of errors should be thrown if the the parameter table does not meet the correct conditions.
  expect_error(is_param_table(params[1:10,]), "param_table is missing a parameter value(s) for: p.ep, p.em, r.ep, r.em, Q.max, K.ads, K.des, dd.beta, Input.P, Input.D, Input.M, CUE", fixed = TRUE)

  ptable_fake_entry <- rbind(params, data.frame(parameter = "fake", description = "fake", units = "fake", value = "fake"))
  expect_error(is_param_table(ptable_fake_entry), "param_table contains unkown parameter value(s): fake", fixed = TRUE)

  expect_error(is_param_table(params[ ,1:2]), "param_table is missing a required column: parameter, description, units, value", fixed = TRUE)
  expect_error(is_param_table(params[ ,3:4]), "param_table is missing a required column: parameter, description, units, value", fixed = TRUE)

})


test_that("is_state_vector works", {

  state <- MEMC::default_initial

  # The default initial state vector should always pass this test.
  assert_that(is_state_vector(state))

  # A number of errors should be thrown if the input does not meet the correct requirements.
  expect_error(is_state_vector(rev(state)), "entires must be in the correct order: P, M, Q, B, D, EP, EM, IC, Tot", fixed = TRUE)

  names(state) <- NULL
  expect_error(is_state_vector(state), "names(state) is not a character vector", fixed = TRUE)

  state <- as.character(state)
  names(state) <- names(MEMC::default_initial)
  expect_error(is_state_vector(state), "state is not a numeric or integer vector")

})
